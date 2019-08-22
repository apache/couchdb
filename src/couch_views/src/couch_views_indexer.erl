% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(couch_views_indexer).

-export([
    spawn_link/0
]).


-export([
    init/0
]).

-include("couch_views.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").
-include_lib("fabric/include/fabric2.hrl").

% TODO:
%  * Handle timeouts of transaction and other errors


spawn_link() ->
    proc_lib:spawn_link(?MODULE, init, []).


init() ->
    {ok, Job, Data} = couch_jobs:accept(?INDEX_JOB_TYPE, #{}),
    #{
        <<"db_name">> := DbName,
        <<"ddoc_id">> := DDocId,
        <<"sig">> := JobSig
    } = Data,

    {ok, Db} = fabric2_db:open(DbName, [?ADMIN_CTX]),
    {ok, DDoc} = fabric2_db:open_doc(Db, DDocId),
    {ok, Mrst} = couch_views_util:ddoc_to_mrst(DbName, DDoc),
    HexSig = fabric2_util:to_hex(Mrst#mrst.sig),

    if  HexSig == JobSig -> ok; true ->
        couch_jobs:finish(undefined, Job, Data#{
            error => sig_changed,
            reason => <<"Design document was modified">>
        }),
        exit(normal)
    end,

    State = #{
        tx_db => undefined,
        db_seq => undefined,
        view_seq => undefined,
        last_seq => undefined,
        job => Job,
        job_data => Data,
        count => 0,
        limit => num_changes(),
        doc_acc => [],
        design_opts => Mrst#mrst.design_opts
    },

    update(Db, Mrst, State).


update(#{} = Db, Mrst0, State0) ->
    {Mrst2, State4} = fabric2_fdb:transactional(Db, fun(TxDb) ->
        % In the first iteration of update we need
        % to populate our db and view sequences
        State1 = case State0 of
            #{db_seq := undefined} ->
                ViewSeq = couch_views_fdb:get_update_seq(TxDb, Mrst0),
                State0#{
                    tx_db := TxDb,
                    db_seq := fabric2_db:get_update_seq(TxDb),
                    view_seq := ViewSeq,
                    last_seq := ViewSeq
                };
            _ ->
                State0#{
                    tx_db := TxDb
                }
        end,

        {ok, State2} = fold_changes(State1),

        #{
            count := Count,
            limit := Limit,
            doc_acc := DocAcc,
            last_seq := LastSeq
        } = State2,

        {Mrst1, MappedDocs} = map_docs(Mrst0, DocAcc),
        write_docs(TxDb, Mrst1, MappedDocs, State2),

        case Count < Limit of
            true ->
                report_progress(State2, finished),
                {Mrst1, finished};
            false ->
                State3 = report_progress(State2, update),
                {Mrst1, State3#{
                    tx_db := undefined,
                    count := 0,
                    doc_acc := [],
                    view_seq := LastSeq
                }}
        end
    end),

    case State4 of
        finished ->
            couch_query_servers:stop_doc_map(Mrst2#mrst.qserver);
        _ ->
            update(Db, Mrst2, State4)
    end.


fold_changes(State) ->
    #{
        view_seq := SinceSeq,
        limit := Limit,
        tx_db := TxDb
    } = State,

    Fun = fun process_changes/2,
    fabric2_db:fold_changes(TxDb, SinceSeq, Fun, State, [{limit, Limit}]).


process_changes(Change, Acc) ->
    #{
        doc_acc := DocAcc,
        count := Count,
        tx_db := TxDb,
        design_opts := DesignOpts
    } = Acc,

    #{
        id := Id,
        sequence := LastSeq,
        deleted := Deleted
    } = Change,

    IncludeDesign = lists:keymember(<<"include_design">>, 1, DesignOpts),

    Acc1 = case {Id, IncludeDesign} of
        {<<?DESIGN_DOC_PREFIX, _/binary>>, false} ->
            maps:merge(Acc, #{
                count => Count + 1,
                last_seq => LastSeq
            });
        _ ->
            % Making a note here that we should make fetching all the docs
            % a parallel fdb operation
            {ok, Doc} = case Deleted of
                true -> {ok, []};
                false -> fabric2_db:open_doc(TxDb, Id)
            end,

            Change1 = maps:put(doc, Doc, Change),
            Acc#{
                doc_acc := DocAcc ++ [Change1],
                count := Count + 1,
                last_seq := LastSeq
            }
    end,
    {ok, Acc1}.


map_docs(Mrst, Docs) ->
    % Run all the non deleted docs through the view engine and
    Mrst1 = start_query_server(Mrst),
    QServer = Mrst1#mrst.qserver,
    MapFun = fun
        (#{deleted := true} = Change) ->
            Change#{results => []};
        (#{deleted := false} = Change) ->
            #{doc := Doc} = Change,
            couch_stats:increment_counter([couchdb, mrview, map_doc]),
            {ok, RawResults} = couch_query_servers:map_doc_raw(QServer, Doc),
            JsonResults = couch_query_servers:raw_to_ejson(RawResults),
            ListResults = lists:map(fun(ViewResults) ->
                [list_to_tuple(Res) || Res <- ViewResults]
            end, JsonResults),
            Change#{results => ListResults}
    end,
    {Mrst1, lists:map(MapFun, Docs)}.


write_docs(TxDb, Mrst, Docs, State) ->
    #mrst{
        views = Views,
        sig = Sig
    } = Mrst,

    #{
        last_seq := LastSeq
    } = State,

    ViewIds = [View#mrview.id_num || View <- Views],

    lists:foreach(fun(Doc) ->
        couch_views_fdb:write_doc(TxDb, Sig, ViewIds, Doc)
    end, Docs),

    couch_views_fdb:set_update_seq(TxDb, Sig, LastSeq).


start_query_server(#mrst{qserver = nil} = Mrst) ->
    #mrst{
        language = Language,
        lib = Lib,
        views = Views
    } = Mrst,
    Defs = [View#mrview.def || View <- Views],
    {ok, QServer} = couch_query_servers:start_doc_map(Language, Defs, Lib),
    Mrst#mrst{qserver = QServer};

start_query_server(#mrst{} = Mrst) ->
    Mrst.


report_progress(State, UpdateType) ->
    #{
        tx_db := TxDb,
        job := Job1,
        job_data := JobData,
        last_seq := LastSeq
    } = State,

    #{
        <<"db_name">> := DbName,
        <<"ddoc_id">> := DDocId,
        <<"sig">> := Sig
    } = JobData,

    % Reconstruct from scratch to remove any
    % possible existing error state.
    NewData = #{
        <<"db_name">> => DbName,
        <<"ddoc_id">> => DDocId,
        <<"sig">> => Sig,
        <<"view_seq">> => LastSeq
    },

    case UpdateType of
        update ->
            case couch_jobs:update(TxDb, Job1, NewData) of
                {ok, Job2} ->
                    State#{job := Job2};
                {error, halt} ->
                    couch_log:error("~s job halted :: ~w", [?MODULE, Job1]),
                    exit(normal)
            end;
        finished ->
            case couch_jobs:finish(TxDb, Job1, NewData) of
                ok ->
                    State;
                {error, halt} ->
                    couch_log:error("~s job halted :: ~w", [?MODULE, Job1]),
                    exit(normal)
            end
    end.


num_changes() ->
    config:get_integer("couch_views", "change_limit", 100).
