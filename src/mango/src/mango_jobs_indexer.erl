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


% Todo: there is a fair amount of copy-pasta from couch_views_indexer
% We need to make the indexing generic and have only the specific mango
% logic here
-module(mango_jobs_indexer).


-export([
    spawn_link/0
]).


-export([
    set_timeout/0,
    init/0
]).


-include("mango.hrl").
-include("mango_idx.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("fabric/include/fabric2.hrl").


spawn_link() ->
    proc_lib:spawn_link(?MODULE, init, []).


set_timeout() ->
    mango_jobs:set_timeout().


init() ->
    {ok, Job, Data} = couch_jobs:accept(?MANGO_INDEX_JOB_TYPE, #{}),
    #{
        <<"db_name">> := DbName,
        <<"ddoc_id">> := DDocId,
        <<"columns">> := JobColumns,
        <<"retries">> := Retries
    } = Data,

    {ok, Db} = try
        fabric2_db:open(DbName, [?ADMIN_CTX])
    catch error:database_does_not_exist ->
        couch_jobs:finish(undefined, Job, Data#{
            error => db_deleted,
            reason => "Database was deleted"
        }),
        exit(normal)
    end,

    [Idx] = case fabric2_db:open_doc(Db, DDocId) of
        {ok, DDoc} ->
            JsonDDoc = couch_doc:to_json_obj(DDoc, []),
            mango_idx:from_ddoc(Db, JsonDDoc);
        {not_found, _} ->
            couch_jobs:finish(undefined, Job, Data#{
                error => ddoc_deleted,
                reason => "Mango Index was deleted"
            }),
            exit(normal)
    end,

    Columns = mango_idx:columns(Idx),

    if  JobColumns == Columns -> ok; true ->
        couch_jobs:finish(undefined, Job, Data#{
            error => index_changed,
            reason => <<"Mango Index was modified">>
        }),
        exit(normal)
    end,


    State = #{
        tx_db => undefined,
        idx_vs => undefined,
        idx_seq => undefined,
        last_seq => undefined,
        job => Job,
        job_data => Data,
        count => 0,
        limit => num_changes(),
        doc_acc => []
    },

    try
        update(Db, Idx, State)
    catch
        exit:normal ->
            ok;
        Error:Reason  ->
            NewRetry = Retries + 1,
            RetryLimit = retry_limit(),

            case should_retry(NewRetry, RetryLimit, Reason) of
                true ->
                    DataErr = Data#{<<"retries">> := NewRetry},
                    StateErr = State#{job_data := DataErr},
                    report_progress(StateErr, update);
                false ->
                    NewData = add_error(Error, Reason, Data),
                    couch_jobs:finish(undefined, Job, NewData),
                    exit(normal)
            end
    end.


% Transaction limit exceeded don't retry
should_retry(_, _, {erlfdb_error, 2101}) ->
    false;

should_retry(Retries, RetryLimit, _) when Retries < RetryLimit ->
    true;

should_retry(_, _, _) ->
    false.


add_error(error, {erlfdb_error, Code}, Data) ->
    CodeBin = couch_util:to_binary(Code),
    CodeString = erlfdb:get_error_string(Code),
    Data#{
        error => foundationdb_error,
        reason => list_to_binary([CodeBin, <<"-">>, CodeString])
    };

add_error(Error, Reason, Data) ->
    Data#{
        error => couch_util:to_binary(Error),
        reason => couch_util:to_binary(Reason)
    }.


update(#{} = Db, #idx{} = Idx, State0) ->
    {Idx2, State4} = fabric2_fdb:transactional(Db, fun(TxDb) ->

        State1 = get_update_start_state(TxDb, Idx, State0),
        {ok, State2} = fold_changes(State1),

        #{
            idx_vs := IdxVS,
            count := Count,
            limit := Limit,
            doc_acc := DocAcc,
            idx_seq := IdxSeq
        } = State2,

        DocAcc1 = couch_views_indexer:fetch_docs(TxDb, DocAcc),
        index_docs(TxDb, Idx, DocAcc1),
        mango_fdb:set_update_seq(TxDb, Idx, IdxSeq),
        case Count < Limit of
            true ->
                mango_fdb:set_build_vs(TxDb, Idx, IdxVS, ?MANGO_INDEX_READY),
                report_progress(State2, finished),
                {Idx, finished};
            false ->
                State3 = report_progress(State2, update),
                {Idx, State3#{
                    tx_db := undefined,
                    count := 0,
                    doc_acc := [],
                    idx_seq := IdxSeq
                }}
        end
    end),

    case State4 of
        finished ->
            ok;
        _ ->
            update(Db, Idx2, State4)
    end.


get_update_start_state(TxDb, Idx, #{idx_vs := undefined} = State) ->
    #{
        job := Job,
        job_data := Data
    } = State,

    {IdxVS, BuildState} = mango_fdb:get_build_vs(TxDb, Idx),
    if BuildState == ?MANGO_INDEX_BUILDING -> ok; true ->
        couch_jobs:finish(undefined, Job, Data#{
            error => index_built,
            reason => <<"Index is already built">>
        }),
        exit(normal)
    end,

    IdxSeq = mango_fdb:get_update_seq(TxDb, Idx),

    State#{
        tx_db := TxDb,
        idx_vs := IdxVS,
        idx_seq := IdxSeq
    };

get_update_start_state(TxDb, _Idx, State) ->
    State#{
        tx_db := TxDb
    }.


fold_changes(State) ->
    #{
        idx_seq := SinceSeq,
        limit := Limit,
        tx_db := TxDb
    } = State,

    Fun = fun process_changes/2,
    Opts = [{limit, Limit}, {restart_tx, false}],
    fabric2_db:fold_changes(TxDb, SinceSeq, Fun, State, Opts).


process_changes(Change, Acc) ->
    #{
        doc_acc := DocAcc,
        count := Count,
        idx_vs := IdxVS
    } = Acc,

    #{
        id := Id,
        sequence := LastSeq
    } = Change,

    DocVS = fabric2_fdb:next_vs(fabric2_fdb:seq_to_vs(LastSeq)),

    case IdxVS =< DocVS of
        true ->
            {stop, Acc};
        false ->
            Acc1 = case Id of
                <<?DESIGN_DOC_PREFIX, _/binary>> ->
                    maps:merge(Acc, #{
                        count => Count + 1,
                        idx_seq => LastSeq
                    });
                _ ->
                    Acc#{
                        doc_acc := DocAcc ++ [Change],
                        count := Count + 1,
                        idx_seq := LastSeq
                    }
            end,
            {ok, Acc1}
    end.


index_docs(Db, Idx, Docs) ->
    lists:foreach(fun (Doc) ->
        index_doc(Db, Idx, Doc)
    end, Docs).


index_doc(_Db, _Idx, #{deleted := true}) ->
    ok;

index_doc(Db, Idx, #{doc := Doc}) ->
    mango_indexer:write_doc(Db, Doc, [Idx]).


report_progress(State, UpdateType) ->
    #{
        tx_db := TxDb,
        job := Job1,
        job_data := JobData
    } = State,

    #{
        <<"db_name">> := DbName,
        <<"ddoc_id">> := DDocId,
        <<"columns">> := Columns,
        <<"retries">> := Retries
    } = JobData,

    % Reconstruct from scratch to remove any
    % possible existing error state.
    NewData = #{
        <<"db_name">> => DbName,
        <<"ddoc_id">> => DDocId,
        <<"columns">> => Columns,
        <<"retries">> => Retries
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
    config:get_integer("mango", "change_limit", 100).


retry_limit() ->
    config:get_integer("mango", "retry_limit", 3).
