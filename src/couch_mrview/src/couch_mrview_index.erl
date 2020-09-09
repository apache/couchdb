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

-module(couch_mrview_index).


-export([get/2]).
-export([init/2, open/2, close/1, reset/1, delete/1, shutdown/1]).
-export([start_update/4, purge/4, process_doc/3, finish_update/1, commit/1]).
-export([compact/3, swap_compacted/2, remove_compacted/1]).
-export([index_file_exists/1]).
-export([update_local_purge_doc/2, verify_index_exists/2]).
-export([ensure_local_purge_docs/2]).
-export([format_status/2]).

-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").


get(db_name, #mrst{db_name = DbName}) ->
    DbName;
get(idx_name, #mrst{idx_name = IdxName}) ->
    IdxName;
get(signature, #mrst{sig = Signature}) ->
    Signature;
get(update_seq, #mrst{update_seq = UpdateSeq}) ->
    UpdateSeq;
get(purge_seq, #mrst{purge_seq = PurgeSeq}) ->
    PurgeSeq;
get(update_options, #mrst{design_opts = Opts}) ->
    IncDesign = couch_util:get_value(<<"include_design">>, Opts, false),
    LocalSeq = couch_util:get_value(<<"local_seq">>, Opts, false),
    Partitioned = couch_util:get_value(<<"partitioned">>, Opts, false),
    if IncDesign -> [include_design]; true -> [] end
        ++ if LocalSeq -> [local_seq]; true -> [] end
        ++ if Partitioned -> [partitioned]; true -> [] end;
get(fd, #mrst{fd = Fd}) ->
    Fd;
get(language, #mrst{language = Language}) ->
    Language;
get(views, #mrst{views = Views}) ->
    Views;
get(info, State) ->
    #mrst{
        fd = Fd,
        sig = Sig,
        id_btree = IdBtree,
        language = Lang,
        update_seq = UpdateSeq,
        purge_seq = PurgeSeq,
        views = Views
    } = State,
    {ok, FileSize} = couch_file:bytes(Fd),
    {ok, ExternalSize} = couch_mrview_util:calculate_external_size(Views),
    {ok, ActiveViewSize} = couch_mrview_util:calculate_active_size(Views),
    ActiveSize = couch_btree:size(IdBtree) + ActiveViewSize,

    UpdateOptions0 = get(update_options, State),
    UpdateOptions = [atom_to_binary(O, latin1) || O <- UpdateOptions0],

    {ok, [
        {signature, list_to_binary(couch_index_util:hexsig(Sig))},
        {language, Lang},
        {sizes, {[
            {file, FileSize},
            {active, ActiveSize},
            {external, ExternalSize}
        ]}},
        {update_seq, UpdateSeq},
        {purge_seq, PurgeSeq},
        {update_options, UpdateOptions}
    ]};
get(Other, _) ->
    throw({unknown_index_property, Other}).


init(Db, DDoc) ->
    {ok, State} = couch_mrview_util:ddoc_to_mrst(couch_db:name(Db), DDoc),
    {ok, set_partitioned(Db, State)}.


open(Db, State0) ->
    #mrst{
        db_name=DbName,
        sig=Sig
    } = State = set_partitioned(Db, State0),
    IndexFName = couch_mrview_util:index_file(DbName, Sig),

    % If we are upgrading from <= 2.x, we upgrade the view
    % index file on the fly, avoiding an index reset.
    % We are making commit with a new state
    % right after the upgrade to ensure
    % that we have a proper sig in the header
    % when open the view next time
    %
    % OldSig is `ok` if no upgrade happened.
    %
    % To remove support for 2.x auto-upgrades in the
    % future, just remove the next line and the code
    % between "upgrade code for <= 2.x" and
    % "end of upgrade code for <= 2.x" and the corresponding
    % code in couch_mrview_util

    OldSig = couch_mrview_util:maybe_update_index_file(State),

    case couch_mrview_util:open_file(IndexFName) of
        {ok, Fd} ->
            case couch_file:read_header(Fd) of
                % upgrade code for <= 2.x
                {ok, {OldSig, Header}} ->
                    % Matching view signatures.
                    NewSt = couch_mrview_util:init_state(Db, Fd, State, Header),
                    ok = commit(NewSt),
                    ensure_local_purge_doc(Db, NewSt),
                    {ok, NewSt};
                % end of upgrade code for <= 2.x
                {ok, {Sig, Header}} ->
                    % Matching view signatures.
                    NewSt = couch_mrview_util:init_state(Db, Fd, State, Header),
                    ensure_local_purge_doc(Db, NewSt),
                    {ok, NewSt};
                {ok, {WrongSig, _}} ->
                    couch_log:error("~s has the wrong signature: expected: ~p but got ~p",
                        [IndexFName, Sig, WrongSig]),
                    NewSt = couch_mrview_util:reset_index(Db, Fd, State),
                    ensure_local_purge_doc(Db, NewSt),
                    {ok, NewSt};
                {ok, Else} ->
                    couch_log:error("~s has a bad header: got ~p",
                        [IndexFName, Else]),
                    NewSt = couch_mrview_util:reset_index(Db, Fd, State),
                    ensure_local_purge_doc(Db, NewSt),
                    {ok, NewSt};
                no_valid_header ->
                    NewSt = couch_mrview_util:reset_index(Db, Fd, State),
                    ensure_local_purge_doc(Db, NewSt),
                    {ok, NewSt}
            end;
        {error, Reason} = Error ->
            couch_log:error("Failed to open view file '~s': ~s",
                            [IndexFName, file:format_error(Reason)]),
            Error
    end.


close(State) ->
    erlang:demonitor(State#mrst.fd_monitor, [flush]),
    couch_file:close(State#mrst.fd).


% This called after ddoc_updated event occurrs, and
% before we shutdown couch_index process.
% We unlink couch_index from corresponding couch_file and demonitor it.
% This allows all outstanding queries that are currently streaming
% data from couch_file finish successfully.
% couch_file will be closed automatically after all
% outstanding queries are done.
shutdown(State) ->
    erlang:demonitor(State#mrst.fd_monitor, [flush]),
    unlink(State#mrst.fd).


delete(#mrst{db_name=DbName, sig=Sig}=State) ->
    couch_file:close(State#mrst.fd),
    catch couch_mrview_util:delete_files(DbName, Sig).


reset(State) ->
    couch_util:with_db(State#mrst.db_name, fun(Db) ->
        NewState = couch_mrview_util:reset_index(Db, State#mrst.fd, State),
        {ok, NewState}
    end).


start_update(PartialDest, State, NumChanges, NumChangesDone) ->
    couch_mrview_updater:start_update(
        PartialDest,
        State,
        NumChanges,
        NumChangesDone
    ).


purge(Db, PurgeSeq, PurgedIdRevs, State) ->
    couch_mrview_updater:purge(Db, PurgeSeq, PurgedIdRevs, State).


process_doc(Doc, Seq, State) ->
    couch_mrview_updater:process_doc(Doc, Seq, State).


finish_update(State) ->
    couch_mrview_updater:finish_update(State).


commit(State) ->
    Header = {State#mrst.sig, couch_mrview_util:make_header(State)},
    couch_file:write_header(State#mrst.fd, Header).


compact(Db, State, Opts) ->
    couch_mrview_compactor:compact(Db, State, Opts).


swap_compacted(OldState, NewState) ->
    couch_mrview_compactor:swap_compacted(OldState, NewState).


remove_compacted(State) ->
    couch_mrview_compactor:remove_compacted(State).


index_file_exists(State) ->
    #mrst{
        db_name=DbName,
        sig=Sig
    } = State,
    IndexFName = couch_mrview_util:index_file(DbName, Sig),
    filelib:is_file(IndexFName).


verify_index_exists(DbName, Props) ->
    try
        Type = couch_util:get_value(<<"type">>, Props),
        if Type =/= <<"mrview">> -> false; true ->
            DDocId = couch_util:get_value(<<"ddoc_id">>, Props),
            couch_util:with_db(DbName, fun(Db) ->
                case couch_db:get_design_doc(Db, DDocId) of
                    {ok, #doc{} = DDoc} ->
                        {ok, IdxState} = couch_mrview_util:ddoc_to_mrst(
                            DbName, DDoc),
                        IdxSig = IdxState#mrst.sig,
                        SigInLocal = couch_util:get_value(
                            <<"signature">>, Props),
                        couch_index_util:hexsig(IdxSig) == SigInLocal;
                    {not_found, _} ->
                        false
                end
            end)
        end
    catch _:_ ->
        false
    end.


set_partitioned(Db, State) ->
    #mrst{
        design_opts = DesignOpts
    } = State,
    DbPartitioned = couch_db:is_partitioned(Db),
    ViewPartitioned = couch_util:get_value(
            <<"partitioned">>, DesignOpts, DbPartitioned),
    IsPartitioned = DbPartitioned andalso ViewPartitioned,
    State#mrst{partitioned = IsPartitioned}.


ensure_local_purge_docs(DbName, DDocs) ->
    couch_util:with_db(DbName, fun(Db) ->
        lists:foreach(fun(DDoc) ->
            try couch_mrview_util:ddoc_to_mrst(DbName, DDoc) of
                {ok, MRSt} ->
                    ensure_local_purge_doc(Db, MRSt)
            catch _:_ ->
                ok
            end
        end, DDocs)
    end).


ensure_local_purge_doc(Db, #mrst{}=State) ->
    Sig = couch_index_util:hexsig(get(signature, State)),
    DocId = couch_mrview_util:get_local_purge_doc_id(Sig),
    case couch_db:open_doc(Db, DocId, []) of
        {not_found, _} ->
            create_local_purge_doc(Db, State);
        {ok, _} ->
            ok
    end.


create_local_purge_doc(Db, State) ->
    PurgeSeq = couch_db:get_purge_seq(Db),
    update_local_purge_doc(Db, State, PurgeSeq).


update_local_purge_doc(Db, State) ->
    update_local_purge_doc(Db, State, get(purge_seq, State)).


update_local_purge_doc(Db, State, PSeq) ->
    Sig = couch_index_util:hexsig(State#mrst.sig),
    DocId = couch_mrview_util:get_local_purge_doc_id(Sig),
    {Mega, Secs, _} = os:timestamp(),
    NowSecs = Mega * 1000000 + Secs,
    BaseDoc = couch_doc:from_json_obj({[
        {<<"_id">>, DocId},
        {<<"type">>, <<"mrview">>},
        {<<"purge_seq">>, PSeq},
        {<<"updated_on">>, NowSecs},
        {<<"ddoc_id">>, get(idx_name, State)},
        {<<"signature">>, Sig}
    ]}),
    Doc = case couch_db:open_doc(Db, DocId, []) of
        {ok, #doc{revs = Revs}} ->
            BaseDoc#doc{revs = Revs};
        {not_found, _} ->
            BaseDoc
    end,
    couch_db:update_doc(Db, Doc, []).

format_status(_Opt, [_PDict, State]) ->
    Scrubbed = State#mrst{
        lib = nil,
        views = nil,
        id_btree = nil,
        doc_acc = nil,
        doc_queue = nil,
        write_queue = nil
    },
    ?record_to_keyval(mrst, Scrubbed).
