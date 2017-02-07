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
-export([init/2, open/2, close/1, reset/1, delete/1]).
-export([start_update/3, purge/4, process_doc/3, finish_update/1, commit/1]).
-export([compact/3, swap_compacted/2]).
-export([index_file_exists/1]).

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
    SeqIndexed = couch_util:get_value(<<"seq_indexed">>, Opts, false),
    KeySeqIndexed = couch_util:get_value(<<"keyseq_indexed">>, Opts, false),
    if IncDesign -> [include_design]; true -> [] end
        ++ if LocalSeq -> [local_seq]; true -> [] end
        ++ if KeySeqIndexed -> [keyseq_indexed]; true -> [] end
        ++ if SeqIndexed -> [seq_indexed]; true -> [] end;
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
        log_btree = LogBtree,
        language = Lang,
        update_seq = UpdateSeq,
        purge_seq = PurgeSeq,
        views = Views,
        design_opts = Opts
    } = State,
    {ok, FileSize} = couch_file:bytes(Fd),
    {ok, ExternalSize} = couch_mrview_util:calculate_external_size(Views),
    LogBtSize = case LogBtree of
        nil ->
            0;
        _ ->
            couch_btree:size(LogBtree)
    end,
    ActiveSize = couch_btree:size(IdBtree) + LogBtSize + ExternalSize,

    IncDesign = couch_util:get_value(<<"include_design">>, Opts, false),
    LocalSeq = couch_util:get_value(<<"local_seq">>, Opts, false),
    SeqIndexed = couch_util:get_value(<<"seq_indexed">>, Opts, false),
    KeySeqIndexed = couch_util:get_value(<<"keyseq_indexed">>, Opts, false),
    UpdateOptions =
        if IncDesign -> [<<"include_design">>]; true -> [] end
        ++ if LocalSeq -> [<<"local_seq">>]; true -> [] end
        ++ if KeySeqIndexed -> [<<"keyseq_indexed">>]; true -> [] end
        ++ if SeqIndexed -> [<<"seq_indexed">>]; true -> [] end,


    {ok, [
        {signature, list_to_binary(couch_index_util:hexsig(Sig))},
        {language, Lang},
        {disk_size, FileSize}, % legacy
        {data_size, ExternalSize}, % legacy
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
    couch_mrview_util:ddoc_to_mrst(couch_db:name(Db), DDoc).


open(Db, State) ->
    #mrst{
        db_name=DbName,
        sig=Sig
    } = State,
    IndexFName = couch_mrview_util:index_file(DbName, Sig),

    % If we are upgrading from <=1.2.x, we upgrade the view
    % index file on the fly, avoiding an index reset.
    %
    % OldSig is `ok` if no upgrade happened.
    %
    % To remove suppport for 1.2.x auto-upgrades in the
    % future, just remove the next line and the code
    % between "upgrade code for <= 1.2.x" and
    % "end upgrade code for <= 1.2.x" and the corresponding
    % code in couch_mrview_util

    OldSig = couch_mrview_util:maybe_update_index_file(State),

    case couch_mrview_util:open_file(IndexFName) of
        {ok, Fd} ->
            case (catch couch_file:read_header(Fd)) of
                % upgrade code for <= 1.2.x
                {ok, {OldSig, Header}} ->
                    % Matching view signatures.
                    NewSt = couch_mrview_util:init_state(Db, Fd, State, Header),
                    {ok, NewSt};
                % end of upgrade code for <= 1.2.x
                {ok, {Sig, Header}} ->
                    % Matching view signatures.
                    NewSt = couch_mrview_util:init_state(Db, Fd, State, Header),
                    {ok, NewSt};
                _ ->
                    NewSt = couch_mrview_util:reset_index(Db, Fd, State),
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


delete(#mrst{db_name=DbName, sig=Sig}=State) ->
    couch_file:close(State#mrst.fd),
    catch couch_mrview_util:delete_files(DbName, Sig).


reset(State) ->
    couch_util:with_db(State#mrst.db_name, fun(Db) ->
        NewState = couch_mrview_util:reset_index(Db, State#mrst.fd, State),
        {ok, NewState}
    end).


start_update(PartialDest, State, NumChanges) ->
    couch_mrview_updater:start_update(PartialDest, State, NumChanges).


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


index_file_exists(State) ->
    #mrst{
        db_name=DbName,
        sig=Sig
    } = State,
    IndexFName = couch_mrview_util:index_file(DbName, Sig),
    filelib:is_file(IndexFName).
