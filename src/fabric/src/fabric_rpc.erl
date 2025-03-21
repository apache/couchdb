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

-module(fabric_rpc).

-export([
    get_db_info/1,
    get_doc_count/1,
    get_design_doc_count/1,
    get_update_seq/1
]).
-export([
    open_doc/3,
    open_revs/3, open_revs/4,
    get_doc_info/3,
    get_full_doc_info/3,
    get_missing_revs/2, get_missing_revs/3,
    update_docs/3
]).
-export([all_docs/3, changes/3, map_view/4, reduce_view/4, group_info/2]).
-export([
    create_db/1, create_db/2,
    delete_db/1,
    reset_validation_funs/1,
    set_security/3,
    set_revs_limit/3,
    create_shard_db_doc/2,
    delete_shard_db_doc/2,
    get_partition_info/2
]).
-export([get_all_security/2, open_shard/2]).
-export([compact/1, compact/2]).
-export([get_purge_seq/2, get_purged_infos/1, purge_docs/3, set_purge_infos_limit/3]).

-export([
    get_db_info/2,
    get_doc_count/2,
    get_design_doc_count/2,
    get_update_seq/2,
    changes/4,
    map_view/5,
    reduce_view/5,
    group_info/3,
    update_mrview/4,
    get_uuid/1
]).

-include_lib("fabric/include/fabric.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").

%% rpc endpoints
%%  call to with_db will supply your M:F with a Db instance
%%  and then remaining args

%% @equiv changes(DbName, Args, StartSeq, [])
changes(DbName, Args, StartSeq) ->
    changes(DbName, Args, StartSeq, []).

changes(DbName, #changes_args{} = Args, StartSeq, DbOptions) ->
    changes(DbName, [Args], StartSeq, DbOptions);
changes(DbName, Options, StartVector, DbOptions) ->
    set_io_priority(DbName, DbOptions),
    Args0 = lists:keyfind(changes_args, 1, Options),
    #changes_args{dir = Dir, filter_fun = Filter} = Args0,
    Args =
        case Filter of
            {fetch, custom, Style, Req, {DDocId, Rev}, FName} ->
                {ok, DDoc} = ddoc_cache:open_doc(mem3:dbname(DbName), DDocId, Rev),
                Args0#changes_args{
                    filter_fun = {custom, Style, Req, DDoc, FName}
                };
            {fetch, view, Style, {DDocId, Rev}, VName} ->
                {ok, DDoc} = ddoc_cache:open_doc(mem3:dbname(DbName), DDocId, Rev),
                Args0#changes_args{filter_fun = {view, Style, DDoc, VName}};
            _ ->
                Args0
        end,

    DbOpenOptions = Args#changes_args.db_open_options ++ DbOptions,
    case get_or_create_db(DbName, DbOpenOptions) of
        {ok, Db} ->
            StartSeq = calculate_start_seq(Db, config:node_name(), StartVector),
            Enum = fun changes_enumerator/2,
            Opts = [{dir, Dir}],
            Pending0 =
                case Dir of
                    fwd ->
                        couch_db:count_changes_since(Db, StartSeq);
                    rev ->
                        ChangesTotal = couch_db:count_changes_since(Db, 0),
                        ChangesToEnd = couch_db:count_changes_since(Db, StartSeq),
                        ChangesTotal - ChangesToEnd
                end,
            Acc0 = #fabric_changes_acc{
                db = Db,
                seq = StartSeq,
                args = Args,
                options = Options,
                pending = Pending0,
                epochs = couch_db:get_epochs(Db)
            },
            try
                {ok, #fabric_changes_acc{seq = LastSeq, pending = Pending, epochs = Epochs}} =
                    do_changes(Db, StartSeq, Enum, Acc0, Opts),
                rexi:stream_last(
                    {complete, [
                        {seq, {LastSeq, uuid(Db), couch_db:owner_of(Epochs, LastSeq)}},
                        {pending, Pending}
                    ]}
                )
            after
                couch_db:close(Db)
            end;
        Error ->
            rexi:stream_last(Error)
    end.

do_changes(Db, Seq, Enum, #fabric_changes_acc{args = Args} = Acc0, Opts) ->
    #changes_args{filter_fun = Filter, dir = Dir} = Args,

    case Filter of
        {doc_ids, _Style, DocIds} ->
            case length(DocIds) =< couch_changes:doc_ids_limit() of
                true ->
                    couch_changes:send_changes_doc_ids(Db, Seq, Dir, Enum, Acc0, Filter);
                false ->
                    couch_db:fold_changes(Db, Seq, Enum, Acc0, Opts)
            end;
        {design_docs, _Style} ->
            couch_changes:send_changes_design_docs(Db, Seq, Dir, Enum, Acc0, Filter);
        _ ->
            couch_db:fold_changes(Db, Seq, Enum, Acc0, Opts)
    end.

all_docs(DbName, Options, Args0) ->
    case fabric_util:upgrade_mrargs(Args0) of
        #mrargs{keys = undefined} = Args ->
            set_io_priority(DbName, Options),
            {ok, Db} = get_or_create_db(DbName, Options),
            CB = get_view_cb(Args),
            couch_mrview:query_all_docs(Db, Args, CB, Args)
    end.

update_mrview(DbName, {DDocId, Rev}, ViewName, Args0) ->
    {ok, DDoc} = ddoc_cache:open_doc(mem3:dbname(DbName), DDocId, Rev),
    couch_util:with_db(DbName, fun(Db) ->
        UpdateSeq = couch_db:get_update_seq(Db),
        {ok, Pid, _} = couch_mrview:get_view_index_pid(
            Db, DDoc, ViewName, fabric_util:upgrade_mrargs(Args0)
        ),
        couch_index:get_state(Pid, UpdateSeq)
    end).

%% @equiv map_view(DbName, DDoc, ViewName, Args0, [])
map_view(DbName, DDocInfo, ViewName, Args0) ->
    map_view(DbName, DDocInfo, ViewName, Args0, []).

map_view(DbName, {DDocId, Rev}, ViewName, Args0, DbOptions) ->
    {ok, DDoc} = ddoc_cache:open_doc(mem3:dbname(DbName), DDocId, Rev),
    map_view(DbName, DDoc, ViewName, Args0, DbOptions);
map_view(DbName, DDoc, ViewName, Args0, DbOptions) ->
    set_io_priority(DbName, DbOptions),
    Args = fabric_util:upgrade_mrargs(Args0),
    {ok, Db} = get_or_create_db(DbName, DbOptions),
    CB = get_view_cb(Args),
    couch_mrview:query_view(Db, DDoc, ViewName, Args, CB, Args).

%% @equiv reduce_view(DbName, DDoc, ViewName, Args0)
reduce_view(DbName, DDocInfo, ViewName, Args0) ->
    reduce_view(DbName, DDocInfo, ViewName, Args0, []).

reduce_view(DbName, {DDocId, Rev}, ViewName, Args0, DbOptions) ->
    {ok, DDoc} = ddoc_cache:open_doc(mem3:dbname(DbName), DDocId, Rev),
    reduce_view(DbName, DDoc, ViewName, Args0, DbOptions);
reduce_view(DbName, DDoc, ViewName, Args0, DbOptions) ->
    set_io_priority(DbName, DbOptions),
    Args = fabric_util:upgrade_mrargs(Args0),
    {ok, Db} = get_or_create_db(DbName, DbOptions),
    VAcc0 = #vacc{db = Db},
    Callback = fun(Msg, Acc) -> reduce_cb(Msg, Acc, Args#mrargs.extra) end,
    couch_mrview:query_view(Db, DDoc, ViewName, Args, Callback, VAcc0).

create_db(DbName) ->
    create_db(DbName, []).

create_db(DbName, Options) ->
    rexi:reply(
        case couch_server:create(DbName, Options) of
            {ok, _} ->
                ok;
            Error ->
                Error
        end
    ).

create_shard_db_doc(_, Doc) ->
    rexi:reply(mem3_util:write_db_doc(Doc)).

delete_db(DbName) ->
    couch_server:delete(DbName, []).

delete_shard_db_doc(_, DocId) ->
    rexi:reply(mem3_util:delete_db_doc(DocId)).

%% @equiv get_db_info(DbName, [])
get_db_info(DbName) ->
    get_db_info(DbName, []).

get_db_info(DbName, DbOptions) ->
    with_db(DbName, DbOptions, {couch_db, get_db_info, []}).

get_partition_info(DbName, Partition) ->
    with_db(DbName, [], {couch_db, get_partition_info, [Partition]}).

%% equiv get_doc_count(DbName, [])
get_doc_count(DbName) ->
    get_doc_count(DbName, []).

get_doc_count(DbName, DbOptions) ->
    with_db(DbName, DbOptions, {couch_db, get_doc_count, []}).

%% equiv get_design_doc_count(DbName, [])
get_design_doc_count(DbName) ->
    get_design_doc_count(DbName, []).

get_design_doc_count(DbName, DbOptions) ->
    with_db(DbName, DbOptions, {couch_db, get_design_doc_count, []}).

%% equiv get_update_seq(DbName, [])
get_update_seq(DbName) ->
    get_update_seq(DbName, []).

get_update_seq(DbName, DbOptions) ->
    with_db(DbName, DbOptions, {couch_db, get_update_seq, []}).

set_security(DbName, SecObj, Options0) ->
    Options =
        case lists:keyfind(io_priority, 1, Options0) of
            false ->
                [{io_priority, {db_meta, security}} | Options0];
            _ ->
                Options0
        end,
    with_db(DbName, Options, {couch_db, set_security, [SecObj]}).

get_all_security(DbName, Options) ->
    with_db(DbName, Options, {couch_db, get_security, []}).

set_revs_limit(DbName, Limit, Options) ->
    with_db(DbName, Options, {couch_db, set_revs_limit, [Limit]}).

set_purge_infos_limit(DbName, Limit, Options) ->
    with_db(DbName, Options, {couch_db, set_purge_infos_limit, [Limit]}).

open_doc(DbName, DocId, Options) ->
    with_db(DbName, Options, {couch_db, open_doc, [DocId, Options]}).

open_revs(DbName, IdRevsOpts, Options) ->
    with_db(DbName, Options, {couch_db, open_doc_revs, [IdRevsOpts, Options]}).

open_revs(DbName, Id, Revs, Options) ->
    with_db(DbName, Options, {couch_db, open_doc_revs, [Id, Revs, Options]}).

get_full_doc_info(DbName, DocId, Options) ->
    with_db(DbName, Options, {couch_db, get_full_doc_info, [DocId]}).

get_doc_info(DbName, DocId, Options) ->
    with_db(DbName, Options, {couch_db, get_doc_info, [DocId]}).

get_missing_revs(DbName, IdRevsList) ->
    get_missing_revs(DbName, IdRevsList, []).

get_missing_revs(DbName, IdRevsList, Options) ->
    with_db(DbName, Options, {couch_db, get_missing_revs, [IdRevsList]}).

update_docs(DbName, Docs0, Options) ->
    {Docs1, Type} =
        case couch_util:get_value(read_repair, Options) of
            NodeRevs when is_list(NodeRevs) ->
                Filtered = read_repair_filter(DbName, Docs0, NodeRevs, Options),
                {Filtered, ?REPLICATED_CHANGES};
            undefined ->
                X =
                    case proplists:get_value(?REPLICATED_CHANGES, Options) of
                        true -> ?REPLICATED_CHANGES;
                        _ -> ?INTERACTIVE_EDIT
                    end,
                {Docs0, X}
        end,
    Docs2 = make_att_readers(Docs1),
    with_db(DbName, Options, {couch_db, update_docs, [Docs2, Options, Type]}).

get_purged_infos(DbName) ->
    FoldFun = fun({_Seq, _UUID, Id, Revs}, Acc) -> {ok, [{Id, Revs} | Acc]} end,
    with_db(DbName, [], {couch_db, fold_purge_infos, [FoldFun, []]}).

get_purge_seq(DbName, Options) ->
    with_db(DbName, Options, {couch_db, get_purge_seq, []}).

purge_docs(DbName, UUIdsIdsRevs, Options) ->
    with_db(DbName, Options, {couch_db, purge_docs, [UUIdsIdsRevs, Options]}).

%% @equiv group_info(DbName, DDocId, [])
group_info(DbName, DDocId) ->
    group_info(DbName, DDocId, []).

group_info(DbName, DDocId, DbOptions) ->
    with_db(DbName, DbOptions, {couch_mrview, get_info, [DDocId]}).

reset_validation_funs(DbName) ->
    case get_or_create_db(DbName, []) of
        {ok, Db} ->
            couch_db:reload_validation_funs(Db);
        _ ->
            ok
    end.

open_shard(Name, Opts) ->
    set_io_priority(Name, Opts),
    try
        rexi:reply(mem3_util:get_or_create_db(Name, Opts))
    catch
        exit:{timeout, _} ->
            couch_stats:increment_counter([fabric, open_shard, timeouts])
    end.

compact(DbName) ->
    with_db(DbName, [], {couch_db, start_compact, []}).

compact(ShardName, DesignName) ->
    {ok, Pid} = couch_index_server:get_index(
        couch_mrview_index, ShardName, <<"_design/", DesignName/binary>>
    ),
    Ref = erlang:make_ref(),
    Pid ! {'$gen_call', {self(), Ref}, compact}.

get_uuid(DbName) ->
    with_db(DbName, [], {couch_db, get_uuid, []}).

%%
%% internal
%%

with_db(DbName, Options, {M, F, A}) ->
    set_io_priority(DbName, Options),
    case get_or_create_db(DbName, Options) of
        {ok, Db} ->
            rexi:reply(
                try
                    apply(M, F, [Db | A])
                catch
                    Exception ->
                        Exception;
                    error:Reason:Stack ->
                        couch_log:error("rpc ~p:~p/~p ~p ~p", [
                            M,
                            F,
                            length(A) + 1,
                            Reason,
                            clean_stack(Stack)
                        ]),
                        {error, Reason}
                end
            );
        Error ->
            rexi:reply(Error)
    end.

read_repair_filter(DbName, Docs, NodeRevs, Options) ->
    set_io_priority(DbName, Options),
    case get_or_create_db(DbName, Options) of
        {ok, Db} ->
            try
                read_repair_filter(Db, Docs, NodeRevs)
            after
                couch_db:close(Db)
            end;
        Error ->
            rexi:reply(Error)
    end.

% A read repair operation may have been triggered by a node
% that was out of sync with the local node. Thus, any time
% we receive a read repair request we need to check if we
% may have recently purged any of the given revisions and
% ignore them if so.
%
% This is accomplished by looking at the purge infos that we
% have locally that have not been replicated to the remote
% node. The logic here is that we may have received the purge
% request before the remote shard copy. So to check that we
% need to look at the purge infos that we have locally but
% have not yet sent to the remote copy.
%
% NodeRevs is a list of the {node(), [rev()]} tuples passed
% as the read_repair option to update_docs.
read_repair_filter(Db, Docs, NodeRevs) ->
    [#doc{id = DocId} | _] = Docs,
    NonLocalNodeRevs = [NR || {N, _} = NR <- NodeRevs, N /= config:node_name()],
    Nodes = lists:usort([Node || {Node, _} <- NonLocalNodeRevs]),
    NodeSeqs = get_node_seqs(Db, Nodes),

    DbPSeq = couch_db:get_purge_seq(Db),
    Lag = config:get_integer("couchdb", "read_repair_lag", 100),

    % Filter out read-repair updates from any node that is
    % so out of date that it would force us to scan a large
    % number of purge infos
    NodeFiltFun = fun({Node, _Revs}) ->
        {Node, NodeSeq} = lists:keyfind(Node, 1, NodeSeqs),
        NodeSeq >= DbPSeq - Lag
    end,
    RecentNodeRevs = lists:filter(NodeFiltFun, NonLocalNodeRevs),

    % For each node we scan the purge infos to filter out any
    % revisions that have been locally purged since we last
    % replicated to the remote node's shard copy.
    AllowableRevs = lists:foldl(
        fun({Node, Revs}, RevAcc) ->
            {Node, StartSeq} = lists:keyfind(Node, 1, NodeSeqs),
            FoldFun = fun({_PSeq, _UUID, PDocId, PRevs}, InnerAcc) ->
                if
                    PDocId /= DocId -> {ok, InnerAcc};
                    true -> {ok, InnerAcc -- PRevs}
                end
            end,
            {ok, FiltRevs} = couch_db:fold_purge_infos(Db, StartSeq, FoldFun, Revs),
            lists:usort(FiltRevs ++ RevAcc)
        end,
        [],
        RecentNodeRevs
    ),

    % Finally, filter the doc updates to only include revisions
    % that have not been purged locally.
    DocFiltFun = fun(#doc{revs = {Pos, [Rev | _]}}) ->
        lists:member({Pos, Rev}, AllowableRevs)
    end,
    lists:filter(DocFiltFun, Docs).

get_node_seqs(Db, Nodes) ->
    % Gather the list of {Node, PurgeSeq} pairs for all nodes
    % that are present in our read repair group
    FoldFun = fun(#doc{id = Id, body = {Props}}, Acc) ->
        case Id of
            <<?LOCAL_DOC_PREFIX, "purge-mem3-", _/binary>> ->
                TgtNode = couch_util:get_value(<<"target_node">>, Props),
                PurgeSeq = couch_util:get_value(<<"purge_seq">>, Props),
                case lists:keyfind(TgtNode, 1, Acc) of
                    {_, OldSeq} ->
                        NewSeq = erlang:max(OldSeq, PurgeSeq),
                        NewEntry = {TgtNode, NewSeq},
                        NewAcc = lists:keyreplace(TgtNode, 1, Acc, NewEntry),
                        {ok, NewAcc};
                    false ->
                        {ok, Acc}
                end;
            _ ->
                % We've processed all _local mem3 purge docs
                {stop, Acc}
        end
    end,
    InitAcc = [{list_to_binary(atom_to_list(Node)), 0} || Node <- Nodes],
    Opts = [{start_key, <<?LOCAL_DOC_PREFIX, "purge-mem3-">>}],
    {ok, NodeBinSeqs} = couch_db:fold_local_docs(Db, FoldFun, InitAcc, Opts),
    [{list_to_existing_atom(binary_to_list(N)), S} || {N, S} <- NodeBinSeqs].

get_or_create_db(DbName, Options) ->
    mem3_util:get_or_create_db_int(DbName, Options).

get_view_cb(#mrargs{extra = Options}) ->
    case couch_util:get_value(callback, Options) of
        {Mod, Fun} when is_atom(Mod), is_atom(Fun) ->
            fun Mod:Fun/2;
        _ ->
            fun view_cb/2
    end;
get_view_cb(_) ->
    fun view_cb/2.

view_cb({meta, Meta}, Acc) ->
    % Map function starting
    ok = rexi:stream2({meta, Meta}),
    {ok, Acc};
view_cb({row, Props}, #mrargs{extra = Options} = Acc) ->
    % Adding another row
    ViewRow = fabric_view_row:from_props(Props, Options),
    ok = rexi:stream2(ViewRow),
    {ok, Acc};
view_cb(complete, Acc) ->
    % Finish view output
    ok = rexi:stream_last(complete),
    {ok, Acc};
view_cb(ok, ddoc_updated) ->
    rexi:reply({ok, ddoc_updated});
view_cb(ok, insufficient_storage) ->
    rexi:reply({ok, insufficient_storage}).

reduce_cb({meta, Meta}, Acc, _Options) ->
    % Map function starting
    ok = rexi:stream2({meta, Meta}),
    {ok, Acc};
reduce_cb({row, Props}, Acc, Options) ->
    % Adding another row
    ViewRow = fabric_view_row:from_props(Props, Options),
    ok = rexi:stream2(ViewRow),
    {ok, Acc};
reduce_cb(complete, Acc, _Options) ->
    % Finish view output
    ok = rexi:stream_last(complete),
    {ok, Acc};
reduce_cb(ok, ddoc_updated, _Options) ->
    rexi:reply({ok, ddoc_updated});
reduce_cb(ok, insufficient_storage, _Options) ->
    rexi:reply({ok, insufficient_storage}).

changes_enumerator(#full_doc_info{} = FDI, Acc) ->
    changes_enumerator(couch_doc:to_doc_info(FDI), Acc);
changes_enumerator(#doc_info{id = <<"_local/", _/binary>>, high_seq = Seq}, Acc) ->
    {ok, Acc#fabric_changes_acc{seq = Seq, pending = Acc#fabric_changes_acc.pending - 1}};
changes_enumerator(DocInfo, Acc) ->
    #fabric_changes_acc{
        db = Db,
        args = #changes_args{
            include_docs = IncludeDocs,
            conflicts = Conflicts,
            filter_fun = Filter,
            doc_options = DocOptions
        },
        pending = Pending,
        epochs = Epochs
    } = Acc,
    Opts =
        case Conflicts of
            true -> [conflicts | DocOptions];
            false -> DocOptions
        end,
    Seq = DocInfo#doc_info.high_seq,
    {Docs0, Revs} = couch_changes:filter(Db, DocInfo, Filter, IncludeDocs, Conflicts),
    Changes = [X || X <- Revs, X /= null],
    Docs = [X || X <- Docs0, X /= null],
    ChangesRow =
        case {Changes, Docs, IncludeDocs} of
            {[], _, _} ->
                {no_pass, [
                    {pending, Pending - 1},
                    {seq, {Seq, uuid(Db), couch_db:owner_of(Epochs, Seq)}}
                ]};
            {_, _, false} ->
                changes_row(Changes, [], DocInfo, Acc);
            {_, [], true} ->
                Docs1 = [doc_member(Db, DocInfo, Opts, Filter)],
                changes_row(Changes, Docs1, DocInfo, Acc);
            {_, [Doc | _], true} ->
                Docs1 = [get_json_doc(Doc, Opts, Filter)],
                changes_row(Changes, Docs1, DocInfo, Acc)
        end,
    ok = rexi:stream2(ChangesRow),
    {ok, Acc#fabric_changes_acc{seq = Seq, pending = Pending - 1}}.

changes_row(Changes, Docs, DocInfo, Acc) ->
    #fabric_changes_acc{db = Db, pending = Pending, epochs = Epochs} = Acc,
    #doc_info{id = Id, high_seq = Seq, revs = [#rev_info{deleted = Del} | _]} = DocInfo,
    {change, [
        {pending, Pending - 1},
        {seq, {Seq, uuid(Db), couch_db:owner_of(Epochs, Seq)}},
        {id, Id},
        {changes, Changes},
        {deleted, Del}
        | Docs
    ]}.

doc_member(Shard, DocInfo, Opts, Filter) ->
    case couch_db:open_doc(Shard, DocInfo, [deleted | Opts]) of
        {ok, Doc} ->
            get_json_doc(Doc, Opts, Filter);
        Error ->
            Error
    end.

get_json_doc(Doc, Opts, Filter) ->
    {doc, maybe_filtered_json_doc(Doc, Opts, Filter)}.

maybe_filtered_json_doc(Doc, Opts, {selector, _Style, {_Selector, Fields}}) when
    Fields =/= nil
->
    mango_fields:extract(couch_doc:to_json_obj(Doc, Opts), Fields);
maybe_filtered_json_doc(Doc, Opts, _Filter) ->
    couch_doc:to_json_obj(Doc, Opts).

make_att_readers([]) ->
    [];
make_att_readers([#doc{atts = Atts0} = Doc | Rest]) ->
    % % go through the attachments looking for 'follows' in the data,
    % % replace with function that reads the data from MIME stream.
    Atts = [couch_att:transform(data, fun make_att_reader/1, Att) || Att <- Atts0],
    [Doc#doc{atts = Atts} | make_att_readers(Rest)].

make_att_reader({follows, Parser, Ref}) when is_pid(Parser) ->
    % This code will fail if the returned closure is called by a
    % process other than the one that called make_att_reader/1 in the
    % first place. The reason we don't put everything inside the
    % closure is that the `hello_from_writer` message must *always* be
    % sent to the parser, even if the closure never gets called. Also,
    % make sure `hello_from_writer` is sent only once for the all the
    % rest of the possible attachments.
    WriterPid = self(),
    ParserRef =
        case get({mp_parser_ref, Parser}) of
            undefined ->
                % First time encountering a particular parser pid. Monitor it,
                % in case it dies, and notify it about us, so it could monitor
                % us in case we die.
                PRef = erlang:monitor(process, Parser),
                put({mp_parser_ref, Parser}, PRef),
                Parser ! {hello_from_writer, Ref, WriterPid},
                PRef;
            Else ->
                Else
        end,
    fun() ->
        % Make sure the closure is always called from the same process which
        % sent the hello_from_writer message.
        case self() =:= WriterPid of
            true -> ok;
            false -> error({make_att_pid_assertion, self(), WriterPid})
        end,
        % Check if parser already died. This is for belt and suspenders mostly,
        % in case somehow we call the data function again after mp_parser_died
        % was thrown, so we are not stuck forever waiting for bytes.
        case get({mp_parser_died, Parser}) of
            undefined -> ok;
            AlreadyDiedReason -> throw({mp_parser_died, AlreadyDiedReason})
        end,
        Parser ! {get_bytes, Ref, self()},
        receive
            {bytes, Ref, Bytes} ->
                rexi:reply(attachment_chunk_received),
                Bytes;
            {'DOWN', ParserRef, _, _, Reason} ->
                put({mp_parser_died, Parser}, Reason),
                throw({mp_parser_died, Reason})
        end
    end;
make_att_reader({fabric_attachment_receiver, Middleman, Length}) ->
    fabric_doc_atts:receiver_callback(Middleman, Length);
make_att_reader(Else) ->
    Else.

clean_stack(S) ->
    lists:map(
        fun
            ({M, F, A}) when is_list(A) -> {M, F, length(A)};
            (X) -> X
        end,
        S
    ).

set_io_priority(DbName, Options) ->
    case lists:keyfind(io_priority, 1, Options) of
        {io_priority, Pri} ->
            erlang:put(io_priority, Pri);
        false ->
            erlang:put(io_priority, {interactive, DbName})
    end,
    case erlang:get(io_priority) of
        {interactive, _} ->
            case config:get("couchdb", "maintenance_mode", "false") of
                "true" ->
                    % Done to silence error logging by rexi_server
                    rexi:reply({rexi_EXIT, {maintenance_mode, node()}}),
                    exit(normal);
                _ ->
                    ok
            end;
        _ ->
            ok
    end.

calculate_start_seq(Db, Node, Seq) ->
    case couch_db:calculate_start_seq(Db, Node, Seq) of
        N when is_integer(N) ->
            N;
        {replace, OriginalNode, Uuid, OriginalSeq} ->
            %% Scan history looking for an entry with
            %%  * target_node == TargetNode
            %%  * target_uuid == TargetUUID
            %%  * target_seq  =< TargetSeq
            %% If such an entry is found, stream from associated source_seq
            mem3_rep:find_source_seq(Db, OriginalNode, Uuid, OriginalSeq)
    end.

uuid(Db) ->
    Uuid = couch_db:get_uuid(Db),
    Prefix = fabric_util:get_uuid_prefix_len(),
    binary:part(Uuid, {0, Prefix}).

-ifdef(TEST).
-include_lib("couch/include/couch_eunit.hrl").

maybe_filtered_json_doc_no_filter_test() ->
    Body = {[{<<"a">>, 1}]},
    Doc = #doc{id = <<"1">>, revs = {1, [<<"r1">>]}, body = Body},
    {JDocProps} = maybe_filtered_json_doc(Doc, [], x),
    ExpectedProps = [{<<"_id">>, <<"1">>}, {<<"_rev">>, <<"1-r1">>}, {<<"a">>, 1}],
    ?assertEqual(lists:keysort(1, JDocProps), ExpectedProps).

maybe_filtered_json_doc_with_filter_test() ->
    Body = {[{<<"a">>, 1}]},
    Doc = #doc{id = <<"1">>, revs = {1, [<<"r1">>]}, body = Body},
    Fields = [<<"a">>, <<"nonexistent">>],
    Filter = {selector, main_only, {some_selector, Fields}},
    {JDocProps} = maybe_filtered_json_doc(Doc, [], Filter),
    ?assertEqual(JDocProps, [{<<"a">>, 1}]).

view_cb_test_() ->
    {
        foreach,
        fun() -> meck:new(rexi) end,
        fun(_) -> meck:unload() end,
        [
            ?TDEF_FE(t_view_cb_meta),
            ?TDEF_FE(t_view_cb_row_record),
            ?TDEF_FE(t_view_cb_row_map),
            ?TDEF_FE(t_view_cb_complete),
            ?TDEF_FE(t_view_cb_ddoc_updated),
            ?TDEF_FE(t_view_cb_insufficient_storage)
        ]
    }.

t_view_cb_meta(_) ->
    meck:expect(rexi, stream2, [{meta, meta}], meck:val(ok)),
    ?assertEqual({ok, acc}, view_cb({meta, meta}, acc)).

t_view_cb_row_record(_) ->
    Acc = #mrargs{},
    Props = [{id, id}, {key, key}, {value, value}],
    ViewRow = #view_row{id = id, key = key, value = value},
    meck:expect(rexi, stream2, [ViewRow], meck:val(ok)),
    ?assertEqual({ok, Acc}, view_cb({row, Props}, Acc)).

t_view_cb_row_map(_) ->
    Acc = #mrargs{extra = [{view_row_map, true}]},
    Props = [{id, id}, {key, key}, {value, value}],
    ViewRow = {view_row, #{id => id, key => key, value => value}},
    meck:expect(rexi, stream2, [ViewRow], meck:val(ok)),
    ?assertEqual({ok, Acc}, view_cb({row, Props}, Acc)).

t_view_cb_complete(_) ->
    meck:expect(rexi, stream_last, [complete], meck:val(ok)),
    ?assertEqual({ok, acc}, view_cb(complete, acc)).

t_view_cb_ddoc_updated(_) ->
    meck:expect(rexi, reply, [{ok, ddoc_updated}], meck:val(ok)),
    ?assertEqual(ok, view_cb(ok, ddoc_updated)).

t_view_cb_insufficient_storage(_) ->
    meck:expect(rexi, reply, [{ok, insufficient_storage}], meck:val(ok)),
    ?assertEqual(ok, view_cb(ok, insufficient_storage)).

reduce_cb_test_() ->
    {
        foreach,
        fun() -> meck:new(rexi) end,
        fun(_) -> meck:unload() end,
        [
            ?TDEF_FE(t_reduce_cb_meta),
            ?TDEF_FE(t_reduce_cb_row_record),
            ?TDEF_FE(t_reduce_cb_row_map),
            ?TDEF_FE(t_reduce_cb_complete),
            ?TDEF_FE(t_reduce_cb_ddoc_updated),
            ?TDEF_FE(t_reduce_cb_insufficient_storage)
        ]
    }.

t_reduce_cb_meta(_) ->
    meck:expect(rexi, stream2, [{meta, meta}], meck:val(ok)),
    ?assertEqual({ok, acc}, reduce_cb({meta, meta}, acc, options)).

t_reduce_cb_row_record(_) ->
    Options = [],
    Props = [{id, id}, {key, key}, {value, value}],
    ViewRow = #view_row{id = id, key = key, value = value},
    meck:expect(rexi, stream2, [ViewRow], meck:val(ok)),
    ?assertEqual({ok, acc}, reduce_cb({row, Props}, acc, Options)).

t_reduce_cb_row_map(_) ->
    Options = [{view_row_map, true}],
    Props = [{id, id}, {key, key}, {value, value}],
    ViewRow = {view_row, #{id => id, key => key, value => value}},
    meck:expect(rexi, stream2, [ViewRow], meck:val(ok)),
    ?assertEqual({ok, acc}, reduce_cb({row, Props}, acc, Options)).

t_reduce_cb_complete(_) ->
    meck:expect(rexi, stream_last, [complete], meck:val(ok)),
    ?assertEqual({ok, acc}, reduce_cb(complete, acc, options)).

t_reduce_cb_ddoc_updated(_) ->
    meck:expect(rexi, reply, [{ok, ddoc_updated}], meck:val(ok)),
    ?assertEqual(ok, reduce_cb(ok, ddoc_updated, options)).

t_reduce_cb_insufficient_storage(_) ->
    meck:expect(rexi, reply, [{ok, insufficient_storage}], meck:val(ok)),
    ?assertEqual(ok, reduce_cb(ok, insufficient_storage, options)).

-endif.
