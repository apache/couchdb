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

-export([get_db_info/1, get_doc_count/1, get_update_seq/1]).
-export([open_doc/3, open_revs/4, get_doc_info/3, get_full_doc_info/3,
    get_missing_revs/2, get_missing_revs/3, update_docs/3]).
-export([all_docs/3, changes/3, map_view/4, reduce_view/4, group_info/2]).
-export([create_db/1, create_db/2, delete_db/1, reset_validation_funs/1,
    set_security/3, set_revs_limit/3, create_shard_db_doc/2,
    delete_shard_db_doc/2]).
-export([get_all_security/2, open_shard/2]).
-export([compact/1, compact/2]).

-export([get_db_info/2, get_doc_count/2, get_update_seq/2,
         changes/4, map_view/5, reduce_view/5, group_info/3, update_mrview/4]).

-include_lib("fabric/include/fabric.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").

-record (cacc, {
    db,
    seq,
    args,
    options,
    pending,
    epochs
}).

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
    #changes_args{dir=Dir, filter_fun=Filter} = Args0,
    Args = case Filter of
        {fetch, custom, Style, Req, {DDocId, Rev}, FName} ->
            {ok, DDoc} = ddoc_cache:open_doc(mem3:dbname(DbName), DDocId, Rev),
            Args0#changes_args{
                filter_fun={custom, Style, Req, DDoc, FName}
            };
        {fetch, FilterType, Style, {DDocId, Rev}, VName}
                when FilterType == view orelse FilterType == fast_view ->
            {ok, DDoc} = ddoc_cache:open_doc(mem3:dbname(DbName), DDocId, Rev),
            Args0#changes_args{filter_fun={FilterType, Style, DDoc, VName}};
        _ ->
            Args0
    end,

    DbOpenOptions = Args#changes_args.db_open_options ++ DbOptions,
    case get_or_create_db(DbName, DbOpenOptions) of
    {ok, Db} ->
        StartSeq = calculate_start_seq(Db, node(), StartVector),
        Enum = fun changes_enumerator/2,
        Opts = [{dir,Dir}],
        Acc0 = #cacc{
          db = Db,
          seq = StartSeq,
          args = Args,
          options = Options,
          pending = couch_db:count_changes_since(Db, StartSeq),
          epochs = couch_db:get_epochs(Db)
        },
        try
            {ok, #cacc{seq=LastSeq, pending=Pending, epochs=Epochs}} =
                couch_db:fold_changes(Db, StartSeq, Enum, Acc0, Opts),
            rexi:stream_last({complete, [
                {seq, {LastSeq, uuid(Db), couch_db:owner_of(Epochs, LastSeq)}},
                {pending, Pending}
            ]})
        after
            couch_db:close(Db)
        end;
    Error ->
        rexi:stream_last(Error)
    end.

all_docs(DbName, Options, Args0) ->
    case fabric_util:upgrade_mrargs(Args0) of
        #mrargs{keys=undefined} = Args1 ->
            set_io_priority(DbName, Options),
            Args = fix_skip_and_limit(Args1),
            {ok, Db} = get_or_create_db(DbName, Options),
            VAcc0 = #vacc{db=Db},
            couch_mrview:query_all_docs(Db, Args, fun view_cb/2, VAcc0)
    end.

update_mrview(DbName, {DDocId, Rev}, ViewName, Args0) ->
    {ok, DDoc} = ddoc_cache:open_doc(mem3:dbname(DbName), DDocId, Rev),
    couch_util:with_db(DbName, fun(Db) ->
        UpdateSeq = couch_db:get_update_seq(Db),
        {ok, Pid, _} = couch_mrview:get_view_index_pid(
            Db, DDoc, ViewName, fabric_util:upgrade_mrargs(Args0)),
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
    Args = fix_skip_and_limit(fabric_util:upgrade_mrargs(Args0)),
    {ok, Db} = get_or_create_db(DbName, DbOptions),
    VAcc0 = #vacc{db=Db},
    couch_mrview:query_view(Db, DDoc, ViewName, Args, fun view_cb/2, VAcc0).

%% @equiv reduce_view(DbName, DDoc, ViewName, Args0)
reduce_view(DbName, DDocInfo, ViewName, Args0) ->
    reduce_view(DbName, DDocInfo, ViewName, Args0, []).

reduce_view(DbName, {DDocId, Rev}, ViewName, Args0, DbOptions) ->
    {ok, DDoc} = ddoc_cache:open_doc(mem3:dbname(DbName), DDocId, Rev),
    reduce_view(DbName, DDoc, ViewName, Args0, DbOptions);
reduce_view(DbName, DDoc, ViewName, Args0, DbOptions) ->
    set_io_priority(DbName, DbOptions),
    Args = fix_skip_and_limit(fabric_util:upgrade_mrargs(Args0)),
    {ok, Db} = get_or_create_db(DbName, DbOptions),
    VAcc0 = #vacc{db=Db},
    couch_mrview:query_view(Db, DDoc, ViewName, Args, fun reduce_cb/2, VAcc0).

fix_skip_and_limit(Args) ->
    #mrargs{skip=Skip, limit=Limit}=Args,
    Args#mrargs{skip=0, limit=Skip+Limit}.

create_db(DbName) ->
    create_db(DbName, []).

create_db(DbName, Options) ->
    rexi:reply(case couch_server:create(DbName, Options) of
    {ok, _} ->
        ok;
    Error ->
        Error
    end).

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

%% equiv get_doc_count(DbName, [])
get_doc_count(DbName) ->
    get_doc_count(DbName, []).

get_doc_count(DbName, DbOptions) ->
    with_db(DbName, DbOptions, {couch_db, get_doc_count, []}).

%% equiv get_update_seq(DbName, [])
get_update_seq(DbName) ->
    get_update_seq(DbName, []).

get_update_seq(DbName, DbOptions) ->
    with_db(DbName, DbOptions, {couch_db, get_update_seq, []}).

set_security(DbName, SecObj, Options0) ->
    Options = case lists:keyfind(io_priority, 1, Options0) of
        false ->
            [{io_priority, {db_meta, security}}|Options0];
        _ ->
            Options0
    end,
    with_db(DbName, Options, {couch_db, set_security, [SecObj]}).

get_all_security(DbName, Options) ->
    with_db(DbName, Options, {couch_db, get_security, []}).

set_revs_limit(DbName, Limit, Options) ->
    with_db(DbName, Options, {couch_db, set_revs_limit, [Limit]}).

open_doc(DbName, DocId, Options) ->
    with_db(DbName, Options, {couch_db, open_doc, [DocId, Options]}).

open_revs(DbName, Id, Revs, Options) ->
    with_db(DbName, Options, {couch_db, open_doc_revs, [Id, Revs, Options]}).

get_full_doc_info(DbName, DocId, Options) ->
    with_db(DbName, Options, {couch_db, get_full_doc_info, [DocId]}).

get_doc_info(DbName, DocId, Options) ->
    with_db(DbName, Options, {couch_db, get_doc_info, [DocId]}).

get_missing_revs(DbName, IdRevsList) ->
    get_missing_revs(DbName, IdRevsList, []).

get_missing_revs(DbName, IdRevsList, Options) ->
    % reimplement here so we get [] for Ids with no missing revs in response
    set_io_priority(DbName, Options),
    rexi:reply(case get_or_create_db(DbName, Options) of
    {ok, Db} ->
        Ids = [Id1 || {Id1, _Revs} <- IdRevsList],
        {ok, lists:zipwith(fun({Id, Revs}, FullDocInfoResult) ->
            case FullDocInfoResult of
            #full_doc_info{rev_tree=RevisionTree} = FullInfo ->
                MissingRevs = couch_key_tree:find_missing(RevisionTree, Revs),
                {Id, MissingRevs, possible_ancestors(FullInfo, MissingRevs)};
            not_found ->
                {Id, Revs, []}
            end
        end, IdRevsList, couch_db:get_full_doc_infos(Db, Ids))};
    Error ->
        Error
    end).

update_docs(DbName, Docs0, Options) ->
    case proplists:get_value(replicated_changes, Options) of
    true ->
        X = replicated_changes;
    _ ->
        X = interactive_edit
    end,
    Docs = make_att_readers(Docs0),
    with_db(DbName, Options, {couch_db, update_docs, [Docs, Options, X]}).

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
        rexi:reply(couch_db:open(Name, Opts))
    catch exit:{timeout, _} ->
        couch_stats:increment_counter([fabric, open_shard, timeouts])
    end.

compact(DbName) ->
    with_db(DbName, [], {couch_db, start_compact, []}).

compact(ShardName, DesignName) ->
    {ok, Pid} = couch_index_server:get_index(
        couch_mrview_index, ShardName, <<"_design/", DesignName/binary>>),
    Ref = erlang:make_ref(),
    Pid ! {'$gen_call', {self(), Ref}, compact}.

%%
%% internal
%%

with_db(DbName, Options, {M,F,A}) ->
    set_io_priority(DbName, Options),
    case get_or_create_db(DbName, Options) of
    {ok, Db} ->
        rexi:reply(try
            apply(M, F, [Db | A])
        catch Exception ->
            Exception;
        error:Reason ->
            couch_log:error("rpc ~p:~p/~p ~p ~p", [M, F, length(A)+1, Reason,
                clean_stack()]),
            {error, Reason}
        end);
    Error ->
        rexi:reply(Error)
    end.

get_or_create_db(DbName, Options) ->
    couch_db:open_int(DbName, [{create_if_missing, true} | Options]).


view_cb({meta, Meta}, Acc) ->
    % Map function starting
    ok = rexi:stream2({meta, Meta}),
    {ok, Acc};
view_cb({row, Row}, Acc) ->
    % Adding another row
    ViewRow = #view_row{
        id = couch_util:get_value(id, Row),
        key = couch_util:get_value(key, Row),
        value = couch_util:get_value(value, Row),
        doc = couch_util:get_value(doc, Row)
    },
    ok = rexi:stream2(ViewRow),
    {ok, Acc};
view_cb(complete, Acc) ->
    % Finish view output
    ok = rexi:stream_last(complete),
    {ok, Acc}.


reduce_cb({meta, Meta}, Acc) ->
    % Map function starting
    ok = rexi:stream2({meta, Meta}),
    {ok, Acc};
reduce_cb({row, Row}, Acc) ->
    % Adding another row
    ok = rexi:stream2(#view_row{
        key = couch_util:get_value(key, Row),
        value = couch_util:get_value(value, Row)
    }),
    {ok, Acc};
reduce_cb(complete, Acc) ->
    % Finish view output
    ok = rexi:stream_last(complete),
    {ok, Acc}.


changes_enumerator(#full_doc_info{} = FDI, Acc) ->
    changes_enumerator(couch_doc:to_doc_info(FDI), Acc);
changes_enumerator(#doc_info{id= <<"_local/", _/binary>>, high_seq=Seq}, Acc) ->
    {ok, Acc#cacc{seq = Seq, pending = Acc#cacc.pending-1}};
changes_enumerator(DocInfo, Acc) ->
    #cacc{
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
    #doc_info{id=Id, high_seq=Seq, revs=[#rev_info{deleted=Del}|_]} = DocInfo,
    case [X || X <- couch_changes:filter(Db, DocInfo, Filter), X /= null] of
    [] ->
        ChangesRow = {no_pass, [
            {pending, Pending-1},
            {seq, Seq}]};
    Results ->
        Opts = if Conflicts -> [conflicts | DocOptions]; true -> DocOptions end,
        ChangesRow = {change, [
	    {pending, Pending-1},
            {seq, {Seq, uuid(Db), couch_db:owner_of(Epochs, Seq)}},
            {id, Id},
            {changes, Results},
            {deleted, Del} |
            if IncludeDocs -> [doc_member(Db, DocInfo, Opts, Filter)]; true -> [] end
        ]}
    end,
    ok = rexi:stream2(ChangesRow),
    {ok, Acc#cacc{seq = Seq, pending = Pending-1}}.

doc_member(Shard, DocInfo, Opts, Filter) ->
    case couch_db:open_doc(Shard, DocInfo, [deleted | Opts]) of
    {ok, Doc} ->
        {doc, maybe_filtered_json_doc(Doc, Opts, Filter)};
    Error ->
        Error
    end.

maybe_filtered_json_doc(Doc, Opts, {selector, _Style, {_Selector, Fields}})
    when Fields =/= nil ->
    mango_fields:extract(couch_doc:to_json_obj(Doc, Opts), Fields);
maybe_filtered_json_doc(Doc, Opts, _Filter) ->
    couch_doc:to_json_obj(Doc, Opts).


possible_ancestors(_FullInfo, []) ->
    [];
possible_ancestors(FullInfo, MissingRevs) ->
    #doc_info{revs=RevsInfo} = couch_doc:to_doc_info(FullInfo),
    LeafRevs = [Rev || #rev_info{rev=Rev} <- RevsInfo],
    % Find the revs that are possible parents of this rev
    lists:foldl(fun({LeafPos, LeafRevId}, Acc) ->
        % this leaf is a "possible ancenstor" of the missing
        % revs if this LeafPos lessthan any of the missing revs
        case lists:any(fun({MissingPos, _}) ->
                LeafPos < MissingPos end, MissingRevs) of
        true ->
            [{LeafPos, LeafRevId} | Acc];
        false ->
            Acc
        end
    end, [], LeafRevs).

make_att_readers([]) ->
    [];
make_att_readers([#doc{atts=Atts0} = Doc | Rest]) ->
    % % go through the attachments looking for 'follows' in the data,
    % % replace with function that reads the data from MIME stream.
    Atts = [couch_att:transform(data, fun make_att_reader/1, Att) || Att <- Atts0],
    [Doc#doc{atts = Atts} | make_att_readers(Rest)].

make_att_reader({follows, Parser, Ref}) ->
    fun() ->
        ParserRef = case get(mp_parser_ref) of
            undefined ->
                PRef = erlang:monitor(process, Parser),
                put(mp_parser_ref, PRef),
                PRef;
            Else ->
                Else
        end,
        Parser ! {get_bytes, Ref, self()},
        receive
            {bytes, Ref, Bytes} ->
                rexi:reply(attachment_chunk_received),
                Bytes;
            {'DOWN', ParserRef, _, _, Reason} ->
                throw({mp_parser_died, Reason})
        end
    end;
make_att_reader(Else) ->
    Else.

clean_stack() ->
    lists:map(fun({M,F,A}) when is_list(A) -> {M,F,length(A)}; (X) -> X end,
        erlang:get_stacktrace()).

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
    binary:part(Uuid, {0, uuid_prefix_len()}).

uuid_prefix_len() ->
    list_to_integer(config:get("fabric", "uuid_prefix_len", "7")).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

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

-endif.
