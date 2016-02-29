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
-export([create_db/1, delete_db/1, reset_validation_funs/1, set_security/3,
    set_revs_limit/3, create_shard_db_doc/2, delete_shard_db_doc/2]).
-export([get_all_security/2, open_shard/2]).

-export([get_db_info/2, get_doc_count/2, get_update_seq/2,
         changes/4, map_view/5, reduce_view/5, group_info/3]).

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
%%  call to with_db will supply your M:F with a #db{} and then remaining args

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
          epochs = get_epochs(Db)
        },
        try
            {ok, #cacc{seq=LastSeq, pending=Pending, epochs=Epochs}} =
                couch_db:changes_since(Db, StartSeq, Enum, Opts, Acc0),
            rexi:stream_last({complete, [
                {seq, {LastSeq, uuid(Db), owner_of(LastSeq, Epochs)}},
                {pending, Pending}
            ]})
        after
            couch_db:close(Db)
        end;
    Error ->
        rexi:stream_last(Error)
    end.

all_docs(DbName, Options, #mrargs{keys=undefined} = Args0) ->
    set_io_priority(DbName, Options),
    Args = fix_skip_and_limit(Args0),
    {ok, Db} = get_or_create_db(DbName, Options),
    VAcc0 = #vacc{db=Db},
    couch_mrview:query_all_docs(Db, Args, fun view_cb/2, VAcc0).

%% @equiv map_view(DbName, DDoc, ViewName, Args0, [])
map_view(DbName, DDocInfo, ViewName, Args0) ->
    map_view(DbName, DDocInfo, ViewName, Args0, []).

map_view(DbName, {DDocId, Rev}, ViewName, Args0, DbOptions) ->
    {ok, DDoc} = ddoc_cache:open_doc(mem3:dbname(DbName), DDocId, Rev),
    map_view(DbName, DDoc, ViewName, Args0, DbOptions);
map_view(DbName, DDoc, ViewName, Args0, DbOptions) ->
    set_io_priority(DbName, DbOptions),
    Args = fix_skip_and_limit(Args0),
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
    Args = fix_skip_and_limit(Args0),
    {ok, Db} = get_or_create_db(DbName, DbOptions),
    VAcc0 = #vacc{db=Db},
    couch_mrview:query_view(Db, DDoc, ViewName, Args, fun reduce_cb/2, VAcc0).

fix_skip_and_limit(Args) ->
    #mrargs{skip=Skip, limit=Limit}=Args,
    Args#mrargs{skip=0, limit=Skip+Limit}.

create_db(DbName) ->
    rexi:reply(case couch_server:create(DbName, []) of
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
            {ok, #full_doc_info{rev_tree=RevisionTree} = FullInfo} ->
                MissingRevs = couch_key_tree:find_missing(RevisionTree, Revs),
                {Id, MissingRevs, possible_ancestors(FullInfo, MissingRevs)};
            not_found ->
                {Id, Revs, []}
            end
        end, IdRevsList, couch_btree:lookup(Db#db.id_tree, Ids))};
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
    {ok, #db{main_pid = Pid}} ->
        gen_server:cast(Pid, {load_validation_funs, undefined});
    _ ->
        ok
    end.

open_shard(Name, Opts) ->
    set_io_priority(Name, Opts),
    rexi:reply(couch_db:open(Name, Opts)).

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
    case couch_db:open_int(DbName, Options) of
    {not_found, no_db_file} ->
        couch_log:warning("~p creating ~s", [?MODULE, DbName]),
        couch_server:create(DbName, Options);
    Else ->
        Else
    end.


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
        {ok, Acc#cacc{seq = Seq, pending = Pending-1}};
    Results ->
        Opts = if Conflicts -> [conflicts | DocOptions]; true -> DocOptions end,
        ChangesRow = {change, [
	    {pending, Pending-1},
            {seq, {Seq, uuid(Db), owner_of(Seq, Epochs)}},
            {id, Id},
            {changes, Results},
            {deleted, Del} |
            if IncludeDocs -> [doc_member(Db, DocInfo, Opts)]; true -> [] end
        ]},
        ok = rexi:stream2(ChangesRow),
        {ok, Acc#cacc{seq = Seq, pending = Pending-1}}
    end.

doc_member(Shard, DocInfo, Opts) ->
    case couch_db:open_doc(Shard, DocInfo, [deleted | Opts]) of
    {ok, Doc} ->
        {doc, couch_doc:to_json_obj(Doc, Opts)};
    Error ->
        Error
    end.

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

calculate_start_seq(_Db, _Node, Seq) when is_integer(Seq) ->
    Seq;
calculate_start_seq(Db, Node, {Seq, Uuid}) ->
    % Treat the current node as the epoch node
    calculate_start_seq(Db, Node, {Seq, Uuid, Node});
calculate_start_seq(Db, _Node, {Seq, Uuid, EpochNode}) ->
    case is_prefix(Uuid, couch_db:get_uuid(Db)) of
        true ->
            case is_owner(EpochNode, Seq, couch_db:get_epochs(Db)) of
                true -> Seq;
                false -> 0
            end;
        false ->
            %% The file was rebuilt, most likely in a different
            %% order, so rewind.
            0
    end;
calculate_start_seq(Db, _Node, {replace, OriginalNode, Uuid, Seq}) ->
    case is_prefix(Uuid, couch_db:get_uuid(Db)) of
        true ->
            start_seq(get_epochs(Db), OriginalNode, Seq);
        false ->
            %% Scan history looking for an entry with
            %%  * target_node == TargetNode
            %%  * target_uuid == TargetUUID
            %%  * target_seq  =< TargetSeq
            %% If such an entry is found, stream from associated source_seq
            mem3_rep:find_source_seq(Db, OriginalNode, Uuid, Seq)
    end.

is_prefix(Pattern, Subject) ->
     binary:longest_common_prefix([Pattern, Subject]) == size(Pattern).

is_owner(Node, Seq, Epochs) ->
    validate_epochs(Epochs),
    Node =:= owner_of(Seq, Epochs).

owner_of(_Seq, []) ->
    undefined;
owner_of(Seq, [{EpochNode, EpochSeq} | _Rest]) when Seq > EpochSeq ->
    EpochNode;
owner_of(Seq, [_ | Rest]) ->
    owner_of(Seq, Rest).

get_epochs(Db) ->
    Epochs = couch_db:get_epochs(Db),
    validate_epochs(Epochs),
    Epochs.

start_seq([{OrigNode, EpochSeq} | _], OrigNode, Seq) when Seq > EpochSeq ->
    %% OrigNode is the owner of the Seq so we can safely stream from there
    Seq;
start_seq([{_, NewSeq}, {OrigNode, _} | _], OrigNode, Seq) when Seq > NewSeq ->
    %% We transferred this file before Seq was written on OrigNode, so we need
    %% to stream from the beginning of the next epoch. Note that it is _not_
    %% necessary for the current node to own the epoch beginning at NewSeq
    NewSeq;
start_seq([_ | Rest], OrigNode, Seq) ->
    start_seq(Rest, OrigNode, Seq);
start_seq([], OrigNode, Seq) ->
    erlang:error({epoch_mismatch, OrigNode, Seq}).

validate_epochs(Epochs) ->
    %% Assert uniqueness.
    case length(Epochs) == length(lists:ukeysort(2, Epochs)) of
        true  -> ok;
        false -> erlang:error(duplicate_epoch)
    end,
    %% Assert order.
    case Epochs == lists:sort(fun({_, A}, {_, B}) -> B =< A end, Epochs) of
        true  -> ok;
        false -> erlang:error(epoch_order)
    end.

uuid(Db) ->
    Uuid = couch_db:get_uuid(Db),
    binary:part(Uuid, {0, uuid_prefix_len()}).

uuid_prefix_len() ->
    list_to_integer(config:get("fabric", "uuid_prefix_len", "7")).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

calculate_start_seq_test() ->
    %% uuid mismatch is always a rewind.
    Hdr1 = couch_db_header:new(),
    Hdr2 = couch_db_header:set(Hdr1, [{epochs, [{node1, 1}]}, {uuid, <<"uuid1">>}]),
    ?assertEqual(0, calculate_start_seq(#db{header=Hdr2}, node1, {1, <<"uuid2">>})),
    %% uuid matches and seq is owned by node.
    Hdr3 = couch_db_header:set(Hdr2, [{epochs, [{node1, 1}]}]),
    ?assertEqual(2, calculate_start_seq(#db{header=Hdr3}, node1, {2, <<"uuid1">>})),
    %% uuids match but seq is not owned by node.
    Hdr4 = couch_db_header:set(Hdr2, [{epochs, [{node2, 2}, {node1, 1}]}]),
    ?assertEqual(0, calculate_start_seq(#db{header=Hdr4}, node1, {3, <<"uuid1">>})),
    %% return integer if we didn't get a vector.
    ?assertEqual(4, calculate_start_seq(#db{}, foo, 4)).

is_owner_test() ->
    ?assertNot(is_owner(foo, 1, [])),
    ?assertNot(is_owner(foo, 1, [{foo, 1}])),
    ?assert(is_owner(foo, 2, [{foo, 1}])),
    ?assert(is_owner(foo, 50, [{bar, 100}, {foo, 1}])),
    ?assert(is_owner(foo, 50, [{baz, 200}, {bar, 100}, {foo, 1}])),
    ?assert(is_owner(bar, 150, [{baz, 200}, {bar, 100}, {foo, 1}])),
    ?assertError(duplicate_epoch, is_owner(foo, 1, [{foo, 1}, {bar, 1}])),
    ?assertError(epoch_order, is_owner(foo, 1, [{foo, 100}, {bar, 200}])).

-endif.
