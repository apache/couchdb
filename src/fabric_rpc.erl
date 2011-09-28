% Copyright 2010 Cloudant
%
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
-export([open_doc/3, open_revs/4, get_missing_revs/2, get_missing_revs/3,
    update_docs/3]).
-export([all_docs/2, changes/3, map_view/4, reduce_view/4, group_info/2]).
-export([create_db/1, delete_db/1, reset_validation_funs/1, set_security/3,
    set_revs_limit/3, create_shard_db_doc/2, delete_shard_db_doc/2]).

-include("fabric.hrl").
-include_lib("couch/include/couch_db.hrl").

-record (view_acc, {
    db,
    limit,
    include_docs,
    doc_info = nil,
    offset = nil,
    total_rows,
    reduce_fun = fun couch_db:enum_docs_reduce_to_count/1,
    group_level = 0
}).

%% rpc endpoints
%%  call to with_db will supply your M:F with a #db{} and then remaining args

all_docs(DbName, #view_query_args{keys=nil} = QueryArgs) ->
    {ok, Db} = get_or_create_db(DbName, []),
    #view_query_args{
        start_key = StartKey,
        start_docid = StartDocId,
        end_key = EndKey,
        end_docid = EndDocId,
        limit = Limit,
        skip = Skip,
        include_docs = IncludeDocs,
        direction = Dir,
        inclusive_end = Inclusive,
        extra = Extra
    } = QueryArgs,
    set_io_priority(DbName, Extra),
    {ok, Total} = couch_db:get_doc_count(Db),
    Acc0 = #view_acc{
        db = Db,
        include_docs = IncludeDocs,
        limit = Limit+Skip,
        total_rows = Total
    },
    EndKeyType = if Inclusive -> end_key; true -> end_key_gt end,
    Options = [
        {dir, Dir},
        {start_key, if is_binary(StartKey) -> StartKey; true -> StartDocId end},
        {EndKeyType, if is_binary(EndKey) -> EndKey; true -> EndDocId end}
    ],
    {ok, _, Acc} = couch_db:enum_docs(Db, fun view_fold/3, Acc0, Options),
    final_response(Total, Acc#view_acc.offset).

changes(DbName, Args, StartSeq) ->
    erlang:put(io_priority, {interactive, DbName}),
    #changes_args{dir=Dir} = Args,
    case get_or_create_db(DbName, []) of
    {ok, Db} ->
        Enum = fun changes_enumerator/2,
        Opts = [{dir,Dir}],
        Acc0 = {Db, StartSeq, Args},
        try
            {ok, {_, LastSeq, _}} =
                couch_db:changes_since(Db, StartSeq, Enum, Opts, Acc0),
            rexi:reply({complete, LastSeq})
        after
            couch_db:close(Db)
        end;
    Error ->
        rexi:reply(Error)
    end.

map_view(DbName, DDoc, ViewName, QueryArgs) ->
    {ok, Db} = get_or_create_db(DbName, []),
    #view_query_args{
        limit = Limit,
        skip = Skip,
        keys = Keys,
        include_docs = IncludeDocs,
        stale = Stale,
        view_type = ViewType,
        extra = Extra
    } = QueryArgs,
    set_io_priority(DbName, Extra),
    {LastSeq, MinSeq} = calculate_seqs(Db, Stale),
    Group0 = couch_view_group:design_doc_to_view_group(DDoc),
    {ok, Pid} = gen_server:call(couch_view, {get_group_server, DbName, Group0}),
    {ok, Group} = couch_view_group:request_group(Pid, MinSeq),
    maybe_update_view_group(Pid, LastSeq, Stale),
    erlang:monitor(process, Group#group.fd),
    View = fabric_view:extract_view(Pid, ViewName, Group#group.views, ViewType),
    {ok, Total} = couch_view:get_row_count(View),
    Acc0 = #view_acc{
        db = Db,
        include_docs = IncludeDocs,
        limit = Limit+Skip,
        total_rows = Total,
        reduce_fun = fun couch_view:reduce_to_count/1
    },
    case Keys of
    nil ->
        Options = couch_httpd_view:make_key_options(QueryArgs),
        {ok, _, Acc} = couch_view:fold(View, fun view_fold/3, Acc0, Options);
    _ ->
        Acc = lists:foldl(fun(Key, AccIn) ->
            KeyArgs = QueryArgs#view_query_args{start_key=Key, end_key=Key},
            Options = couch_httpd_view:make_key_options(KeyArgs),
            {_Go, _, Out} = couch_view:fold(View, fun view_fold/3, AccIn,
                Options),
            Out
        end, Acc0, Keys)
    end,
    final_response(Total, Acc#view_acc.offset).

reduce_view(DbName, Group0, ViewName, QueryArgs) ->
    erlang:put(io_priority, {interactive, DbName}),
    {ok, Db} = get_or_create_db(DbName, []),
    #view_query_args{
        group_level = GroupLevel,
        limit = Limit,
        skip = Skip,
        keys = Keys,
        stale = Stale,
        extra = Extra
    } = QueryArgs,
    set_io_priority(DbName, Extra),
    GroupFun = group_rows_fun(GroupLevel),
    {LastSeq, MinSeq} = calculate_seqs(Db, Stale),
    {ok, Pid} = gen_server:call(couch_view, {get_group_server, DbName, Group0}),
    {ok, Group} = couch_view_group:request_group(Pid, MinSeq),
    maybe_update_view_group(Pid, LastSeq, Stale),
    #group{views=Views, def_lang=Lang, fd=Fd} = Group,
    erlang:monitor(process, Fd),
    {NthRed, View} = fabric_view:extract_view(Pid, ViewName, Views, reduce),
    ReduceView = {reduce, NthRed, Lang, View},
    Acc0 = #view_acc{group_level = GroupLevel, limit = Limit+Skip},
    case Keys of
    nil ->
        Options0 = couch_httpd_view:make_key_options(QueryArgs),
        Options = [{key_group_fun, GroupFun} | Options0],
        couch_view:fold_reduce(ReduceView, fun reduce_fold/3, Acc0, Options);
    _ ->
        lists:map(fun(Key) ->
            KeyArgs = QueryArgs#view_query_args{start_key=Key, end_key=Key},
            Options0 = couch_httpd_view:make_key_options(KeyArgs),
            Options = [{key_group_fun, GroupFun} | Options0],
            couch_view:fold_reduce(ReduceView, fun reduce_fold/3, Acc0, Options)
        end, Keys)
    end,
    rexi:reply(complete).

calculate_seqs(Db, Stale) ->
    LastSeq = couch_db:get_update_seq(Db),
    if
        Stale == ok orelse Stale == update_after ->
            {LastSeq, 0};
        true ->
            {LastSeq, LastSeq}
    end.

maybe_update_view_group(GroupPid, LastSeq, update_after) ->
    couch_view_group:trigger_group_update(GroupPid, LastSeq);
maybe_update_view_group(_, _, _) ->
    ok.

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

get_db_info(DbName) ->
    with_db(DbName, [], {couch_db, get_db_info, []}).

get_doc_count(DbName) ->
    with_db(DbName, [], {couch_db, get_doc_count, []}).

get_update_seq(DbName) ->
    with_db(DbName, [], {couch_db, get_update_seq, []}).

set_security(DbName, SecObj, Options) ->
    with_db(DbName, Options, {couch_db, set_security, [SecObj]}).

set_revs_limit(DbName, Limit, Options) ->
    with_db(DbName, Options, {couch_db, set_revs_limit, [Limit]}).

open_doc(DbName, DocId, Options) ->
    with_db(DbName, Options, {couch_db, open_doc, [DocId, Options]}).

open_revs(DbName, Id, Revs, Options) ->
    with_db(DbName, Options, {couch_db, open_doc_revs, [Id, Revs, Options]}).

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

group_info(DbName, Group0) ->
    {ok, Pid} = gen_server:call(couch_view, {get_group_server, DbName, Group0}),
    rexi:reply(couch_view_group:request_group_info(Pid)).

reset_validation_funs(DbName) ->
    case get_or_create_db(DbName, []) of
    {ok, #db{main_pid = Pid}} ->
        gen_server:cast(Pid, {load_validation_funs, undefined});
    _ ->
        ok
    end.

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
            twig:log(error, "rpc ~p:~p/~p ~p ~p", [M, F, length(A)+1, Reason,
                clean_stack()]),
            {error, Reason}
        end);
    Error ->
        rexi:reply(Error)
    end.

get_or_create_db(DbName, Options) ->
    case couch_db:open_int(DbName, Options) of
    {not_found, no_db_file} ->
        twig:log(warn, "~p creating ~s", [?MODULE, DbName]),
        couch_server:create(DbName, Options);
    Else ->
        Else
    end.

view_fold(#full_doc_info{} = FullDocInfo, OffsetReds, Acc) ->
    % matches for _all_docs and translates #full_doc_info{} -> KV pair
    case couch_doc:to_doc_info(FullDocInfo) of
    #doc_info{id=Id, revs=[#rev_info{deleted=false, rev=Rev}|_]} = DI ->
        Value = {[{rev,couch_doc:rev_to_str(Rev)}]},
        view_fold({{Id,Id}, Value}, OffsetReds, Acc#view_acc{doc_info=DI});
    #doc_info{revs=[#rev_info{deleted=true}|_]} ->
        {ok, Acc}
    end;
view_fold(KV, OffsetReds, #view_acc{offset=nil, total_rows=Total} = Acc) ->
    % calculates the offset for this shard
    #view_acc{reduce_fun=Reduce} = Acc,
    Offset = Reduce(OffsetReds),
    case rexi:sync_reply({total_and_offset, Total, Offset}) of
    ok ->
        view_fold(KV, OffsetReds, Acc#view_acc{offset=Offset});
    stop ->
        exit(normal);
    timeout ->
        exit(timeout)
    end;
view_fold(_KV, _Offset, #view_acc{limit=0} = Acc) ->
    % we scanned through limit+skip local rows
    {stop, Acc};
view_fold({{Key,Id}, Value}, _Offset, Acc) ->
    % the normal case
    #view_acc{
        db = Db,
        doc_info = DocInfo,
        limit = Limit,
        include_docs = IncludeDocs
    } = Acc,
    case Value of {Props} ->
        LinkedDocs = (couch_util:get_value(<<"_id">>, Props) =/= undefined);
    _ ->
        LinkedDocs = false
    end,
    if LinkedDocs ->
        % we'll embed this at a higher level b/c the doc may be non-local
        Doc = undefined;
    IncludeDocs ->
        IdOrInfo = if DocInfo =/= nil -> DocInfo; true -> Id end,
        case couch_db:open_doc(Db, IdOrInfo, []) of
        {not_found, deleted} ->
            Doc = null;
        {not_found, missing} ->
            Doc = undefined;
        {ok, Doc0} ->
            Doc = couch_doc:to_json_obj(Doc0, [])
        end;
    true ->
        Doc = undefined
    end,
    case rexi:sync_reply(#view_row{key=Key, id=Id, value=Value, doc=Doc}) of
        ok ->
            {ok, Acc#view_acc{limit=Limit-1}};
        timeout ->
            exit(timeout)
    end.

final_response(Total, nil) ->
    case rexi:sync_reply({total_and_offset, Total, Total}) of ok ->
        rexi:reply(complete);
    stop ->
        ok;
    timeout ->
        exit(timeout)
    end;
final_response(_Total, _Offset) ->
    rexi:reply(complete).

%% TODO: handle case of bogus group level
group_rows_fun(exact) ->
    fun({Key1,_}, {Key2,_}) -> Key1 == Key2 end;
group_rows_fun(0) ->
    fun(_A, _B) -> true end;
group_rows_fun(GroupLevel) when is_integer(GroupLevel) ->
    fun({[_|_] = Key1,_}, {[_|_] = Key2,_}) ->
        lists:sublist(Key1, GroupLevel) == lists:sublist(Key2, GroupLevel);
    ({Key1,_}, {Key2,_}) ->
        Key1 == Key2
    end.

reduce_fold(_Key, _Red, #view_acc{limit=0} = Acc) ->
    {stop, Acc};
reduce_fold(_Key, Red, #view_acc{group_level=0} = Acc) ->
    send(null, Red, Acc);
reduce_fold(Key, Red, #view_acc{group_level=exact} = Acc) ->
    send(Key, Red, Acc);
reduce_fold(K, Red, #view_acc{group_level=I} = Acc) when I > 0, is_list(K) ->
    send(lists:sublist(K, I), Red, Acc);
reduce_fold(K, Red, #view_acc{group_level=I} = Acc) when I > 0 ->
    send(K, Red, Acc).


send(Key, Value, #view_acc{limit=Limit} = Acc) ->
    case rexi:sync_reply(#view_row{key=Key, value=Value}) of
    ok ->
        {ok, Acc#view_acc{limit=Limit-1}};
    stop ->
        exit(normal);
    timeout ->
        exit(timeout)
    end.

changes_enumerator(DocInfo, {Db, _Seq, Args}) ->
    #changes_args{include_docs=IncludeDocs, filter=Acc} = Args,
    #doc_info{high_seq=Seq, revs=[#rev_info{deleted=Del}|_]} = DocInfo,
    case [X || X <- couch_changes:filter(DocInfo, Acc), X /= null] of
    [] ->
        {ok, {Db, Seq, Args}};
    Results ->
        ChangesRow = changes_row(Db, DocInfo, Results, Del, IncludeDocs),
        Go = rexi:sync_reply(ChangesRow),
        {Go, {Db, Seq, Args}}
    end.

changes_row(Db, #doc_info{id=Id, high_seq=Seq} = DI, Results, Del, true) ->
    #change{key=Seq, id=Id, value=Results, doc=doc_member(Db,DI), deleted=Del};
changes_row(_, #doc_info{id=Id, high_seq=Seq}, Results, true, _) ->
    #change{key=Seq, id=Id, value=Results, deleted=true};
changes_row(_, #doc_info{id=Id, high_seq=Seq}, Results, _, _) ->
    #change{key=Seq, id=Id, value=Results}.

doc_member(Shard, DocInfo) ->
    case couch_db:open_doc(Shard, DocInfo, [deleted]) of
    {ok, Doc} ->
        couch_doc:to_json_obj(Doc, []);
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
    Atts = [Att#att{data=make_att_reader(D)} || #att{data=D} = Att <- Atts0],
    [Doc#doc{atts = Atts} | make_att_readers(Rest)].

make_att_reader({follows, Parser}) ->
    fun() ->
        Parser ! {get_bytes, self()},
        receive {bytes, Bytes} -> Bytes end
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
    end.
