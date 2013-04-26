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
-export([all_docs/3, changes/3, map_view/4, reduce_view/4, group_info/2]).
-export([create_db/1, delete_db/1, reset_validation_funs/1, set_security/3,
    set_revs_limit/3, create_shard_db_doc/2, delete_shard_db_doc/2]).
-export([get_all_security/2]).

-include_lib("fabric/include/fabric.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").

%% rpc endpoints
%%  call to with_db will supply your M:F with a #db{} and then remaining args

changes(DbName, #changes_args{} = Args, StartSeq) ->
    changes(DbName, [Args], StartSeq);
changes(DbName, Options, StartSeq) ->
    erlang:put(io_priority, {interactive, DbName}),
    #changes_args{dir=Dir} = Args = lists:keyfind(changes_args, 1, Options),
    case get_or_create_db(DbName, []) of
    {ok, Db} ->
        Enum = fun changes_enumerator/2,
        Opts = [{dir,Dir}],
        Acc0 = {Db, StartSeq, Args, Options},
        try
            {ok, {_, LastSeq, _, _}} =
                couch_db:changes_since(Db, StartSeq, Enum, Opts, Acc0),
            rexi:reply({complete, LastSeq})
        after
            couch_db:close(Db)
        end;
    Error ->
        rexi:reply(Error)
    end.

all_docs(DbName, Options, #mrargs{keys=undefined} = Args0) ->
    Args = fix_skip_and_limit(Args0),
    {ok, Db} = get_or_create_db(DbName, Options),
    VAcc0 = #vacc{db=Db},
    couch_mrview:query_all_docs(Db, Args, fun view_cb/2, VAcc0).

map_view(DbName, DDoc, ViewName, Args0) ->
    Args = fix_skip_and_limit(Args0),
    {ok, Db} = get_or_create_db(DbName, []),
    VAcc0 = #vacc{db=Db},
    couch_mrview:query_view(Db, DDoc, ViewName, Args, fun view_cb/2, VAcc0).

reduce_view(DbName, DDoc, ViewName, Args0) ->
    Args = fix_skip_and_limit(Args0),
    {ok, Db} = get_or_create_db(DbName, []),
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

get_db_info(DbName) ->
    with_db(DbName, [], {couch_db, get_db_info, []}).

get_doc_count(DbName) ->
    with_db(DbName, [], {couch_db, get_doc_count, []}).

get_update_seq(DbName) ->
    with_db(DbName, [], {couch_db, get_update_seq, []}).

set_security(DbName, SecObj, Options) ->
    with_db(DbName, Options, {couch_db, set_security, [SecObj]}).

get_all_security(DbName, Options) ->
    with_db(DbName, Options, {couch_db, get_security, []}).

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

group_info(DbName, DDocId) ->
    with_db(DbName, [], {couch_mrview, get_info, [DDocId]}).

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
    case rexi:sync_reply({meta, Meta}) of
        ok ->
            {ok, Acc};
        stop ->
            exit(normal);
        timeout ->
            exit(timeout)
    end;
view_cb({row, Row}, Acc) ->
    % Adding another row
    ViewRow = #view_row{
        id = couch_util:get_value(id, Row),
        key = couch_util:get_value(key, Row),
        value = couch_util:get_value(value, Row),
        doc = couch_util:get_value(doc, Row)
    },
    case rexi:stream(ViewRow) of
        ok ->
            {ok, Acc};
        timeout ->
            exit(timeout)
    end;
view_cb(complete, Acc) ->
    % Finish view output
    rexi:reply(complete),
    {ok, Acc}.


reduce_cb({meta, Meta}, Acc) ->
    % Map function starting
    case rexi:sync_reply({meta, Meta}) of
        ok ->
            {ok, Acc};
        stop ->
            exit(normal);
        timeout ->
            exit(timeout)
    end;
reduce_cb({row, Row}, Acc) ->
    % Adding another row
    Key = couch_util:get_value(key, Row),
    Value = couch_util:get_value(value, Row),
    send(Key, Value, Acc);
reduce_cb(complete, Acc) ->
    % Finish view output
    rexi:reply(complete),
    {ok, Acc}.


send(Key, Value, Acc) ->
    case put(fabric_sent_first_row, true) of
    undefined ->
        case rexi:sync_reply(#view_row{key=Key, value=Value}) of
        ok ->
            {ok, Acc};
        stop ->
            exit(normal);
        timeout ->
            exit(timeout)
        end;
    true ->
        case rexi:stream(#view_row{key=Key, value=Value}) of
        ok ->
            {ok, Acc};
        timeout ->
            exit(timeout)
        end
    end.

changes_enumerator(#doc_info{id= <<"_local/", _/binary>>, high_seq=Seq},
        {Db, _OldSeq, Args, Options}) ->
    {ok, {Db, Seq, Args, Options}};
changes_enumerator(DocInfo, {Db, _Seq, Args, Options}) ->
    #changes_args{
        include_docs = IncludeDocs,
        filter = Acc
    } = Args,
    Conflicts = proplists:get_value(conflicts, Options, false),
    #doc_info{high_seq=Seq, revs=[#rev_info{deleted=Del}|_]} = DocInfo,
    case [X || X <- couch_changes:filter(Db, DocInfo, Acc), X /= null] of
    [] ->
        {ok, {Db, Seq, Args, Options}};
    Results ->
        Opts = if Conflicts -> [conflicts]; true -> [] end,
        ChangesRow = changes_row(Db, DocInfo, Results, Del, IncludeDocs, Opts),
        Go = rexi:sync_reply(ChangesRow),
        {Go, {Db, Seq, Args, Options}}
    end.

changes_row(Db, #doc_info{id=Id, high_seq=Seq}=DI, Results, Del, true, Opts) ->
    Doc = doc_member(Db, DI, Opts),
    #change{key=Seq, id=Id, value=Results, doc=Doc, deleted=Del};
changes_row(_, #doc_info{id=Id, high_seq=Seq}, Results, true, _, _) ->
    #change{key=Seq, id=Id, value=Results, deleted=true};
changes_row(_, #doc_info{id=Id, high_seq=Seq}, Results, _, _, _) ->
    #change{key=Seq, id=Id, value=Results}.

doc_member(Shard, DocInfo, Opts) ->
    case couch_db:open_doc(Shard, DocInfo, [deleted | Opts]) of
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
            case config:get("cloudant", "non_interactive_mode", "false") of
                "true" ->
                    % Done to silence error logging by rexi_server
                    rexi:reply({rexi_EXIT, {non_interactive_mode, node()}}),
                    exit(normal);
                _ ->
                    ok
            end;
        _ ->
            ok
    end.
