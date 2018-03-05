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

-module(couch_replicator_selector_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_replicator/src/couch_replicator.hrl").


setup(_) ->
    Ctx = test_util:start_couch([couch_replicator]),
    Source = create_db(),
    create_docs(Source),
    Target = create_db(),
    {Ctx, {Source, Target}}.

teardown(_, {Ctx, {Source, Target}}) ->
    delete_db(Source),
    delete_db(Target),
    ok = application:stop(couch_replicator),
    ok = test_util:stop_couch(Ctx).

selector_replication_test_() ->
    Pairs = [{local, local}, {local, remote},
             {remote, local}, {remote, remote}],
    {
        "Selector filtered replication tests",
        {
            foreachx,
            fun setup/1, fun teardown/2,
            [{Pair, fun should_succeed/2} || Pair <- Pairs]
        }
    }.

should_succeed({From, To}, {_Ctx, {Source, Target}}) ->
    RepObject = {[
        {<<"source">>, db_url(From, Source)},
        {<<"target">>, db_url(To, Target)},
        {<<"selector">>, {[{<<"_id">>, <<"doc2">>}]}}
    ]},
    {ok, _} = couch_replicator:replicate(RepObject, ?ADMIN_USER),
    %% FilteredFun is an Erlang version of following mango selector
    FilterFun = fun(_DocId, {Props}) ->
        couch_util:get_value(<<"_id">>, Props) == <<"doc2">>
    end,
    {ok, TargetDbInfo, AllReplies} = compare_dbs(Source, Target, FilterFun),
    {lists:flatten(io_lib:format("~p -> ~p", [From, To])), [
        {"Target DB has proper number of docs",
        ?_assertEqual(1, proplists:get_value(doc_count, TargetDbInfo))},
        {"All the docs selected as expected",
        ?_assert(lists:all(fun(Valid) -> Valid end, AllReplies))}
    ]}.

compare_dbs(Source, Target, FilterFun) ->
    {ok, SourceDb} = couch_db:open_int(Source, []),
    {ok, TargetDb} = couch_db:open_int(Target, []),
    {ok, TargetDbInfo} = couch_db:get_db_info(TargetDb),
    Fun = fun(FullDocInfo, Acc) ->
        {ok, DocId, SourceDoc} = read_doc(SourceDb, FullDocInfo),
        TargetReply = read_doc(TargetDb, DocId),
        case FilterFun(DocId, SourceDoc) of
            true ->
                ValidReply = {ok, DocId, SourceDoc} == TargetReply,
                {ok, [ValidReply|Acc]};
            false ->
                ValidReply = {not_found, missing} == TargetReply,
                {ok, [ValidReply|Acc]}
        end
    end,
    {ok, AllReplies} = couch_db:fold_docs(SourceDb, Fun, [], []),
    ok = couch_db:close(SourceDb),
    ok = couch_db:close(TargetDb),
    {ok, TargetDbInfo, AllReplies}.

read_doc(Db, DocIdOrInfo) ->
    case couch_db:open_doc(Db, DocIdOrInfo) of
        {ok, Doc} ->
            {Props} = couch_doc:to_json_obj(Doc, [attachments]),
            DocId = couch_util:get_value(<<"_id">>, Props),
            {ok, DocId, {Props}};
        Error ->
            Error
    end.

create_db() ->
    DbName = ?tempdb(),
    {ok, Db} = couch_db:create(DbName, [?ADMIN_CTX]),
    ok = couch_db:close(Db),
    DbName.

create_docs(DbName) ->
    {ok, Db} = couch_db:open(DbName, [?ADMIN_CTX]),
    Doc1 = couch_doc:from_json_obj({[
        {<<"_id">>, <<"doc1">>}
    ]}),
    Doc2 = couch_doc:from_json_obj({[
        {<<"_id">>, <<"doc2">>}
    ]}),
    {ok, _} = couch_db:update_docs(Db, [Doc1, Doc2]),
    couch_db:ensure_full_commit(Db),
    couch_db:close(Db).

delete_db(DbName) ->
    ok = couch_server:delete(DbName, [?ADMIN_CTX]).

db_url(local, DbName) ->
    DbName;
db_url(remote, DbName) ->
    Addr = config:get("httpd", "bind_address", "127.0.0.1"),
    Port = mochiweb_socket_server:get(couch_httpd, port),
    ?l2b(io_lib:format("http://~s:~b/~s", [Addr, Port, DbName])).
