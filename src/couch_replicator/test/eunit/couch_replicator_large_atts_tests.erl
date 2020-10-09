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

-module(couch_replicator_large_atts_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-import(couch_replicator_test_helper, [
    db_url/1,
    replicate/2,
    compare_dbs/2
]).

-define(ATT_SIZE_1, 2 * 1024 * 1024).
-define(ATT_SIZE_2, round(6.6 * 1024 * 1024)).
-define(DOCS_COUNT, 11).
-define(TIMEOUT_EUNIT, 120).


setup() ->
    DbName = ?tempdb(),
    {ok, Db} = couch_db:create(DbName, [?ADMIN_CTX]),
    ok = couch_db:close(Db),
    DbName.

setup(remote) ->
    {remote, setup()};
setup({A, B}) ->
    Ctx = test_util:start_couch([couch_replicator]),
    config:set("attachments", "compressible_types", "text/*", false),
    Source = setup(A),
    Target = setup(B),
    {Ctx, {Source, Target}}.

teardown({remote, DbName}) ->
    teardown(DbName);
teardown(DbName) ->
    ok = couch_server:delete(DbName, [?ADMIN_CTX]),
    ok.

teardown(_, {Ctx, {Source, Target}}) ->
    teardown(Source),
    teardown(Target),

    ok = application:stop(couch_replicator),
    ok = test_util:stop_couch(Ctx).

large_atts_test_() ->
    Pairs = [{remote, remote}],
    {
        "Replicate docs with large attachments",
        {
            foreachx,
            fun setup/1, fun teardown/2,
            [{Pair, fun should_populate_replicate_compact/2}
             || Pair <- Pairs]
        }
    }.


should_populate_replicate_compact({From, To}, {_Ctx, {Source, Target}}) ->
    {lists:flatten(io_lib:format("~p -> ~p", [From, To])),
     {inorder, [should_populate_source(Source),
                should_replicate(Source, Target),
                should_compare_databases(Source, Target)]}}.

should_populate_source({remote, Source}) ->
    should_populate_source(Source);
should_populate_source(Source) ->
    {timeout, ?TIMEOUT_EUNIT, ?_test(populate_db(Source, ?DOCS_COUNT))}.

should_replicate({remote, Source}, Target) ->
    should_replicate(db_url(Source), Target);
should_replicate(Source, {remote, Target}) ->
    should_replicate(Source, db_url(Target));
should_replicate(Source, Target) ->
    {timeout, ?TIMEOUT_EUNIT, ?_test(replicate(Source, Target))}.

should_compare_databases({remote, Source}, Target) ->
    should_compare_databases(Source, Target);
should_compare_databases(Source, {remote, Target}) ->
    should_compare_databases(Source, Target);
should_compare_databases(Source, Target) ->
    {timeout, ?TIMEOUT_EUNIT, ?_test(compare_dbs(Source, Target))}.


populate_db(DbName, DocCount) ->
    {ok, Db} = couch_db:open_int(DbName, []),
    Docs = lists:foldl(
        fun(DocIdCounter, Acc) ->
            Doc = #doc{
                id = iolist_to_binary(["doc", integer_to_list(DocIdCounter)]),
                body = {[]},
                atts = [
                    att(<<"att1">>, ?ATT_SIZE_1, <<"text/plain">>),
                    att(<<"att2">>, ?ATT_SIZE_2, <<"app/binary">>)
                ]
            },
            [Doc | Acc]
        end,
        [], lists:seq(1, DocCount)),
    {ok, _} = couch_db:update_docs(Db, Docs, []),
    couch_db:close(Db).

att(Name, Size, Type) ->
    couch_att:new([
        {name, Name},
        {type, Type},
        {att_len, Size},
        {data, fun(Count) -> crypto:strong_rand_bytes(Count) end}
    ]).
