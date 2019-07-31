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

-module(couch_replicator_missing_stubs_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-import(couch_replicator_test_helper, [
    db_url/1,
    replicate/2,
    compare_dbs/2
]).

-define(REVS_LIMIT, 3).
-define(TIMEOUT_EUNIT, 30).

setup() ->
    DbName = ?tempdb(),
    {ok, Db} = couch_db:create(DbName, [?ADMIN_CTX]),
    ok = couch_db:close(Db),
    DbName.

setup(remote) ->
    {remote, setup()};
setup({A, B}) ->
    Ctx = test_util:start_couch([couch_replicator]),
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

missing_stubs_test_() ->
    Pairs = [{remote, remote}],
    {
        "Replicate docs with missing stubs (COUCHDB-1365)",
        {
            foreachx,
            fun setup/1, fun teardown/2,
            [{Pair, fun should_replicate_docs_with_missed_att_stubs/2}
             || Pair <- Pairs]
        }
    }.


should_replicate_docs_with_missed_att_stubs({From, To}, {_Ctx, {Source, Target}}) ->
    {lists:flatten(io_lib:format("~p -> ~p", [From, To])),
     {inorder, [
        should_populate_source(Source),
        should_set_target_revs_limit(Target, ?REVS_LIMIT),
        should_replicate(Source, Target),
        should_compare_databases(Source, Target),
        should_update_source_docs(Source, ?REVS_LIMIT * 2),
        should_replicate(Source, Target),
        should_compare_databases(Source, Target)
     ]}}.

should_populate_source({remote, Source}) ->
    should_populate_source(Source);
should_populate_source(Source) ->
    {timeout, ?TIMEOUT_EUNIT, ?_test(populate_db(Source))}.

should_replicate({remote, Source}, Target) ->
    should_replicate(db_url(Source), Target);
should_replicate(Source, {remote, Target}) ->
    should_replicate(Source, db_url(Target));
should_replicate(Source, Target) ->
    {timeout, ?TIMEOUT_EUNIT, ?_test(replicate(Source, Target))}.

should_set_target_revs_limit({remote, Target}, RevsLimit) ->
    should_set_target_revs_limit(Target, RevsLimit);
should_set_target_revs_limit(Target, RevsLimit) ->
    ?_test(begin
        {ok, Db} = couch_db:open_int(Target, [?ADMIN_CTX]),
        ?assertEqual(ok, couch_db:set_revs_limit(Db, RevsLimit)),
        ok = couch_db:close(Db)
    end).

should_compare_databases({remote, Source}, Target) ->
    should_compare_databases(Source, Target);
should_compare_databases(Source, {remote, Target}) ->
    should_compare_databases(Source, Target);
should_compare_databases(Source, Target) ->
    {timeout, ?TIMEOUT_EUNIT, ?_test(compare_dbs(Source, Target))}.

should_update_source_docs({remote, Source}, Times) ->
    should_update_source_docs(Source, Times);
should_update_source_docs(Source, Times) ->
    {timeout, ?TIMEOUT_EUNIT, ?_test(update_db_docs(Source, Times))}.


populate_db(DbName) ->
    {ok, Db} = couch_db:open_int(DbName, []),
    AttData = crypto:strong_rand_bytes(6000),
    Doc = #doc{
        id = <<"doc1">>,
        atts = [
            couch_att:new([
                {name, <<"doc1_att1">>},
                {type, <<"application/foobar">>},
                {att_len, byte_size(AttData)},
                {data, AttData}
           ])
        ]
    },
    {ok, _} = couch_db:update_doc(Db, Doc, []),
    couch_db:close(Db).

update_db_docs(DbName, Times) ->
    {ok, Db} = couch_db:open_int(DbName, []),
    {ok, _} = couch_db:fold_docs(
        Db,
        fun(FDI, Acc) -> db_fold_fun(FDI, Acc) end,
        {DbName, Times},
        []),
    ok = couch_db:close(Db).

db_fold_fun(FullDocInfo, {DbName, Times}) ->
    {ok, Db} = couch_db:open_int(DbName, []),
    {ok, Doc} = couch_db:open_doc(Db, FullDocInfo),
    lists:foldl(
        fun(_, {Pos, RevId}) ->
            {ok, Db2} = couch_db:reopen(Db),
            NewDocVersion = Doc#doc{
                revs = {Pos, [RevId]},
                body = {[{<<"value">>, base64:encode(crypto:strong_rand_bytes(100))}]}
            },
            {ok, NewRev} = couch_db:update_doc(Db2, NewDocVersion, []),
            NewRev
        end,
        {element(1, Doc#doc.revs), hd(element(2, Doc#doc.revs))},
        lists:seq(1, Times)),
    ok = couch_db:close(Db),
    {ok, {DbName, Times}}.
