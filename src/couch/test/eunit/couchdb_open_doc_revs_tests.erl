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

-module(couchdb_open_doc_revs_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

open_doc_revs_test_() ->
    {
        foreach,
        fun setup/0,
        fun teardown/1,
        [
            ?TDEF_FE(open_doc_revs_single),
            ?TDEF_FE(open_doc_revs_batch)
        ]
    }.

setup() ->
    Ctx = test_util:start_couch(),
    DbName = ?tempdb(),
    {ok, Db} = couch_db:create(DbName, [?ADMIN_CTX]),
    Docs = [
        doc(<<"1">>, [<<"z">>]),
        doc(<<"1">>, [<<"x">>, <<"z">>]),
        doc(<<"1">>, [<<"y">>, <<"z">>]),
        doc(<<"1">>, [<<"w">>, <<"y">>, <<"z">>]),

        doc(<<"2">>, [<<"m">>]),
        doc(<<"2">>, [<<"l">>, <<"k">>], true)
    ],
    {ok, []} = couch_db:update_docs(Db, Docs, [], ?REPLICATED_CHANGES),
    ok = couch_db:close(Db),
    {Ctx, DbName}.

teardown({Ctx, DbName}) ->
    ok = couch_server:delete(DbName, [?ADMIN_CTX]),
    test_util:stop_couch(Ctx).

open_doc_revs_single({_, DbName}) ->
    {ok, Db} = couch_db:open_int(DbName, []),

    % Empty revs list
    ?assertEqual({ok, []}, couch_db:open_doc_revs(Db, <<"1">>, [], [])),

    % Missing rev
    ?assertMatch(
        {ok, [{{not_found, missing}, _}]},
        couch_db:open_doc_revs(Db, <<"1">>, [{1, <<"foo">>}], [])
    ),

    % Missing doc
    ?assertMatch(
        {ok, [{{not_found, missing}, _}]},
        couch_db:open_doc_revs(Db, <<"foo">>, [{1, <<"bar">>}], [])
    ),

    % All revs option
    {ok, Res1} = couch_db:open_doc_revs(Db, <<"1">>, all, []),
    ?assertMatch(
        [
            {ok, #doc{revs = {2, [<<"x">>, <<"z">>]}}},
            {ok, #doc{revs = {3, [<<"w">>, <<"y">>, <<"z">>]}}}
        ],
        Res1
    ),

    % Basic revs fetch
    {ok, Res2} = couch_db:open_doc_revs(Db, <<"1">>, [{2, <<"x">>}], []),
    ?assertMatch([{ok, #doc{revs = {2, [<<"x">>, <<"z">>]}}}], Res2),

    % Fetch a revision that's not the latest
    {ok, Res3} = couch_db:open_doc_revs(Db, <<"1">>, [{1, <<"z">>}], []),
    ?assertMatch([{ok, #doc{revs = {1, [<<"z">>]}}}], Res3),

    % Force latest revision fetchs, since it's a conflict we get 2
    % revisions back. They are both "latest".
    {ok, Res4} = couch_db:open_doc_revs(Db, <<"1">>, [{1, <<"z">>}], [latest]),
    ?assertMatch(
        [
            {ok, #doc{revs = {2, [<<"x">>, <<"z">>]}}},
            {ok, #doc{revs = {3, [<<"w">>, <<"y">>, <<"z">>]}}}
        ],
        Res4
    ),

    % Latest option with rev from a single branch so we get only one
    % result back.
    {ok, Res5} = couch_db:open_doc_revs(Db, <<"1">>, [{2, <<"y">>}], [latest]),
    ?assertMatch(
        [
            {ok, #doc{revs = {3, [<<"w">>, <<"y">>, <<"z">>]}}}
        ],
        Res5
    ),

    % Make sure deleted revisions are also returned
    {ok, Res6} = couch_db:open_doc_revs(Db, <<"2">>, all, []),
    ?assertMatch(
        [
            {ok, #doc{revs = {1, [<<"m">>]}}},
            {ok, #doc{revs = {2, [<<"l">>, <<"k">>]}, deleted = true}}
        ],
        Res6
    ).

open_doc_revs_batch({_, DbName}) ->
    {ok, Db} = couch_db:open_int(DbName, []),

    % Empty batch
    ?assertEqual([], couch_db:open_doc_revs(Db, [], [])),

    % One doc, empty list of revisions
    ?assertEqual(
        [[]],
        couch_db:open_doc_revs(
            Db,
            [
                {{<<"1">>, []}, []}
            ],
            []
        )
    ),

    % Multiple results. Some found, some not found
    ?assertMatch(
        [
            [
                {ok, #doc{revs = {2, [<<"x">>, <<"z">>]}}},
                {ok, #doc{revs = {3, [<<"w">>, <<"y">>, <<"z">>]}}}
            ],
            [
                {{not_found, _}, _}
            ],
            [
                {{not_found, _}, _}
            ]
        ],
        couch_db:open_doc_revs(
            Db,
            [
                {{<<"1">>, all}, []},
                {{<<"1">>, [{1, <<"foo">>}]}, []},
                {{<<"foo">>, [{1, <<"bar">>}]}, []}
            ],
            []
        )
    ),

    % Fetch the exact same doc and revisions
    ?assertMatch(
        [
            [
                {ok, #doc{revs = {2, [<<"y">>, <<"z">>]}}}
            ],
            [
                {ok, #doc{revs = {2, [<<"y">>, <<"z">>]}}}
            ]
        ],
        couch_db:open_doc_revs(
            Db,
            [
                {{<<"1">>, [{2, <<"y">>}]}, []},
                {{<<"1">>, [{2, <<"y">>}]}, []}
            ],
            []
        )
    ),

    % Make sure individual doc options are applied
    [[{ok, Doc1}], [{ok, Doc2}]] = couch_db:open_doc_revs(
        Db,
        [
            {{<<"1">>, [{2, <<"y">>}]}, [{atts_since, [{2, <<"y">>}]}]},
            {{<<"1">>, [{2, <<"y">>}]}, []}
        ],
        []
    ),
    [Att1] = Doc1#doc.atts,
    [Att2] = Doc2#doc.atts,

    % Only attachments since revision > 2-y will be included, which means for
    % revisions =< 2-y we'll get a stub only.
    ?assertEqual(stub, couch_att:fetch(data, Att1)),

    % atts_since shouldn't be applied to the second doc so the stream open
    % handle to read attachment data should be included.
    ?assertMatch({stream, _}, couch_att:fetch(data, Att2)).

doc(Id, Revs) ->
    doc(Id, Revs, false).

doc(Id, Revs, Deleted) ->
    #doc{
        id = Id,
        revs = {length(Revs), Revs},
        deleted = Deleted,
        body = {[{<<"data">>, 42}]},
        atts = [att(<<"att1">>)]
    }.

att(Name) when is_binary(Name) ->
    couch_att:new([
        {name, Name},
        {att_len, 1},
        {type, <<"app/binary">>},
        {data, <<"x">>}
    ]).
