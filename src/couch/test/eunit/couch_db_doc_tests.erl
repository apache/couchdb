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

-module(couch_db_doc_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

start() ->
    test_util:start_couch([ioq]).

setup() ->
    DbName = ?tempdb(),
    {ok, Db} = couch_db:create(DbName, [?ADMIN_CTX]),
    couch_db:close(Db),
    DbName.

teardown(DbName) ->
    ok = couch_server:delete(DbName, [?ADMIN_CTX]),
    ok.

couch_db_doc_test_() ->
    {
        "CouchDB doc tests",
        {
            setup,
            fun start/0,
            fun test_util:stop_couch/1,
            {
                foreach,
                fun setup/0,
                fun teardown/1,
                [
                    ?TDEF_FE(t_truncate_number_of_revisions),
                    ?TDEF_FE(t_raise_bad_request_on_invalid_rev),
                    ?TDEF_FE(t_allow_access_in_doc_keys),
                    ?TDEF_FE(t_make_docs_in_batch)
                ]
            }
        }
    }.

t_truncate_number_of_revisions(DbName) ->
    DocId = <<"foo">>,
    Db = open_db(DbName),
    couch_db:set_revs_limit(Db, 5),
    Rev = create_doc(Db, DocId),
    Rev10 = add_revisions(Db, DocId, Rev, 10),
    {ok, [{ok, #doc{revs = {11, Revs}}}]} = open_doc_rev(Db, DocId, Rev10),
    ?assertEqual(5, length(Revs)).

t_raise_bad_request_on_invalid_rev(DbName) ->
    DocId = <<"foo">>,
    InvalidRev1 = <<"foo">>,
    InvalidRev2 = <<"a-foo">>,
    InvalidRev3 = <<"1-xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx">>,
    Expect = {bad_request, <<"Invalid rev format">>},
    Db = open_db(DbName),
    create_doc(Db, DocId),
    ?assertThrow(Expect, add_revisions(Db, DocId, InvalidRev1, 1)),
    ?assertThrow(Expect, add_revisions(Db, DocId, InvalidRev2, 1)),
    ?assertThrow(Expect, add_revisions(Db, DocId, InvalidRev3, 1)).

t_allow_access_in_doc_keys(_DbName) ->
    Json = <<"{\"_id\":\"foo\",\"_access\":[\"test\"]}">>,
    EJson = couch_util:json_decode(Json),
    Expected = {[{<<"_id">>, <<"foo">>}, {<<"_access">>, [<<"test">>]}]},
    EJson = Expected,
    Doc = couch_doc:from_json_obj(EJson),
    NewEJson = couch_doc:to_json_obj(Doc, []),
    ?assertEqual(NewEJson, Expected).

t_make_docs_in_batch(DbName) ->
    Db0 = open_db(DbName),
    Rev = create_doc(Db0, ~"somedoc"),
    add_revisions(Db0, ~"somedoc", Rev, 3),
    del_doc(Db0, ~"deleted", create_doc(Db0, ~"deleted")),
    create_att_doc(Db0, ~"att"),
    {ok, Db} = couch_db:reopen(Db0),
    Ids = [~"somedoc", ~"deleted", ~"att"],
    Specs = doc_specs(Db, Ids),
    ?assertEqual(3, length(Specs)),

    Expected = [
        couch_db:make_doc(Db, Id, Del, Ptr, Path)
     || {Id, Del, Ptr, Path} <- Specs
    ],
    ?assertEqual(Expected, couch_db:make_docs(Db, Specs)),
    ?assertEqual(lists:reverse(Expected), couch_db:make_docs(Db, lists:reverse(Specs))),
    ?assertEqual([], couch_db:make_docs(Db, [])),

    % And the docs are the ones written
    [#doc{revs = {4, RevIds}}] = couch_db:make_docs(Db, doc_specs(Db, [~"somedoc"])),
    ?assertEqual(4, length(RevIds)),
    [#doc{deleted = true}] = couch_db:make_docs(Db, doc_specs(Db, [~"deleted"])),
    [#doc{atts = [Att]}] = couch_db:make_docs(Db, doc_specs(Db, [~"att"])),
    ?assertEqual(~"a.txt", couch_att:fetch(name, Att)).

open_db(DbName) ->
    {ok, Db} = couch_db:open_int(DbName, [?ADMIN_CTX]),
    Db.

doc_specs(Db, Ids) ->
    lists:flatmap(
        fun(Id) ->
            #full_doc_info{rev_tree = Tree} = couch_db:get_full_doc_info(Db, Id),
            Leafs = couch_key_tree:get_all_leafs(Tree),
            [{Id, Del, Ptr, Path} || {#leaf{deleted = Del, ptr = Ptr}, Path} <- Leafs]
        end,
        Ids
    ).

del_doc(Db0, DocId, Rev) ->
    {ok, Db} = couch_db:reopen(Db0),
    {Pos, RevId} = couch_doc:parse_rev(Rev),
    Doc = #doc{id = DocId, revs = {Pos, [RevId]}, deleted = true, body = {[]}},
    {ok, _} = couch_db:update_doc(Db, Doc, []).

create_att_doc(Db0, DocId) ->
    {ok, Db} = couch_db:reopen(Db0),
    Doc = couch_doc:from_json_obj(
        {[
            {~"_id", DocId},
            {
                ~"_attachments",
                {[
                    {
                        ~"a.txt",
                        {[
                            {~"content_type", ~"text/plain"},
                            {~"data", base64:encode(~"xkcd")}
                        ]}
                    }
                ]}
            }
        ]}
    ),
    {ok, _} = couch_db:update_doc(Db, Doc, []).

create_doc(Db, DocId) ->
    add_revision(Db, DocId, undefined).

open_doc_rev(Db0, DocId, Rev) ->
    {ok, Db} = couch_db:reopen(Db0),
    couch_db:open_doc_revs(Db, DocId, [couch_doc:parse_rev(Rev)], []).

add_revision(Db, DocId, undefined) ->
    add_revision(Db, DocId, []);
add_revision(Db, DocId, Rev) when is_binary(Rev) ->
    add_revision(Db, DocId, [{<<"_rev">>, Rev}]);
add_revision(Db0, DocId, Rev) ->
    {ok, Db} = couch_db:reopen(Db0),
    Doc = couch_doc:from_json_obj({
        [
            {<<"_id">>, DocId},
            {<<"value">>, DocId}
        ] ++ Rev
    }),
    {ok, NewRev} = couch_db:update_doc(Db, Doc, []),
    couch_doc:rev_to_str(NewRev).

add_revisions(Db, DocId, Rev, N) ->
    lists:foldl(
        fun(_, OldRev) ->
            add_revision(Db, DocId, OldRev)
        end,
        Rev,
        lists:seq(1, N)
    ).

rev_to_str_test() ->
    ?assertEqual(<<"0-0">>, couch_doc:rev_to_str({0, "0"})),
    ?assertEqual(<<"0-0">>, couch_doc:rev_to_str({0, <<"0">>})),
    ?assertEqual(<<"1-a">>, couch_doc:rev_to_str({1, <<"a">>})),
    Bytes = <<<<48>> || _ <- lists:seq(1, 16)>>,
    ?assertEqual(<<"0-0000000000000000">>, couch_doc:rev_to_str({0, Bytes})),
    ?assertEqual(<<"1-30303030303030303030303030303030">>, couch_doc:rev_to_str({1, Bytes})),
    ?assertError(badarg, couch_doc:rev_to_str({a, b})),
    ?assertError(function_clause, couch_doc:rev_to_str({-1, <<"x">>})),
    ?assertError(function_clause, couch_doc:rev_to_str(foo)).

parse_rev_test() ->
    ?assertEqual({1, <<"x">>}, couch_doc:parse_rev("1-x")),
    ?assertEqual({1, <<"x">>}, couch_doc:parse_rev(<<"1-x">>)),
    ?assertEqual({0, <<"y">>}, couch_doc:parse_rev("0-y")),
    ?assertEqual({1234567890, <<"abc">>}, couch_doc:parse_rev("1234567890-abc")),
    ?assertEqual(
        {0, <<"00000000000000000000000000000000">>},
        couch_doc:parse_rev("0-00000000000000000000000000000000")
    ),
    ?assertEqual(
        {1, <<0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0>>},
        couch_doc:parse_rev("1-00000000000000000000000000000000")
    ),
    ?assertThrow({bad_request, _}, couch_doc:parse_rev(foo)),
    ?assertThrow({bad_request, _}, couch_doc:parse_rev("x-y")),
    ?assertThrow({bad_request, _}, couch_doc:parse_rev("x")),
    ?assertThrow({bad_request, _}, couch_doc:parse_rev("0")),
    ?assertThrow({bad_request, _}, couch_doc:parse_rev("1")),
    ?assertThrow({bad_request, _}, couch_doc:parse_rev("-")),
    ?assertThrow({bad_request, _}, couch_doc:parse_rev("-0-1")).
