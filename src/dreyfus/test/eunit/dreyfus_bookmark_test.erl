% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
% http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(dreyfus_bookmark_test).

-include_lib("mem3/include/mem3.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DBNAME, <<"db">>).

setup() ->
    meck:new(mem3),
    % One ([0,10]) is a known shard only
    meck:expect(mem3, get_shard, fun
        (?DBNAME, n1, [0, 10]) -> {ok, known_shard()};
        (_, _, _) -> {error, not_found}
    end).

teardown(_) ->
    meck:unload().

known_shard() ->
    #shard{
        name = <<"shards/00000000-0000000a/db.1">>,
        node = n1,
        range = [0, 10],
        dbname = ?DBNAME
    }.

placeholder(Node, Range) ->
    #shard{node = Node, range = Range, dbname = ?DBNAME, _ = '_'}.

dreyfus_bookmark_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        [
            fun t_roundtrip/0,
            fun t_rejects_unknown_atoms/0,
            fun t_rejects_malformed/0
        ]
    }.

t_roundtrip() ->
    Workers = [
        {known_shard(), {1.5, <<"doc1">>}},
        {#shard{node = n2, range = [11, ?RING_END]}, [<<"x">>, null]},
        {#shard{node = n3, range = [11, ?RING_END]}, nil}
    ],
    Packed = dreyfus_bookmark:pack(Workers),
    ?assertEqual(
        [
            {known_shard(), {1.5, <<"doc1">>}},
            {placeholder(n2, [11, ?RING_END]), [<<"x">>, null]}
        ],
        dreyfus_bookmark:unpack(?DBNAME, Packed)
    ).

t_rejects_unknown_atoms() ->
    %> couch_util:encodeBase64Url(term_to_binary([{'otherclust@baz', [0, 10], 42}])).
    %> <<"g2wAAAABaAN3Dm90aGVyY2x1c3RAYmF6awACAAphKmo">>
    Unknown = <<"g2wAAAABaAN3Dm90aGVyY2x1c3RAYmF6awACAAphKmo">>,
    %> couch_util:encodeBase64Url(term_to_binary([{n1, [0, 10], 42}])).
    %> <<"g2wAAAABaAN3Am4xawACAAphKmo">>
    Known = <<"g2wAAAABaAN3Am4xawACAAphKmo">>,
    ?assertError(badarg, dreyfus_bookmark:unpack(?DBNAME, Unknown)),
    ?assertError(badarg, binary_to_existing_atom(<<"otherclust@baz">>, utf8)),
    ?assertEqual([{known_shard(), 42}], dreyfus_bookmark:unpack(?DBNAME, Known)).

t_rejects_malformed() ->
    T = fun(Term) -> couch_util:encodeBase64Url(term_to_binary(Term)) end,
    ?assertError(badarg, dreyfus_bookmark:unpack(?DBNAME, T({n1, [0, 10], 42}))),
    ?assertError(badarg, dreyfus_bookmark:unpack(?DBNAME, T([{n1, [0, 10]}]))),
    ?assertError(badarg, dreyfus_bookmark:unpack(?DBNAME, T([{n1, {0, 10}, 42}]))),
    ?assertError(badarg, dreyfus_bookmark:unpack(?DBNAME, T([{n1, [0, 1 bsl 32], 42}]))),
    ?assertError(badarg, dreyfus_bookmark:unpack(?DBNAME, T([{n1, [10, 0], 42}]))),
    ?assertError(badarg, dreyfus_bookmark:unpack(?DBNAME, T([{<<"n1">>, [0, 10], 42}]))),
    ?assertError(badarg, dreyfus_bookmark:unpack(?DBNAME, <<"not base64url junk!">>)),
    ?assertError(badarg, dreyfus_bookmark:unpack(?DBNAME, couch_util:encodeBase64Url(<<2, 0>>))).
