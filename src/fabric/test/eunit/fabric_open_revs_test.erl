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

-module(fabric_open_revs_test).

-include_lib("couch/include/couch_db.hrl").
-include_lib("couch/include/couch_eunit.hrl").

open_revs_fabric_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        with([
            ?TDEF(t_empty_request),
            ?TDEF(t_missing_docs),
            ?TDEF(t_missing_docs_all),
            ?TDEF(t_missing_docs_latest),
            ?TDEF(t_existing_doc_missing_rev),
            ?TDEF(t_existing_doc_missing_rev_latest),
            ?TDEF(t_exact_rev_single_leaf),
            ?TDEF(t_exact_rev_latest_single_leaf),
            ?TDEF(t_exact_rev_multiple_leafs),
            ?TDEF(t_exact_rev_latest_multiple_leafs),
            ?TDEF(t_all_revs_latest_single_leaf),
            ?TDEF(t_two_revs_single_doc),
            ?TDEF(t_two_revs_single_doc_one_missing),
            ?TDEF(t_two_revs_single_doc_latest)
        ])
    }.

open_revs_fabric_timeout_test_() ->
    {
        foreach,
        fun setup/0,
        fun teardown/1,
        [
            ?TDEF_FE(t_timeout_rexi_error)
        ]
    }.

setup() ->
    Ctx = test_util:start_couch([fabric]),
    DbName = ?tempdb(),
    ok = fabric:create_db(DbName, []),
    Docs = [
        #doc{id = <<"a">>, revs = {1, [<<"z">>]}},
        #doc{id = <<"a">>, revs = {2, [<<"x">>, <<"z">>]}},
        #doc{id = <<"a">>, revs = {2, [<<"y">>, <<"z">>]}}
    ],
    Opts = [?REPLICATED_CHANGES],
    {ok, []} = fabric:update_docs(DbName, Docs, Opts),
    {Ctx, DbName}.

teardown({Ctx, DbName}) ->
    fabric:delete_db(DbName, [?ADMIN_CTX]),
    meck:unload(),
    test_util:stop_couch(Ctx).

t_empty_request({_, DbName}) ->
    ?assertEqual({ok, []}, fabric:open_revs(DbName, [], [])).

t_missing_docs({_, DbName}) ->
    Rev = {1, <<"foo">>},
    IdRevsOpts = {{<<"missing">>, [Rev]}, []},
    ?assertEqual(
        [
            {{not_found, missing}, Rev}
        ],
        open_revs_single(DbName, IdRevsOpts, [])
    ).

t_missing_docs_all({_, DbName}) ->
    IdRevsOpts = {{<<"missing">>, all}, []},
    ?assertEqual([], open_revs_single(DbName, IdRevsOpts, [])).

t_missing_docs_latest({_, DbName}) ->
    Rev = {1, <<"foo">>},
    IdRevsOpts = {{<<"missing">>, [Rev]}, []},
    ?assertEqual(
        [
            {{not_found, missing}, Rev}
        ],
        open_revs_single(DbName, IdRevsOpts, [latest])
    ).

t_existing_doc_missing_rev({_, DbName}) ->
    Rev = {1, <<"foo">>},
    IdRevsOpts = {{<<"a">>, [Rev]}, []},
    ?assertEqual(
        [
            {{not_found, missing}, Rev}
        ],
        open_revs_single(DbName, IdRevsOpts, [])
    ).

t_existing_doc_missing_rev_latest({_, DbName}) ->
    Rev = {1, <<"foo">>},
    IdRevsOpts = {{<<"a">>, [Rev]}, []},
    ?assertEqual(
        [
            {{not_found, missing}, Rev}
        ],
        open_revs_single(DbName, IdRevsOpts, [latest])
    ).

t_exact_rev_single_leaf({_, DbName}) ->
    IdRevsOpts = {{<<"a">>, [{2, <<"x">>}]}, []},
    ResRevs = {2, [<<"x">>, <<"z">>]},
    ?assertEqual(
        [
            {ok, #doc{id = <<"a">>, revs = ResRevs}}
        ],
        open_revs_single(DbName, IdRevsOpts, [])
    ).

t_exact_rev_latest_single_leaf({_, DbName}) ->
    IdRevsOpts = {{<<"a">>, [{2, <<"x">>}]}, []},
    ResRevs = {2, [<<"x">>, <<"z">>]},
    ?assertEqual(
        [
            {ok, #doc{id = <<"a">>, revs = ResRevs}}
        ],
        open_revs_single(DbName, IdRevsOpts, [latest])
    ).

t_exact_rev_multiple_leafs({_, DbName}) ->
    IdRevsOpts = {{<<"a">>, [{1, <<"z">>}]}, []},
    ?assertEqual(
        [
            {ok, #doc{id = <<"a">>, revs = {1, [<<"z">>]}}}
        ],
        open_revs_single(DbName, IdRevsOpts, [])
    ).

t_exact_rev_latest_multiple_leafs({_, DbName}) ->
    IdRevsOpts = {{<<"a">>, [{1, <<"z">>}]}, []},
    ?assertEqual(
        [
            {ok, #doc{id = <<"a">>, revs = {2, [<<"x">>, <<"z">>]}}},
            {ok, #doc{id = <<"a">>, revs = {2, [<<"y">>, <<"z">>]}}}
        ],
        open_revs_single(DbName, IdRevsOpts, [latest])
    ).

t_all_revs_latest_single_leaf({_, DbName}) ->
    IdRevsOpts = {{<<"a">>, all}, []},
    ?assertEqual(
        [
            {ok, #doc{id = <<"a">>, revs = {2, [<<"x">>, <<"z">>]}}},
            {ok, #doc{id = <<"a">>, revs = {2, [<<"y">>, <<"z">>]}}}
        ],
        open_revs_single(DbName, IdRevsOpts, [latest])
    ).

t_two_revs_single_doc({_, DbName}) ->
    IdRevsOpts = {{<<"a">>, [{2, <<"x">>}, {2, <<"y">>}]}, []},
    ?assertEqual(
        [
            {ok, #doc{id = <<"a">>, revs = {2, [<<"x">>, <<"z">>]}}},
            {ok, #doc{id = <<"a">>, revs = {2, [<<"y">>, <<"z">>]}}}
        ],
        open_revs_single(DbName, IdRevsOpts, [])
    ).

t_two_revs_single_doc_one_missing({_, DbName}) ->
    IdRevsOpts = {{<<"a">>, [{2, <<"y">>}, {1, <<"foo">>}]}, []},
    ?assertEqual(
        [
            {ok, #doc{id = <<"a">>, revs = {2, [<<"y">>, <<"z">>]}}},
            {{not_found, missing}, {1, <<"foo">>}}
        ],
        open_revs_single(DbName, IdRevsOpts, [])
    ).

t_two_revs_single_doc_latest({_, DbName}) ->
    IdRevsOpts = {{<<"a">>, [{1, <<"z">>}, {2, <<"y">>}]}, []},
    ?assertEqual(
        [
            {ok, #doc{id = <<"a">>, revs = {2, [<<"x">>, <<"z">>]}}},
            {ok, #doc{id = <<"a">>, revs = {2, [<<"y">>, <<"z">>]}}}
        ],
        open_revs_single(DbName, IdRevsOpts, [latest])
    ).

t_timeout_rexi_error({_, DbName}) ->
    IdRevsOpts = {{<<"a">>, [{2, <<"x">>}]}, []},
    meck:new(rexi_utils, [passthrough]),
    meck:new(rexi, [passthrough]),
    RecFun = fun(_Refs, _KeyPos, _Fun, St, _Timeout, _PerMsgTO) ->
        {timeout, St}
    end,
    meck:expect(rexi_utils, recv, RecFun),
    ?assertEqual({error, timeout}, fabric:open_revs(DbName, [IdRevsOpts], [])),
    % Also check that workers were cleaned up properly
    ?assertEqual(1, meck:num_calls(rexi, kill_all, 1)).

open_revs_single(DbName, IdRevsOpts, Options) ->
    {ok, [Res]} = fabric:open_revs(DbName, [IdRevsOpts], Options),
    % Validate batched open_revs/3 call is equivalent to calling
    % the single open_revs/4 with that same Id and Revs args.
    {{Id, Revs}, DocOpts} = IdRevsOpts,
    {ok, ResSingle} = fabric:open_revs(DbName, Id, Revs, DocOpts ++ Options),
    ?assertEqual(ResSingle, Res),
    Res.
