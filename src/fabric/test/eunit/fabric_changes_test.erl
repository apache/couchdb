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

-module(fabric_changes_test).

-include_lib("couch/include/couch_db.hrl").
-include_lib("couch/include/couch_eunit.hrl").

-define(View,
    {<<"views">>,
        {[{<<"v">>, {[{<<"map">>, <<"function(doc) { if (doc._id == 'a') { emit(doc); } }">>}]}}]}}
).
-define(Custom, {<<"filters">>, {[{<<"f">>, <<"function(doc) { return (doc._id == 'a'); }">>}]}}).

fabric_changes_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        with([
            ?TDEF(t_main_only),
            ?TDEF(t_all_docs),
            ?TDEF(t_main_only_include_docs),
            ?TDEF(t_all_docs_include_docs),
            ?TDEF(t_main_only_include_docs_conflicts),
            ?TDEF(t_all_docs_include_docs_conflicts)
        ])
    }.

changes_selector_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        with([
            ?TDEF(t_selector_main_only),
            ?TDEF(t_selector_all_docs),
            ?TDEF(t_selector_main_only_include_docs),
            ?TDEF(t_selector_all_docs_include_docs),
            ?TDEF(t_selector_main_only_include_docs_conflicts),
            ?TDEF(t_selector_all_docs_include_docs_conflicts)
        ])
    }.

changes_view_test_() ->
    {
        setup,
        fun() -> setup_ddoc(?View) end,
        fun teardown/1,
        with([
            ?TDEF(t_view_main_only),
            ?TDEF(t_view_all_docs),
            ?TDEF(t_view_main_only_include_docs),
            ?TDEF(t_view_all_docs_include_docs),
            ?TDEF(t_view_main_only_include_docs_conflicts),
            ?TDEF(t_view_all_docs_include_docs_conflicts)
        ])
    }.

changes_custom_test_() ->
    {
        setup,
        fun() -> setup_ddoc(?Custom) end,
        fun teardown/1,
        with([
            ?TDEF(t_custom_main_only),
            ?TDEF(t_custom_all_docs),
            ?TDEF(t_custom_main_only_include_docs),
            ?TDEF(t_custom_all_docs_include_docs),
            ?TDEF(t_custom_main_only_include_docs_conflicts),
            ?TDEF(t_custom_all_docs_include_docs_conflicts)
        ])
    }.

t_main_only({_, DbName}) ->
    {ok, [#{changes := Changes}], _, _} =
        changes(DbName, #changes_args{style = main_only}),
    ?assertEqual([{[{<<"rev">>, <<"2-y">>}]}], Changes).

t_all_docs({_, DbName}) ->
    {ok, [#{changes := Changes}], _, _} =
        changes(DbName, #changes_args{style = all_docs}),
    ?assertEqual(
        [
            {[{<<"rev">>, <<"2-y">>}]},
            {[{<<"rev">>, <<"2-x">>}]},
            {[{<<"rev">>, <<"2-d">>}]}
        ],
        Changes
    ).

t_main_only_include_docs({_, DbName}) ->
    {ok, [#{changes := Changes, doc := Doc}], _, _} =
        changes(DbName, #changes_args{
            style = main_only,
            include_docs = true
        }),
    ?assertEqual([{[{<<"rev">>, <<"2-y">>}]}], Changes),
    ?assertEqual(
        {[
            {<<"_id">>, <<"a">>},
            {<<"_rev">>, <<"2-y">>}
        ]},
        Doc
    ).

t_all_docs_include_docs({_, DbName}) ->
    {ok, [#{changes := Changes, doc := Doc}], _, _} =
        changes(DbName, #changes_args{
            style = all_docs,
            include_docs = true
        }),
    ?assertEqual(
        [
            {[{<<"rev">>, <<"2-y">>}]},
            {[{<<"rev">>, <<"2-x">>}]},
            {[{<<"rev">>, <<"2-d">>}]}
        ],
        Changes
    ),
    ?assertEqual(
        {[
            {<<"_id">>, <<"a">>},
            {<<"_rev">>, <<"2-y">>}
        ]},
        Doc
    ).

t_main_only_include_docs_conflicts({_, DbName}) ->
    {ok, [#{changes := Changes, doc := Doc}], _, _} =
        changes(DbName, #changes_args{
            style = main_only,
            include_docs = true,
            conflicts = true
        }),
    ?assertEqual([{[{<<"rev">>, <<"2-y">>}]}], Changes),
    ?assertEqual(
        {[
            {<<"_id">>, <<"a">>},
            {<<"_rev">>, <<"2-y">>},
            {<<"_conflicts">>, [<<"2-x">>]}
        ]},
        Doc
    ).

t_all_docs_include_docs_conflicts({_, DbName}) ->
    {ok, [#{changes := Changes, doc := Doc}], _, _} =
        changes(DbName, #changes_args{
            style = all_docs,
            include_docs = true,
            conflicts = true
        }),
    ?assertEqual(
        [
            {[{<<"rev">>, <<"2-y">>}]},
            {[{<<"rev">>, <<"2-x">>}]},
            {[{<<"rev">>, <<"2-d">>}]}
        ],
        Changes
    ),
    ?assertEqual(
        {[
            {<<"_id">>, <<"a">>},
            {<<"_rev">>, <<"2-y">>},
            {<<"_conflicts">>, [<<"2-x">>]}
        ]},
        Doc
    ).

t_selector_main_only({_, DbName}) ->
    {ok, [#{changes := Changes}], _, _} =
        changes(DbName, #changes_args{
            style = main_only,
            filter = "_selector",
            filter_fun = {selector, main_only, {{[{<<"_id">>, {[{<<"$eq">>, <<"a">>}]}}]}, nil}}
        }),
    ?assertEqual([{[{<<"rev">>, <<"2-y">>}]}], Changes).

t_selector_all_docs({_, DbName}) ->
    {ok, [#{changes := Changes}], _, _} =
        changes(DbName, #changes_args{
            style = all_docs,
            filter = "_selector",
            filter_fun = {selector, all_docs, {{[{<<"_id">>, {[{<<"$eq">>, <<"a">>}]}}]}, nil}}
        }),
    ?assertEqual(
        [
            {[{<<"rev">>, <<"2-y">>}]},
            {[{<<"rev">>, <<"2-x">>}]},
            {[{<<"rev">>, <<"2-d">>}]}
        ],
        Changes
    ).

t_selector_main_only_include_docs({_, DbName}) ->
    {ok, [#{changes := Changes, doc := Doc}], _, _} =
        changes(DbName, #changes_args{
            style = main_only,
            filter = "_selector",
            filter_fun = {selector, main_only, {{[{<<"_id">>, {[{<<"$eq">>, <<"a">>}]}}]}, nil}},
            include_docs = true
        }),
    ?assertEqual([{[{<<"rev">>, <<"2-y">>}]}], Changes),
    ?assertEqual(
        {[
            {<<"_id">>, <<"a">>},
            {<<"_rev">>, <<"2-y">>}
        ]},
        Doc
    ).

t_selector_all_docs_include_docs({_, DbName}) ->
    {ok, [#{changes := Changes, doc := Doc}], _, _} =
        changes(DbName, #changes_args{
            style = all_docs,
            filter = "_selector",
            filter_fun = {selector, all_docs, {{[{<<"_id">>, {[{<<"$eq">>, <<"a">>}]}}]}, nil}},
            include_docs = true
        }),
    ?assertEqual(
        [
            {[{<<"rev">>, <<"2-y">>}]},
            {[{<<"rev">>, <<"2-x">>}]},
            {[{<<"rev">>, <<"2-d">>}]}
        ],
        Changes
    ),
    ?assertEqual(
        {[
            {<<"_id">>, <<"a">>},
            {<<"_rev">>, <<"2-y">>}
        ]},
        Doc
    ).

t_selector_main_only_include_docs_conflicts({_, DbName}) ->
    {ok, [#{changes := Changes, doc := Doc}], _, _} =
        changes(DbName, #changes_args{
            style = main_only,
            filter = "_selector",
            filter_fun = {selector, main_only, {{[{<<"_id">>, {[{<<"$eq">>, <<"a">>}]}}]}, nil}},
            include_docs = true,
            conflicts = true
        }),
    ?assertEqual([{[{<<"rev">>, <<"2-y">>}]}], Changes),
    ?assertEqual(
        {[
            {<<"_id">>, <<"a">>},
            {<<"_rev">>, <<"2-y">>},
            {<<"_conflicts">>, [<<"2-x">>]}
        ]},
        Doc
    ).

t_selector_all_docs_include_docs_conflicts({_, DbName}) ->
    {ok, [#{changes := Changes, doc := Doc}], _, _} =
        changes(DbName, #changes_args{
            style = all_docs,
            filter = "_selector",
            filter_fun = {selector, all_docs, {{[{<<"_id">>, {[{<<"$eq">>, <<"a">>}]}}]}, nil}},
            include_docs = true,
            conflicts = true
        }),
    ?assertEqual(
        [
            {[{<<"rev">>, <<"2-y">>}]},
            {[{<<"rev">>, <<"2-x">>}]},
            {[{<<"rev">>, <<"2-d">>}]}
        ],
        Changes
    ),
    ?assertEqual(
        {[
            {<<"_id">>, <<"a">>},
            {<<"_rev">>, <<"2-y">>},
            {<<"_conflicts">>, [<<"2-x">>]}
        ]},
        Doc
    ).

t_view_main_only({_, DbName, Rev}) ->
    {ok, [#{changes := Changes}], _, _} =
        changes(DbName, #changes_args{
            style = main_only,
            filter = "_view",
            filter_fun = {fetch, view, main_only, {<<"_design/ddoc">>, Rev}, <<"v">>}
        }),
    ?assertEqual([{[{<<"rev">>, <<"2-y">>}]}], Changes).

t_view_all_docs({_, DbName, Rev}) ->
    {ok, [#{changes := Changes}], _, _} =
        changes(DbName, #changes_args{
            style = all_docs,
            filter = "_view",
            filter_fun = {fetch, view, all_docs, {<<"_design/ddoc">>, Rev}, <<"v">>}
        }),
    ?assertEqual(
        [
            {[{<<"rev">>, <<"2-y">>}]},
            {[{<<"rev">>, <<"2-x">>}]},
            {[{<<"rev">>, <<"2-d">>}]}
        ],
        Changes
    ).

t_view_main_only_include_docs({_, DbName, Rev}) ->
    {ok, [#{changes := Changes, doc := Doc}], _, _} =
        changes(DbName, #changes_args{
            style = main_only,
            filter = "_view",
            filter_fun = {fetch, view, main_only, {<<"_design/ddoc">>, Rev}, <<"v">>},
            include_docs = true
        }),
    ?assertEqual([{[{<<"rev">>, <<"2-y">>}]}], Changes),
    ?assertEqual(
        {[
            {<<"_id">>, <<"a">>},
            {<<"_rev">>, <<"2-y">>}
        ]},
        Doc
    ).

t_view_all_docs_include_docs({_, DbName, Rev}) ->
    {ok, [#{changes := Changes, doc := Doc}], _, _} =
        changes(DbName, #changes_args{
            style = all_docs,
            filter = "_view",
            filter_fun = {fetch, view, all_docs, {<<"_design/ddoc">>, Rev}, <<"v">>},
            include_docs = true
        }),
    ?assertEqual(
        [
            {[{<<"rev">>, <<"2-y">>}]},
            {[{<<"rev">>, <<"2-x">>}]},
            {[{<<"rev">>, <<"2-d">>}]}
        ],
        Changes
    ),
    ?assertEqual(
        {[
            {<<"_id">>, <<"a">>},
            {<<"_rev">>, <<"2-y">>}
        ]},
        Doc
    ).

t_view_main_only_include_docs_conflicts({_, DbName, Rev}) ->
    {ok, [#{changes := Changes, doc := Doc}], _, _} =
        changes(DbName, #changes_args{
            style = main_only,
            filter = "_view",
            filter_fun = {fetch, view, main_only, {<<"_design/ddoc">>, Rev}, <<"v">>},
            include_docs = true,
            conflicts = true
        }),
    ?assertEqual([{[{<<"rev">>, <<"2-y">>}]}], Changes),
    ?assertEqual(
        {[
            {<<"_id">>, <<"a">>},
            {<<"_rev">>, <<"2-y">>},
            {<<"_conflicts">>, [<<"2-x">>]}
        ]},
        Doc
    ).

t_view_all_docs_include_docs_conflicts({_, DbName, Rev}) ->
    {ok, [#{changes := Changes, doc := Doc}], _, _} =
        changes(DbName, #changes_args{
            style = all_docs,
            filter = "_view",
            filter_fun = {fetch, view, all_docs, {<<"_design/ddoc">>, Rev}, <<"v">>},
            include_docs = true,
            conflicts = true
        }),
    ?assertEqual(
        [
            {[{<<"rev">>, <<"2-y">>}]},
            {[{<<"rev">>, <<"2-x">>}]},
            {[{<<"rev">>, <<"2-d">>}]}
        ],
        Changes
    ),
    ?assertEqual(
        {[
            {<<"_id">>, <<"a">>},
            {<<"_rev">>, <<"2-y">>},
            {<<"_conflicts">>, [<<"2-x">>]}
        ]},
        Doc
    ).

t_custom_main_only({_, DbName, Rev}) ->
    Req = {json_req, null},
    {ok, [#{changes := Changes}], _, _} =
        changes(DbName, #changes_args{
            style = main_only,
            filter = "ddoc/f",
            filter_fun = {fetch, custom, main_only, Req, {<<"_design/ddoc">>, Rev}, <<"f">>}
        }),
    ?assertEqual([{[{<<"rev">>, <<"2-y">>}]}], Changes).

t_custom_all_docs({_, DbName, Rev}) ->
    Req = {json_req, null},
    {ok, [#{changes := Changes}], _, _} =
        changes(DbName, #changes_args{
            style = all_docs,
            filter = "ddoc/f",
            filter_fun = {fetch, custom, all_docs, Req, {<<"_design/ddoc">>, Rev}, <<"f">>}
        }),
    ?assertEqual(
        [
            {[{<<"rev">>, <<"2-y">>}]},
            {[{<<"rev">>, <<"2-x">>}]},
            {[{<<"rev">>, <<"2-d">>}]}
        ],
        Changes
    ).

t_custom_main_only_include_docs({_, DbName, Rev}) ->
    Req = {json_req, null},
    {ok, [#{changes := Changes, doc := Doc}], _, _} =
        changes(DbName, #changes_args{
            style = main_only,
            filter = "ddoc/f",
            filter_fun = {fetch, custom, main_only, Req, {<<"_design/ddoc">>, Rev}, <<"f">>},
            include_docs = true
        }),
    ?assertEqual([{[{<<"rev">>, <<"2-y">>}]}], Changes),
    ?assertEqual(
        {[
            {<<"_id">>, <<"a">>},
            {<<"_rev">>, <<"2-y">>}
        ]},
        Doc
    ).

t_custom_all_docs_include_docs({_, DbName, Rev}) ->
    Req = {json_req, null},
    {ok, [#{changes := Changes, doc := Doc}], _, _} =
        changes(DbName, #changes_args{
            style = all_docs,
            filter = "ddoc/f",
            filter_fun = {fetch, custom, all_docs, Req, {<<"_design/ddoc">>, Rev}, <<"f">>},
            include_docs = true
        }),
    ?assertEqual(
        [
            {[{<<"rev">>, <<"2-y">>}]},
            {[{<<"rev">>, <<"2-x">>}]},
            {[{<<"rev">>, <<"2-d">>}]}
        ],
        Changes
    ),
    ?assertEqual(
        {[
            {<<"_id">>, <<"a">>},
            {<<"_rev">>, <<"2-y">>}
        ]},
        Doc
    ).

t_custom_main_only_include_docs_conflicts({_, DbName, Rev}) ->
    Req = {json_req, null},
    {ok, [#{changes := Changes, doc := Doc}], _, _} =
        changes(DbName, #changes_args{
            style = main_only,
            filter = "ddoc/f",
            filter_fun = {fetch, custom, main_only, Req, {<<"_design/ddoc">>, Rev}, <<"f">>},
            include_docs = true,
            conflicts = true
        }),
    ?assertEqual([{[{<<"rev">>, <<"2-y">>}]}], Changes),
    ?assertEqual(
        {[
            {<<"_id">>, <<"a">>},
            {<<"_rev">>, <<"2-y">>},
            {<<"_conflicts">>, [<<"2-x">>]}
        ]},
        Doc
    ).

t_custom_all_docs_include_docs_conflicts({_, DbName, Rev}) ->
    Req = {json_req, null},
    {ok, [#{changes := Changes, doc := Doc}], _, _} =
        changes(DbName, #changes_args{
            style = all_docs,
            filter = "ddoc/f",
            filter_fun = {fetch, custom, all_docs, Req, {<<"_design/ddoc">>, Rev}, <<"f">>},
            include_docs = true,
            conflicts = true
        }),
    ?assertEqual(
        [
            {[{<<"rev">>, <<"2-y">>}]},
            {[{<<"rev">>, <<"2-x">>}]},
            {[{<<"rev">>, <<"2-d">>}]}
        ],
        Changes
    ),
    ?assertEqual(
        {[
            {<<"_id">>, <<"a">>},
            {<<"_rev">>, <<"2-y">>},
            {<<"_conflicts">>, [<<"2-x">>]}
        ]},
        Doc
    ).

%%%%%%%%%%%%%%%%%%%% Utility Functions %%%%%%%%%%%%%%%%%%%%
setup() ->
    Ctx = test_util:start_couch([fabric]),
    DbName = ?tempdb(),
    ok = fabric:create_db(DbName, [{q, 1}, {n, 1}]),
    Docs = [
        #doc{id = <<"a">>, revs = {1, [<<"z">>]}},
        #doc{id = <<"a">>, revs = {2, [<<"x">>, <<"z">>]}},
        #doc{id = <<"a">>, revs = {2, [<<"y">>, <<"z">>]}},
        #doc{id = <<"a">>, revs = {2, [<<"d">>, <<"z">>]}, deleted = true}
    ],
    Opts = [?REPLICATED_CHANGES],
    {ok, []} = fabric:update_docs(DbName, Docs, Opts),
    {Ctx, DbName}.

setup_ddoc(Ddoc) ->
    {Ctx, DbName} = setup(),
    Doc = #doc{
        id = <<"_design/ddoc">>,
        revs = {0, []},
        body = {[{<<"language">>, <<"javascript">>}, Ddoc]}
    },
    {ok, Rev} = fabric:update_doc(DbName, Doc, [?ADMIN_CTX]),
    {Ctx, DbName, Rev}.

teardown({Ctx, DbName, _}) ->
    teardown({Ctx, DbName});
teardown({Ctx, DbName}) ->
    ok = fabric:delete_db(DbName, [?ADMIN_CTX]),
    test_util:stop_couch(Ctx).

changes_callback(start, Acc) ->
    {ok, Acc};
changes_callback({change, {Change}}, Acc) ->
    CM = maps:from_list(Change),
    {ok, [CM | Acc]};
changes_callback({stop, EndSeq, Pending}, Acc) ->
    {ok, Acc, EndSeq, Pending}.

changes(DbName, #changes_args{} = Args) ->
    fabric_util:isolate(fun() -> fabric:changes(DbName, fun changes_callback/2, [], Args) end).
