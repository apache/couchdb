% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License.  You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
% License for the specific language governing permissions and limitations under
% the License.

-module(chttpd_endpoints_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

setup("mocked") ->
    fun setup_mocked/1;
setup("not_mocked") ->
    fun setup_not_mocked/1.

setup_mocked({Endpoint, {_Path, Module, Function}}) ->
    catch meck:unload(Module),
    meck:new(Module, [passthrough, non_strict]),
    Expected = mock_handler(Endpoint, Module, Function),
    Expected.

setup_not_mocked({_Endpoint, {_Path, Module, _Function}}) ->
    catch meck:unload(Module),
    meck:new(Module, [non_strict]),
    ok.

teardown({_Endpoint, {Module, _F, _A}}, _) ->
    catch meck:unload(Module),
    ok.

handlers(url_handler) ->
    [
        {<<"">>, chttpd_misc, handle_welcome_req},
        {<<"favicon.ico">>, chttpd_misc, handle_favicon_req},
        {<<"_utils">>, chttpd_misc, handle_utils_dir_req},
        {<<"_all_dbs">>, chttpd_misc, handle_all_dbs_req},
        {<<"_active_tasks">>, chttpd_misc, handle_task_status_req},
        {<<"_node">>, chttpd_misc, handle_node_req},
        {<<"_reload_query_servers">>, chttpd_misc, handle_reload_query_servers_req},
        {<<"_replicate">>, chttpd_misc, handle_replicate_req},
        {<<"_uuids">>, chttpd_misc, handle_uuids_req},
        {<<"_session">>, chttpd_auth, handle_session_req},
        {<<"_oauth">>, couch_httpd_oauth, handle_oauth_req},
        {<<"_up">>, chttpd_misc, handle_up_req},
        {<<"_membership">>, mem3_httpd, handle_membership_req},
        {<<"_db_updates">>, global_changes_httpd, handle_global_changes_req},
        {<<"_cluster_setup">>, setup_httpd, handle_setup_req},
        {<<"anything">>, chttpd_db, handle_request}
    ];
handlers(db_handler) ->
    [
        {<<"_view_cleanup">>, chttpd_db, handle_view_cleanup_req},
        {<<"_compact">>, chttpd_db, handle_compact_req},
        {<<"_design">>, chttpd_db, handle_design_req},
        {<<"_temp_view">>, chttpd_view, handle_temp_view_req},
        {<<"_changes">>, chttpd_db, handle_changes_req},
        {<<"_shards">>, mem3_httpd, handle_shards_req},
        {<<"_index">>, mango_httpd, handle_req},
        {<<"_explain">>, mango_httpd, handle_req},
        {<<"_find">>, mango_httpd, handle_req}
    ];
handlers(design_handler) ->
    [
        {<<"_view">>, chttpd_view, handle_view_req},
        {<<"_show">>, chttpd_show, handle_doc_show_req},
        {<<"_list">>, chttpd_show, handle_view_list_req},
        {<<"_update">>, chttpd_show, handle_doc_update_req},
        {<<"_info">>, chttpd_db, handle_design_info_req},
        {<<"_rewrite">>, chttpd_rewrite, handle_rewrite_req}
    ].

endpoints_test_() ->
    {
        "Checking dynamic endpoints",
        {
            setup,
            fun() -> test_util:start_couch([chttpd, mem3, global_changes, mango]) end,
            fun test_util:stop/1,
            [
                check_dynamic_endpoints(
                    "mocked", url_handler, fun ensure_called/2),
                check_dynamic_endpoints(
                    "mocked", db_handler, fun ensure_called/2),
                check_dynamic_endpoints(
                    "mocked", design_handler, fun ensure_called/2),
                check_dynamic_endpoints(
                    "not_mocked", url_handler, fun verify_we_fail_if_missing/2),
                check_dynamic_endpoints(
                    "not_mocked", db_handler, fun verify_we_fail_if_missing/2),
                check_dynamic_endpoints(
                    "not_mocked", design_handler, fun verify_we_fail_if_missing/2)
            ]
        }
    }.

check_dynamic_endpoints(Setup, EndpointType, TestFun) ->
    {
        "Checking '"
            ++ atom_to_list(EndpointType)
            ++ "' [" ++ Setup ++ "] dynamic endpoints",
        [
            make_test_case(Setup, EndpointType, Spec, TestFun)
               || Spec <- handlers(EndpointType)
        ]
    }.

make_test_case(Setup, EndpointType, {Path, Module, Function}, TestFun) ->
    {
        lists:flatten(io_lib:format("~s -- \"~s\"", [EndpointType, ?b2l(Path)])),
        {
            foreachx, setup(Setup), fun teardown/2,
            [
                {{EndpointType, {Path, Module, Function}}, TestFun}
            ]
        }
    }.


mock_handler(url_handler = Endpoint, M, F) ->
    meck:expect(M, F, fun(X) -> {return, Endpoint, X} end),
    fun M:F/1;
mock_handler(db_handler = Endpoint, M, F) ->
    meck:expect(M, F, fun(X, Y) -> {return, Endpoint, X, Y} end),
    fun M:F/2;
mock_handler(design_handler = Endpoint, M, F) ->
    meck:expect(M, F, fun(X, Y, Z) -> {return, Endpoint, X, Y, Z} end),
    fun M:F/3.

ensure_called({url_handler = Endpoint, {Path, _M, _Fun}}, ExpectedFun) ->
    HandlerFun = handler(Endpoint, Path),
    ?_test(begin
        ?assertEqual(ExpectedFun, HandlerFun),
        ?assertMatch({return, Endpoint, x}, HandlerFun(x))
     end);
ensure_called({db_handler = Endpoint, {Path, _M, _Fun}}, ExpectedFun) ->
    HandlerFun = handler(Endpoint, Path),
    ?_test(begin
        ?assertEqual(ExpectedFun, HandlerFun),
        ?assertMatch({return, Endpoint, x, y}, HandlerFun(x, y))
     end);
ensure_called({design_handler = Endpoint, {Path, _M, _Fun}}, ExpectedFun) ->
    HandlerFun = handler(Endpoint, Path),
    ?_test(begin
        ?assertEqual(ExpectedFun, HandlerFun),
        ?assertMatch({return, Endpoint, x, y, z}, HandlerFun(x, y, z))
     end).

%% Test the test: when the final target function is missing,
%% the Fun call must fail.
verify_we_fail_if_missing({url_handler = Endpoint, {Path, _M, _Fun}}, _) ->
    HandlerFun = handler(Endpoint, Path),
    ?_test(begin
        ?assert(is_function(HandlerFun)),
        ?assertError(undef, HandlerFun(x))
    end);
verify_we_fail_if_missing({db_handler = Endpoint, {Path, _M, _Fun}}, _) ->
    HandlerFun = handler(Endpoint, Path),
    ?_test(begin
        ?assert(is_function(HandlerFun)),
        ?assertError(undef, HandlerFun(x, y))
    end);
verify_we_fail_if_missing({design_handler = Endpoint, {Path, _M, _Fun}}, _) ->
    HandlerFun = handler(Endpoint, Path),
    ?_test(begin
        ?assert(is_function(HandlerFun)),
        ?assertError(undef, HandlerFun(x, y, z))
    end).

handler(url_handler, HandlerKey) ->
    chttpd_handlers:url_handler(HandlerKey, fun chttpd_db:handle_request/1);
handler(db_handler, HandlerKey) ->
    chttpd_handlers:db_handler(HandlerKey, fun chttpd_db:db_req/2);
handler(design_handler, HandlerKey) ->
    chttpd_handlers:design_handler(HandlerKey, fun dummy/3).

dummy(_, _, _) ->
    throw(error).
