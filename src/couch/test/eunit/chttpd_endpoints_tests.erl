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

endpoints_test_() ->
    {
        "Checking dynamic endpoints",
        {
            setup,
            fun() ->
                test_util:start_couch([chttpd])
            end,
            fun test_util:stop/1,
            [
                fun url_handlers/0,
                fun db_handlers/0,
                fun design_handlers/0
            ]
        }
    }.

url_handlers() ->
    Handlers = [
        {<<"">>, chttpd_misc, handle_welcome_req},
        {<<"favicon.ico">>, chttpd_misc, handle_favicon_req},
        {<<"_utils">>, chttpd_misc, handle_utils_dir_req},
        {<<"_all_dbs">>, chttpd_misc, handle_all_dbs_req},
        {<<"_dbs_info">>, chttpd_misc, handle_dbs_info_req},
        {<<"_active_tasks">>, chttpd_misc, handle_task_status_req},
        {<<"_node">>, chttpd_node, handle_node_req},
        {<<"_reload_query_servers">>, chttpd_misc, handle_reload_query_servers_req},
        {<<"_replicate">>, chttpd_misc, handle_replicate_req},
        {<<"_uuids">>, chttpd_misc, handle_uuids_req},
        {<<"_session">>, chttpd_auth, handle_session_req},
        {<<"_up">>, chttpd_misc, handle_up_req},
        {<<"_membership">>, mem3_httpd, handle_membership_req},
        {<<"_db_updates">>, global_changes_httpd, handle_global_changes_req},
        {<<"_cluster_setup">>, setup_httpd, handle_setup_req}
    ],

    lists:foreach(
        fun({Path, Mod, Fun}) ->
            Handler = chttpd_handlers:url_handler(Path, undefined),
            Expect = fun Mod:Fun/1,
            ?assertEqual(Expect, Handler)
        end,
        Handlers
    ),

    ?assertEqual(undefined, chttpd_handlers:url_handler("foo", undefined)).

db_handlers() ->
    Handlers = [
        {<<"_view_cleanup">>, chttpd_db, handle_view_cleanup_req},
        {<<"_compact">>, chttpd_db, handle_compact_req},
        {<<"_design">>, chttpd_db, handle_design_req},
        {<<"_temp_view">>, chttpd_view, handle_temp_view_req},
        {<<"_changes">>, chttpd_db, handle_changes_req},
        {<<"_shards">>, mem3_httpd, handle_shards_req},
        {<<"_index">>, mango_httpd, handle_req},
        {<<"_explain">>, mango_httpd, handle_req},
        {<<"_find">>, mango_httpd, handle_req}
    ],

    lists:foreach(
        fun({Path, Mod, Fun}) ->
            Handler = chttpd_handlers:db_handler(Path, undefined),
            Expect = fun Mod:Fun/2,
            ?assertEqual(Expect, Handler)
        end,
        Handlers
    ),

    ?assertEqual(undefined, chttpd_handlers:db_handler("bam", undefined)).

design_handlers() ->
    Handlers = [
        {<<"_view">>, chttpd_view, handle_view_req},
        {<<"_show">>, chttpd_show, handle_doc_show_req},
        {<<"_list">>, chttpd_show, handle_view_list_req},
        {<<"_update">>, chttpd_show, handle_doc_update_req},
        {<<"_info">>, chttpd_db, handle_design_info_req},
        {<<"_rewrite">>, chttpd_rewrite, handle_rewrite_req}
    ],

    lists:foreach(
        fun({Path, Mod, Fun}) ->
            Handler = chttpd_handlers:design_handler(Path, undefined),
            Expect = fun Mod:Fun/3,
            ?assertEqual(Expect, Handler)
        end,
        Handlers
    ),

    ?assertEqual(undefined, chttpd_handlers:design_handler("baz", undefined)).
