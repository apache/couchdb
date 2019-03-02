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

-module(chttpd_auth_cache_test).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").


setup() ->
    DbName = ?tempdb(),
    ok = fabric:create_db(DbName),
    DbName.


teardown(DbName) ->
    fabric:delete_db(DbName).


auth_db_change_listener_lifecycle_test_() ->
    {
        "auth db and change listener lifecycle tests",
        {
            setup,
            fun chttpd_test_util:start_couch/0,
            fun chttpd_test_util:stop_couch/1,
            {
                foreach,
                fun setup/0, fun teardown/1,
                [
                    fun change_listener_should_not_start_if_auth_db_missing/1,
                    fun change_listener_should_start_if_auth_db_exists/1
                ]
            }
        }
    }.



change_listener_should_not_start_if_auth_db_missing(_DbName) ->
    ?_test(begin
        %% ok = restart_app(chttpd),
        AuthCachePid = whereis(chttpd_auth_cache),
        ?assert(is_pid(AuthCachePid)),
        Expected = {state, undefined, "0"},
        ?assertEqual(Expected, sys:get_state(AuthCachePid)),
        ok
    end).


change_listener_should_start_if_auth_db_exists(DbName) ->
    ?_test(begin
        config:set("chttpd_auth", "authentication_db", ?b2l(DbName), false),
        ok = restart_app(chttpd),
        AuthCachePid = whereis(chttpd_auth_cache),
        ?assert(is_pid(AuthCachePid)),
        ListenerPid = test_util:wait(fun() ->
            case sys:get_state(AuthCachePid) of
                {state, undefined, "0"} -> wait;
                {state, Pid, "0"} when is_pid(Pid) -> Pid
            end
        end, 1000, 10),
        ?assert(is_pid(ListenerPid) andalso is_process_alive(ListenerPid)),
        config:set("chttpd_auth", "authentication_db", "_users", false),
        ok
    end).


restart_app(App) ->
    ok = application:stop(App),
    ok = application:start(App).
