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
    ok = config:delete("chttpd_auth", "authentication_db", false),
    catch fabric:delete_db(DbName).


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
                    fun change_listener_should_not_start_if_auth_db_undefined/1,
                    fun change_listener_should_start_if_auth_db_exists/1
                ]
            }
        }
    }.



change_listener_should_not_start_if_auth_db_undefined(_DbName) ->
    ?_test(begin
        AuthCachePid = whereis(chttpd_auth_cache),
        ?assert(is_pid(AuthCachePid)),
        Expected = {state, undefined, "0"},
        timer:sleep(10),
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

        %% change listener should stop and not restart after auth db deleted
        ok = fabric:delete_db(DbName),
        test_util:wait(fun() ->
            case is_process_alive(ListenerPid) of
                true -> wait;
                false -> ok
            end
        end, 1000, 10),
        ?assert(is_process_alive(AuthCachePid)),
        timer:sleep(10), % assert that it doesn't restart after 10 ms
        ?assertEqual({state, undefined, 0}, sys:get_state(AuthCachePid)),
        ok
    end).

% change_listener_should_start_after_auth_db_created

restart_app(App) ->
    ok = application:stop(App),
    ok = application:start(App).
