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

-module(couch_quickjs_tests).

-include_lib("couch/include/couch_eunit.hrl").

setup() ->
    test_util:start_couch().

teardown(Ctx) ->
    Persist = false,
    config:delete("quickjs", "memory_limit_bytes", Persist),
    config:delete("couchdb", "js_engine", Persist),
    test_util:stop_couch(Ctx).

quickjs_test_() ->
    {
        foreach,
        fun setup/0,
        fun teardown/1,
        [
            ?TDEF_FE(t_get_mainjs_cmd),
            ?TDEF_FE(t_get_coffee_cmd),
            ?TDEF_FE(t_can_configure_memory_limit),
            ?TDEF_FE(t_bad_memory_limit),
            ?TDEF_FE(t_couch_jsengine_config_triggers_proc_server_reload)
        ]
    }.

t_get_mainjs_cmd(_) ->
    Cmd = couch_quickjs:mainjs_cmd(),
    ?assertEqual(0, os_cmd(Cmd ++ " -V")).

t_get_coffee_cmd(_) ->
    Cmd = couch_quickjs:coffee_cmd(),
    ?assertEqual(0, os_cmd(Cmd ++ " -V")).

t_can_configure_memory_limit(_) ->
    Limit = integer_to_list(4 * 1024 * 1024),
    config:set("quickjs", "memory_limit_bytes", Limit, _Persist = false),
    Cmd = couch_quickjs:mainjs_cmd(),
    ?assert(is_list(Cmd)),
    Expect = "-M " ++ Limit,
    ?assertEqual(Expect, string:find(Cmd, Expect)),
    ?assertEqual(0, os_cmd(Cmd ++ " -V")).

t_bad_memory_limit(_) ->
    Limit = integer_to_list(42),
    config:set("quickjs", "memory_limit_bytes", Limit, _Persist = false),
    Cmd = couch_quickjs:mainjs_cmd(),
    ?assert(is_list(Cmd)),
    ExpectCmd = "-M " ++ Limit,
    ?assertEqual(ExpectCmd, string:find(Cmd, ExpectCmd)),
    ?assertEqual(1, os_cmd(Cmd ++ " -V")).

t_couch_jsengine_config_triggers_proc_server_reload(_) ->
    case couch_server:with_spidermonkey() of
        false ->
            % Spidermonkey is not in the build at all, skip the test
            ok;
        true ->
            OldVal = get_proc_manager_default_js(),
            % In the test, set the engine to whatever is not the default
            Toggle =
                case couch_server:get_js_engine() of
                    <<"quickjs">> -> "spidermonkey";
                    <<"spidermonkey">> -> "quickjs"
                end,

            config:set("couchdb", "js_engine", Toggle, false),
            % couch_server:get_js_engine/0 should be visible immediately
            ?assertEqual(list_to_binary(Toggle), couch_server:get_js_engine()),

            wait_until_proc_manager_updates(OldVal),
            ?assertNotEqual(OldVal, get_proc_manager_default_js()),
            NewVal = get_proc_manager_default_js(),

            % Toggle back to the original default (test config:delete/3)
            config:delete("couchdb", "js_engine", false),
            wait_until_proc_manager_updates(NewVal),
            ?assertNotEqual(NewVal, get_proc_manager_default_js()),
            % We should be back to the original default
            ?assertEqual(OldVal, get_proc_manager_default_js())
    end.

os_cmd(Cmd) ->
    Opts = [stream, {line, 4096}, binary, exit_status, hide],
    Port = open_port({spawn, Cmd}, Opts),
    receive
        {Port, {exit_status, Status}} -> Status
    end.

wait_until_proc_manager_updates(OldVal) ->
    WaitFun = fun() ->
        case get_proc_manager_default_js() of
            OldVal -> wait;
            not_found -> wait;
            _ -> ok
        end
    end,
    case test_util:wait(WaitFun, 4500) of
        timeout -> error(timeout);
        _ -> ok
    end.

get_proc_manager_default_js() ->
    case ets:lookup(couch_proc_manager_servers, "JAVASCRIPT") of
        [{"JAVASCRIPT", Val}] -> Val;
        _ -> not_found
    end.
