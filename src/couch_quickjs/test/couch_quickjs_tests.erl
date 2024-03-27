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
    Ctx = test_util:start_couch(),
    Ctx.

teardown(Ctx) ->
    config:delete("quickjs", "memory_limit_bytes", _Persist = false),
    test_util:stop_couch(Ctx).

quickjs_test_() ->
    {
        foreach,
        fun setup/0,
        fun teardown/1,
        case os:type() of
            {win32, _} ->
                [];
            {_, _} ->
                [
                    ?TDEF_FE(t_get_mainjs_cmd),
                    ?TDEF_FE(t_get_coffee_cmd),
                    ?TDEF_FE(t_can_configure_memory_limit),
                    ?TDEF_FE(t_bad_memory_limit)
                ]
        end
    }.

t_get_mainjs_cmd(_) ->
    Cmd = couch_quickjs:mainjs_cmd(),
    ?assert(filelib:is_file(Cmd)),
    ?assertEqual("quickjs\n", os:cmd(Cmd ++ " -V")).

t_get_coffee_cmd(_) ->
    Cmd = couch_quickjs:coffee_cmd(),
    ?assert(filelib:is_file(Cmd)),
    ?assertEqual("quickjs\n", os:cmd(Cmd ++ " -V")).

t_can_configure_memory_limit(_) ->
    Limit = integer_to_list(4 * 1024 * 1024),
    config:set("quickjs", "memory_limit_bytes", Limit, _Persist = false),
    Cmd = couch_quickjs:mainjs_cmd(),
    ?assert(is_list(Cmd)),
    Expect = "-M " ++ Limit,
    ?assertEqual(Expect, string:find(Cmd, Expect)),
    ?assertEqual("quickjs\n", os:cmd(Cmd ++ " -V")).

t_bad_memory_limit(_) ->
    Limit = integer_to_list(42),
    config:set("quickjs", "memory_limit_bytes", Limit, _Persist = false),
    Cmd = couch_quickjs:mainjs_cmd(),
    ?assert(is_list(Cmd)),
    ExpectCmd = "-M " ++ Limit,
    ?assertEqual(ExpectCmd, string:find(Cmd, ExpectCmd)),
    Res = string:trim(os:cmd(Cmd ++ " -V")),
    ExpectRes = "Invalid stack size",
    ?assertEqual(ExpectRes, string:find(Res, ExpectRes)).
