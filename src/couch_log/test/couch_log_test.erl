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

-module(couch_log_test).


-include_lib("couch_log/include/couch_log.hrl").
-include_lib("eunit/include/eunit.hrl").


couch_log_test_() ->
    {setup,
        fun couch_log_test_util:start/0,
        fun couch_log_test_util:stop/1,
        gen() ++ [fun check_set_level/0]
    }.


check_set_level() ->
    couch_log:set_level(crit),
    ?assertEqual("crit", config:get("log", "level")).


levels() ->
    [
        debug,
        info,
        notice,
        warning,
        error,
        critical,
        alert,
        emergency,
        none
    ].


gen() ->
    lists:map(fun(L) ->
        Name = "Test log level: " ++ couch_log_util:level_to_string(L),
        {Name, fun() -> check_levels(L, levels()) end}
    end, levels() -- [none]).


check_levels(_, []) ->
    ok;

check_levels(TestLevel, [CfgLevel | RestLevels]) ->
    TestInt = couch_log_util:level_to_integer(TestLevel),
    CfgInt = couch_log_util:level_to_integer(CfgLevel),
    Pid = self(),
    Msg = new_msg(),
    LastKey = couch_log_test_util:last_log_key(),
    couch_log_test_util:with_level(CfgLevel, fun() ->
        couch_log:TestLevel(Msg, []),
        case TestInt >= CfgInt of
            true ->
                ?assertMatch(
                    #log_entry{
                        level = TestLevel,
                        pid = Pid,
                        msg = Msg
                    },
                    couch_log_test_util:last_log()
                );
            false ->
                ?assertEqual(LastKey, couch_log_test_util:last_log_key())
        end
    end),
    check_levels(TestLevel, RestLevels).


new_msg() ->
    Bin = list_to_binary([couch_rand:uniform(255) || _ <- lists:seq(1, 16)]),
    couch_util:to_hex(Bin).
