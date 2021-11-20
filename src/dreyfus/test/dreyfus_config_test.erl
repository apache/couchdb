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

-module(dreyfus_config_test).

-include_lib("couch_log/include/couch_log.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(TIMEOUT, 1000).

start() ->
    test_util:start_couch([dreyfus]).

setup() ->
    ok.

teardown(_) ->
    ok.

dreyfus_config_test_() ->
    {
        "dreyfus config tests",
        {
            setup,
            fun start/0,
            fun test_util:stop_couch/1,
            {
                foreach,
                fun setup/0,
                fun teardown/1,
                [
                    fun check_black_list/0,
                    fun check_delete_from_blacklist/0
                ]
            }
        }
    }.

check_black_list() ->
    Index = "mydb.myddocid.myindexname",
    Index2 = "mydb2.myddocid2.myindexname2",
    Index3 = "mydb3.myddocid3.myindexname3",
    ok = config:set("dreyfus_blacklist", Index, "true"),
    ok = config:set("dreyfus_blacklist", Index2, "true"),
    ok = config:set("dreyfus_blacklist", Index3, "true"),
    dreyfus_test_util:wait_config_change(Index3, "true"),
    FinalBl = [Index3, Index2, Index],
    lists:foreach(
        fun(I) ->
            ?assertEqual("true", dreyfus_config:get(I))
        end,
        FinalBl
    ).

check_delete_from_blacklist() ->
    Index = "mydb.myddocid.myindexname",
    Index2 = "mydb2.myddocid2.myindexname2",
    ok = config:set("dreyfus_blacklist", Index, "true"),
    dreyfus_test_util:wait_config_change(Index, "true"),
    ok = config:delete("dreyfus_blacklist", Index),
    dreyfus_test_util:wait_config_change(Index, undefined),
    ok = config:set("dreyfus_blacklist", Index2, "true"),
    dreyfus_test_util:wait_config_change(Index2, "true"),
    ?assertEqual(undefined, dreyfus_config:get(Index)),
    ?assertEqual("true", dreyfus_config:get(Index2)).
