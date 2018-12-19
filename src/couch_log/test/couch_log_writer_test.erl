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

-module(couch_log_writer_test).


-include_lib("couch_log/include/couch_log.hrl").
-include_lib("eunit/include/eunit.hrl").


couch_log_writer_test_() ->
    {setup,
        fun couch_log_test_util:start/0,
        fun couch_log_test_util:stop/1,
        [
            fun check_writer_change/0
        ]
    }.


check_writer_change() ->
    % Change to file and back
    couch_log_test_util:with_config_listener(fun() ->
        config:set("log", "writer", "file"),
        couch_log_test_util:wait_for_config(),
        ?assertEqual(undefined, ets:info(?COUCH_LOG_TEST_TABLE)),
        ?assert(is_pid(whereis(couch_log_server))),

        config:set("log", "writer", "couch_log_writer_ets"),
        couch_log_test_util:wait_for_config(),
        ?assertEqual(0, ets:info(?COUCH_LOG_TEST_TABLE, size))
    end),

    % Using a bad setting doesn't break things
    couch_log_test_util:with_config_listener(fun() ->
        config:set("log", "writer", "hopefully not an atom or module"),
        couch_log_test_util:wait_for_config(),
        ?assertEqual(undefined, ets:info(?COUCH_LOG_TEST_TABLE)),
        ?assert(is_pid(whereis(couch_log_server))),

        config:set("log", "writer", "couch_log_writer_ets"),
        couch_log_test_util:wait_for_config(),
        ?assertEqual(0, ets:info(?COUCH_LOG_TEST_TABLE, size))
    end).

