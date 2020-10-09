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

-module(couch_log_config_test).


-include_lib("couch_log/include/couch_log.hrl").
-include_lib("eunit/include/eunit.hrl").


couch_log_config_test_() ->
    {setup,
        fun couch_log_test_util:start/0,
        fun couch_log_test_util:stop/1,
        [
            fun check_level/0,
            fun check_max_message_size/0,
            fun check_bad_level/0,
            fun check_bad_max_message_size/0
        ]
    }.


check_level() ->
    % Default level is info
    ?assertEqual(info, couch_log_config:get(level)),
    ?assertEqual(2, couch_log_config:get(level_int)),

    couch_log_test_util:with_config_listener(fun() ->
        config:set("log", "level", "emerg"),
        couch_log_test_util:wait_for_config(),
        ?assertEqual(emergency, couch_log_config:get(level)),
        ?assertEqual(8, couch_log_config:get(level_int)),

        config:set("log", "level", "debug"),
        couch_log_test_util:wait_for_config(),
        ?assertEqual(debug, couch_log_config:get(level)),
        ?assertEqual(1, couch_log_config:get(level_int)),

        config:delete("log", "level"),
        couch_log_test_util:wait_for_config(),
        ?assertEqual(info, couch_log_config:get(level)),
        ?assertEqual(2, couch_log_config:get(level_int))
    end).


check_max_message_size() ->
    % Default is 16000
    ?assertEqual(16000, couch_log_config:get(max_message_size)),

    couch_log_test_util:with_config_listener(fun() ->
        config:set("log", "max_message_size", "1024"),
        couch_log_test_util:wait_for_config(),
        ?assertEqual(1024, couch_log_config:get(max_message_size)),

        config:delete("log", "max_message_size"),
        couch_log_test_util:wait_for_config(),
        ?assertEqual(16000, couch_log_config:get(max_message_size))
    end).


check_bad_level() ->
    % Default level is info
    ?assertEqual(info, couch_log_config:get(level)),
    ?assertEqual(2, couch_log_config:get(level_int)),

    couch_log_test_util:with_config_listener(fun() ->
        config:set("log", "level", "debug"),
        couch_log_test_util:wait_for_config(),
        ?assertEqual(debug, couch_log_config:get(level)),
        ?assertEqual(1, couch_log_config:get(level_int)),

        config:set("log", "level", "this is not a valid level name"),
        couch_log_test_util:wait_for_config(),
        ?assertEqual(info, couch_log_config:get(level)),
        ?assertEqual(2, couch_log_config:get(level_int)),

        config:delete("log", "level"),
        couch_log_test_util:wait_for_config(),
        ?assertEqual(info, couch_log_config:get(level)),
        ?assertEqual(2, couch_log_config:get(level_int))
    end).


check_bad_max_message_size() ->
    % Default level is 16000
    ?assertEqual(16000, couch_log_config:get(max_message_size)),

    couch_log_test_util:with_config_listener(fun() ->
        config:set("log", "max_message_size", "1024"),
        couch_log_test_util:wait_for_config(),
        ?assertEqual(1024, couch_log_config:get(max_message_size)),

        config:set("log", "max_message_size", "this is not a valid size"),
        couch_log_test_util:wait_for_config(),
        ?assertEqual(16000, couch_log_config:get(max_message_size)),

        config:delete("log", "max_message_size"),
        couch_log_test_util:wait_for_config(),
        ?assertEqual(16000, couch_log_config:get(max_message_size))
    end).
