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

-module(couch_log_util_test).

-include_lib("couch_log/include/couch_log.hrl").
-include_lib("eunit/include/eunit.hrl").

get_message_id_test() ->
    ?assertEqual("--------", couch_log_util:get_msg_id()),
    erlang:put(nonce, "deadbeef"),
    ?assertEqual("deadbeef", couch_log_util:get_msg_id()),
    erlang:put(nonce, undefined).

level_to_atom_test() ->
    lists:foreach(
        fun(L) ->
            ?assert(is_atom(couch_log_util:level_to_atom(L))),
            ?assert(is_integer(couch_log_util:level_to_integer(L))),
            ?assert(is_list(couch_log_util:level_to_string(L)))
        end,
        levels()
    ).

string_p_test() ->
    ?assertEqual(false, couch_log_util:string_p([])),
    ?assertEqual(false, couch_log_util:string_p([[false]])),
    ?assertEqual(true, couch_log_util:string_p([$\n])),
    ?assertEqual(true, couch_log_util:string_p([$\r])),
    ?assertEqual(true, couch_log_util:string_p([$\t])),
    ?assertEqual(true, couch_log_util:string_p([$\v])),
    ?assertEqual(true, couch_log_util:string_p([$\b])),
    ?assertEqual(true, couch_log_util:string_p([$\f])),
    ?assertEqual(true, couch_log_util:string_p([$\e])).

levels() ->
    [
        1,
        2,
        3,
        4,
        5,
        6,
        7,
        8,
        9,
        "1",
        "2",
        "3",
        "4",
        "5",
        "6",
        "7",
        "8",
        "9",
        debug,
        info,
        notice,
        warning,
        warn,
        error,
        err,
        critical,
        crit,
        alert,
        emergency,
        emerg,
        none,
        "debug",
        "info",
        "notice",
        "warning",
        "warn",
        "error",
        "err",
        "critical",
        "crit",
        "alert",
        "emergency",
        "emerg",
        "none"
    ].
