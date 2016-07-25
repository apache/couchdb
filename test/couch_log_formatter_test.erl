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

-module(couch_log_formatter_test).


-include("couch_log.hrl").
-include_lib("eunit/include/eunit.hrl").


truncate_fmt_test() ->
    Msg = [0 || _ <- lists:seq(1, 1048576)],
    Entry = couch_log_formatter:format(info, self(), "~w", [Msg]),
    ?assert(length(Entry#log_entry.msg) =< 16000).


truncate_test() ->
    Msg = [0 || _ <- lists:seq(1, 1048576)],
    Entry = couch_log_formatter:format(info, self(), Msg),
    ?assert(length(Entry#log_entry.msg) =< 16000).


format_reason_test() ->
    MsgFmt = "This is a reason: ~r",
    Reason = {foo, [{x, k, 3}, {c, d, 2}]},
    Entry = couch_log_formatter:format(info, self(), MsgFmt, [Reason]),
    Formatted = "This is a reason: foo at x:k/3 <= c:d/2",
    ?assertEqual(Formatted, lists:flatten(Entry#log_entry.msg)).


gen_server_error_test() ->
    Pid = self(),
    Event = {
        error,
        erlang:group_leader(),
        {
            Pid,
            "** Generic server and some stuff",
            [a_gen_server, {foo, bar}, server_state, some_reason]
        }
    },
    ?assertMatch(
        #log_entry{
            level = error,
            pid = Pid
        },
        do_format(Event)
    ),
    do_matches(do_format(Event), [
        "gen_server a_gen_server terminated",
        "with reason: some_reason",
        "last msg: {foo,bar}",
        "state: server_state"
    ]).


gen_fsm_error_test() ->
    Pid = self(),
    Event = {
        error,
        erlang:group_leader(),
        {
            Pid,
            "** State machine did a thing",
            [a_gen_fsm, {ohai,there}, state_name, curr_state, barf]
        }
    },
    ?assertMatch(
        #log_entry{
            level = error,
            pid = Pid
        },
        do_format(Event)
    ),
    do_matches(do_format(Event), [
        "gen_fsm a_gen_fsm in state state_name",
        "with reason: barf",
        "last msg: {ohai,there}",
        "state: curr_state"
    ]).


gen_event_error_test() ->
    Pid = self(),
    Event = {
        error,
        erlang:group_leader(),
        {
            Pid,
            "** gen_event handler did a thing",
            [
                handler_id,
                a_gen_event,
                {ohai,there},
                curr_state,
                barf
            ]
        }
    },
    ?assertMatch(
        #log_entry{
            level = error,
            pid = Pid
        },
        do_format(Event)
    ),
    do_matches(do_format(Event), [
        "gen_event handler_id installed in a_gen_event",
        "reason: barf",
        "last msg: {ohai,there}",
        "state: curr_state"
    ]).


emulator_error_test() ->
    Event = {
        error,
        erlang:group_leader(),
        {
            emulator,
            "~s~n",
            ["A process died and stuff\n"]
        }
    },
    ?assertMatch(
        #log_entry{
            level = error,
            pid = emulator,
            msg = "A process died and stuff"
        },
        do_format(Event)
    ).


normal_error_test() ->
    Pid = self(),
    Event = {
        error,
        erlang:group_leader(),
        {
            Pid,
            "format thing: ~w ~w",
            [
                first_arg,
                second_arg
            ]
        }
    },
    ?assertMatch(
        #log_entry{
            level = error,
            pid = Pid,
            msg = "format thing: first_arg second_arg"
        },
        do_format(Event)
    ).


error_report_std_error_test() ->
    Pid = self(),
    Event = {
        error_report,
        erlang:group_leader(),
        {
            Pid,
            std_error,
            [foo, {bar, baz}]
        }
    },
    ?assertMatch(
        #log_entry{
            level = error,
            pid = Pid,
            msg = "foo, bar: baz"
        },
        do_format(Event)
    ).


supervisor_report_test() ->
    Pid = self(),
    % A standard supervisor report
    Event1 = {
        error_report,
        erlang:group_leader(),
        {
            Pid,
            supervisor_report,
            [
                {supervisor, sup_name},
                {offender, [
                    {id, sup_child},
                    {pid, list_to_pid("<0.1.0>")},
                    {mfargs, {some_mod, some_fun, 3}}
                ]},
                {reason, a_reason},
                {errorContext, some_context}
            ]
        }
    },
    ?assertMatch(
        #log_entry{
            level = error,
            pid = Pid
        },
        do_format(Event1)
    ),
    do_matches(do_format(Event1), [
        "Supervisor sup_name",
        "had child sup_child started with some_mod:some_fun/3 at <0.1.0> exit",
        "with reason a_reason",
        "in context some_context"
    ]),
    % Slightly older using name instead of id
    % in the offender blob.
    Event2 = {
        error_report,
        erlang:group_leader(),
        {
            Pid,
            supervisor_report,
            [
                {supervisor, sup_name},
                {offender, [
                    {name, sup_child},
                    {pid, list_to_pid("<0.1.0>")},
                    {mfargs, {some_mod, some_fun, 3}}
                ]},
                {reason, a_reason},
                {errorContext, some_context}
            ]
        }
    },
    ?assertMatch(
        #log_entry{
            level = error,
            pid = Pid
        },
        do_format(Event2)
    ),
    do_matches(do_format(Event2), [
        "Supervisor sup_name",
        "had child sup_child started with some_mod:some_fun/3 at <0.1.0> exit",
        "with reason a_reason",
        "in context some_context"
    ]),
    % A supervisor_bridge
    Event3 = {
        error_report,
        erlang:group_leader(),
        {
            Pid,
            supervisor_report,
            [
                {supervisor, sup_name},
                {offender, [
                    {mod, bridge_mod},
                    {pid, list_to_pid("<0.1.0>")}
                ]},
                {reason, a_reason},
                {errorContext, some_context}
            ]
        }
    },
    ?assertMatch(
        #log_entry{
            level = error,
            pid = Pid
        },
        do_format(Event3)
    ),
    do_matches(do_format(Event3), [
        "Supervisor sup_name",
        "had child at module bridge_mod at <0.1.0> exit",
        "with reason a_reason",
        "in context some_context"
    ]),
    % Any other supervisor report
    Event4 = {
        error_report,
        erlang:group_leader(),
        {
            Pid,
            supervisor_report,
            [foo, {a, thing}, bang]
        }
    },
    ?assertMatch(
        #log_entry{
            level = error,
            pid = Pid,
            msg = "SUPERVISOR REPORT foo, a: thing, bang"
        },
        do_format(Event4)
    ).


crash_report_test() ->
    Pid = self(),
    % A standard crash report
    Event1 = {
        error_report,
        erlang:group_leader(),
        {
            Pid,
            crash_report,
            [
                [
                    {pid, list_to_pid("<0.2.0>")},
                    {error_info, {
                        exit,
                        undef,
                        [{mod_name, fun_name, [a, b]}]
                    }}
                ],
                [list_to_pid("<0.3.0>"), list_to_pid("<0.4.0>")]
            ]
        }
    },
    ?assertMatch(
        #log_entry{
            level = error,
            pid = Pid
        },
        do_format(Event1)
    ),
    do_matches(do_format(Event1), [
        "Process <0.2.0>",
        "with 2 neighbors",
        "exited",
        "reason: call to undefined function mod_name:fun_name\\(a, b\\)"
    ]),
    % A registered process crash report
    Event2 = {
        error_report,
        erlang:group_leader(),
        {
            Pid,
            crash_report,
            [
                [
                    {pid, list_to_pid("<0.2.0>")},
                    {registered_name, couch_log_server},
                    {error_info, {
                        exit,
                        undef,
                        [{mod_name, fun_name, [a, b]}]
                    }}
                ],
                [list_to_pid("<0.3.0>"), list_to_pid("<0.4.0>")]
            ]
        }
    },
    do_matches(do_format(Event2), [
        "Process couch_log_server \\(<0.2.0>\\)"
    ]),
    % A non-exit crash report
    Event3 = {
        error_report,
        erlang:group_leader(),
        {
            Pid,
            crash_report,
            [
                [
                    {pid, list_to_pid("<0.2.0>")},
                    {registered_name, couch_log_server},
                    {error_info, {
                        killed,
                        undef,
                        [{mod_name, fun_name, [a, b]}]
                    }}
                ],
                [list_to_pid("<0.3.0>"), list_to_pid("<0.4.0>")]
            ]
        }
    },
    do_matches(do_format(Event3), [
        "crashed"
    ]),
    % A extra report info
    Event4 = {
        error_report,
        erlang:group_leader(),
        {
            Pid,
            crash_report,
            [
                [
                    {pid, list_to_pid("<0.2.0>")},
                    {error_info, {
                        killed,
                        undef,
                        [{mod_name, fun_name, [a, b]}]
                    }},
                    {another, entry},
                    yep
                ],
                [list_to_pid("<0.3.0>"), list_to_pid("<0.4.0>")]
            ]
        }
    },
    do_matches(do_format(Event4), [
        "; another: entry, yep"
    ]).


warning_report_test() ->
    Pid = self(),
    % A warning message
    Event1 = {
        warning_msg,
        erlang:group_leader(),
        {
            Pid,
            "a ~s string ~w",
            ["format", 7]
        }
    },
    ?assertMatch(
        #log_entry{
            level = warning,
            pid = Pid,
            msg = "a format string 7"
        },
        do_format(Event1)
    ),
    % A warning report
    Event2 = {
        warning_report,
        erlang:group_leader(),
        {
            Pid,
            std_warning,
            [list, 'of', {things, indeed}]
        }
    },
    ?assertMatch(
        #log_entry{
            level = warning,
            pid = Pid,
            msg = "list, of, things: indeed"
        },
        do_format(Event2)
    ).


info_report_test() ->
    Pid = self(),
    % An info message
    Event1 = {
        info_msg,
        erlang:group_leader(),
        {
            Pid,
            "an info ~s string ~w",
            ["format", 7]
        }
    },
    ?assertMatch(
        #log_entry{
            level = info,
            pid = Pid,
            msg = "an info format string 7"
        },
        do_format(Event1)
    ),
    % Application exit info
    Event2 = {
        info_report,
        erlang:group_leader(),
        {
            Pid,
            std_info,
            [
                {type, no_idea},
                {application, couch_log},
                {exited, red_sox_are_on}
            ]
        }
    },
    ?assertMatch(
        #log_entry{
            level = info,
            pid = Pid,
            msg = "Application couch_log exited with reason: red_sox_are_on"
        },
        do_format(Event2)
    ),
    % Any other std_info message
    Event3 = {
        info_report,
        erlang:group_leader(),
        {
            Pid,
            std_info,
            [
                {type, no_idea},
                {application, couch_log}
            ]
        }
    },
    ?assertMatch(
        #log_entry{
            level = info,
            pid = Pid,
            msg = "type: no_idea, application: couch_log"
        },
        do_format(Event3)
    ),
    % Non-list other report
    Event4 = {
        info_report,
        erlang:group_leader(),
        {
            Pid,
            std_info,
            dang
        }
    },
    ?assertMatch(
        #log_entry{
            level = info,
            pid = Pid,
            msg = "dang"
        },
        do_format(Event4)
    ).


progress_report_test() ->
    Pid = self(),
    % Application started
    Event1 = {
        info_report,
        erlang:group_leader(),
        {
            Pid,
            progress,
            [{started_at, 'nonode@nohost'}, {application, app_name}]
        }
    },
    ?assertMatch(
        #log_entry{
            level = info,
            pid = Pid,
            msg = "Application app_name started on node nonode@nohost"
        },
        do_format(Event1)
    ),
    % Supervisor started child
    Event2 = {
        info_report,
        erlang:group_leader(),
        {
            Pid,
            progress,
            [
                {supervisor, sup_dude},
                {started, [
                    {mfargs, {mod_name, fun_name, 1}},
                    {pid, list_to_pid("<0.5.0>")}
                ]}
            ]
        }
    },
    ?assertMatch(
        #log_entry{
            level = debug,
            pid = Pid,
            msg = "Supervisor sup_dude started mod_name:fun_name/1"
                    " at pid <0.5.0>"
        },
        do_format(Event2)
    ),
    %  Other progress report
    Event3 = {
        info_report,
        erlang:group_leader(),
        {
            Pid,
            progress,
            [a, {thing, boop}, here]
        }
    },
    ?assertMatch(
        #log_entry{
            level = info,
            pid = Pid,
            msg = "PROGRESS REPORT a, thing: boop, here"
        },
        do_format(Event3)
    ).


log_unknown_event_test() ->
    Pid = self(),
    ?assertMatch(
        #log_entry{
            level = warning,
            pid = Pid,
            msg = "Unexpected error_logger event an_unknown_event"
        },
        do_format(an_unknown_event)
    ).


format_reason_test_() ->
    Cases = [
        {
            {'function not exported', [{a, b, 2}, {c, d, 1}, {e, f, 2}]},
            "call to unexported function a:b/2 at c:d/1 <= e:f/2"
        },
        {
            {'function not exported', [{a, b, 2, []}, {c, d, 1}, {e, f, 2}]},
            "call to unexported function a:b/2 at c:d/1 <= e:f/2"
        },
        {
            {undef, [{a, b, 2, []}, {c, d, 1}, {e, f, 2}]},
            "call to undefined function a:b/2 at c:d/1 <= e:f/2"
        },
        {
            {bad_return, {{a, b, 2}, {'EXIT', killed}}},
            "bad return value {'EXIT',killed} from a:b/2"
        },
        {
            {bad_return_value, foo},
            "bad return value foo"
        },
        {
            {{bad_return_value, foo}, {h, i, 0}},
            "bad return value foo at h:i/0"
        },
        {
            {{badrecord, {foo, 1, 4}}, [{h, i, 0}, {j, k, [a, b]}]},
            "bad record {foo,1,4} at h:i/0 <= j:k/2"
        },
        {
            {{case_clause, bingo}, [{j, k, 3}, {z, z, 0}]},
            "no case clause matching bingo at j:k/3 <= z:z/0"
        },
        {
            {function_clause, [{j, k, [a, 2]}, {y, x, 1}]},
            "no function clause matching j:k(a, 2) at y:x/1"
        },
        {
            {if_clause, [{j, k, [a, 2]}, {y, x, 1}]},
            "no true branch found while evaluating if expression at j:k/2 <= y:x/1"
        },
        {
            {{try_clause, bango}, [{j, k, [a, 2]}, {y, x, 1}]},
            "no try clause matching bango at j:k/2 <= y:x/1"
        },
        {
            {badarith, [{j, k, [a, 2]}, {y, x, 1}]},
            "bad arithmetic expression at j:k/2 <= y:x/1"
        },
        {
            {{badmatch, bongo}, [{j, k, [a, 2]}, {y, x, 1}]},
            "no match of right hand value bongo at j:k/2 <= y:x/1"
        },
        {
            {emfile, [{j, k, [a, 2]}, {y, x, 1}]},
            "maximum number of file descriptors exhausted, check ulimit -n; j:k/2 <= y:x/1"
        },
        {
            {system_limit, [{erlang, open_port, []}, {y, x, 1}]},
            "system limit: maximum number of ports exceeded at y:x/1"
        },
        {
            {system_limit, [{erlang, spawn, []}, {y, x, 1}]},
            "system limit: maximum number of processes exceeded at y:x/1"
        },
        {
            {system_limit, [{erlang, spawn_opt, []}, {y, x, 1}]},
            "system limit: maximum number of processes exceeded at y:x/1"
        },
        {
            {system_limit, [{erlang, list_to_atom, ["foo"]}, {y, x, 1}]},
            "system limit: tried to create an atom larger than 255, or maximum atom count exceeded at y:x/1"
        },
        {
            {system_limit, [{ets, new, []}, {y, x, 1}]},
            "system limit: maximum number of ETS tables exceeded at y:x/1"
        },
        {
            {system_limit, [{couch_log, totes_logs, []}, {y, x, 1}]},
            "system limit: couch_log:totes_logs() at y:x/1"
        },
        {
            {badarg, [{j, k, [a, 2]}, {y, x, 1}]},
            "bad argument in call to j:k(a, 2) at y:x/1"
        },
        {
            {{badarg, [{j, k, [a, 2]}, {y, x, 1}]}, some_ignored_thing},
            "bad argument in call to j:k(a, 2) at y:x/1"
        },
        {
            {{badarity, {fun erlang:spawn/1, [a, b]}}, [{y, x, 1}]},
            "function called with wrong arity of 2 instead of 1 at y:x/1"
        },
        {
            {noproc, [{y, x, 1}]},
            "no such process or port in call to y:x/1"
        },
        {
            {{badfun, 2}, [{y, x, 1}]},
            "bad function 2 called at y:x/1"
        },
        {
            {a_reason, [{y, x, 1}]},
            "a_reason at y:x/1"
        },
        {
            {a_reason, [{y, x, 1, [{line, 4}]}]},
            "a_reason at y:x/1(line:4)"
        }
    ],
    [
        {Msg, fun() -> ?assertEqual(
            Msg,
            lists:flatten(couch_log_formatter:format_reason(Reason))
        ) end}
        || {Reason, Msg} <- Cases
    ].


coverage_test() ->
    % MFA's that aren't
    ?assertEqual(["foo"], couch_log_formatter:format_mfa(foo)),

    % Traces with line numbers
    Trace = [{x, y, [a], [{line, 4}]}],
    ?assertEqual(
        "x:y/1(line:4)",
        lists:flatten(couch_log_formatter:format_trace(Trace))
    ),

    % Excercising print_silly_list
    ?assertMatch(
        #log_entry{
            level = error,
            msg = "foobar"
        },
        do_format({
            error_report,
            erlang:group_leader(),
            {self(), std_error, "foobar"}
        })
    ),

    % Excercising print_silly_list
    ?assertMatch(
        #log_entry{
            level = error,
            msg = "dang"
        },
        do_format({
            error_report,
            erlang:group_leader(),
            {self(), std_error, dang}
        })
    ).


do_format(Event) ->
    E = couch_log_formatter:format(Event),
    E#log_entry{
        msg = lists:flatten(E#log_entry.msg),
        msg_id = lists:flatten(E#log_entry.msg_id),
        time_stamp = lists:flatten(E#log_entry.time_stamp)
    }.


do_matches(_, []) ->
    ok;

do_matches(#log_entry{msg = Msg} = E, [Pattern | RestPatterns]) ->
    case re:run(Msg, Pattern) of
        {match, _} ->
            ok;
        nomatch ->
            Err1 = io_lib:format("'~s' does not match '~s'", [Pattern, Msg]),
            Err2 = lists:flatten(Err1),
            ?assertEqual(nomatch, Err2)
    end,
    do_matches(E, RestPatterns).
