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

-module(couch_log_writer_syslog_test).

-include_lib("couch_log/include/couch_log.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(WRITER, couch_log_writer_syslog).

couch_log_writer_syslog_test_() ->
    {setup, fun couch_log_test_util:start/0, fun couch_log_test_util:stop/1, [
        fun check_init_terminate/0,
        fun() ->
            couch_log_test_util:with_meck(
                [{io, [unstick]}],
                fun check_stderr_write/0
            )
        end,
        fun() ->
            couch_log_test_util:with_meck(
                [{gen_udp, [unstick]}],
                fun check_udp_send/0
            )
        end,
        fun() ->
            couch_log_test_util:with_meck(
                [{gen_udp, [unstick]}],
                fun check_format/0
            )
        end
    ]}.

check_init_terminate() ->
    {ok, St} = ?WRITER:init(),
    ok = ?WRITER:terminate(stop, St).

check_stderr_write() ->
    meck:expect(io, format, 3, ok),

    Entry = #log_entry{
        level = debug,
        pid = list_to_pid("<0.1.0>"),
        msg = "stuff",
        msg_id = "msg_id",
        time_stamp = "time_stamp"
    },
    {ok, St} = ?WRITER:init(),
    {ok, NewSt} = ?WRITER:write(Entry, St),
    ok = ?WRITER:terminate(stop, NewSt),

    ?assert(meck:called(io, format, 3)),
    ?assert(meck:validate(io)).

check_udp_send() ->
    meck:expect(gen_udp, open, 1, {ok, socket}),
    meck:expect(gen_udp, send, 4, ok),
    meck:expect(gen_udp, close, fun(socket) -> ok end),

    config:set("log", "syslog_host", "localhost"),
    try
        Entry = #log_entry{
            level = debug,
            pid = list_to_pid("<0.1.0>"),
            msg = "stuff",
            msg_id = "msg_id",
            time_stamp = "time_stamp"
        },
        {ok, St} = ?WRITER:init(),
        {ok, NewSt} = ?WRITER:write(Entry, St),
        ok = ?WRITER:terminate(stop, NewSt)
    after
        config:delete("log", "syslog_host")
    end,

    ?assert(meck:called(gen_udp, open, 1)),
    ?assert(meck:called(gen_udp, send, 4)),
    ?assert(meck:called(gen_udp, close, 1)),
    ?assert(meck:validate(gen_udp)).

check_format() ->
    meck:expect(gen_udp, open, 1, {ok, socket}),
    meck:expect(gen_udp, send, 4, ok),
    meck:expect(gen_udp, close, fun(socket) -> ok end),
    config:set("log", "syslog_host", "localhost"),
    config:set("log", "syslog_enterprise_number", "12345"),
    try
        Entry = #log_entry{
            level = report,
            pid = list_to_pid("<0.1.0>"),
            msg = "[foo=1] stuff",
            msg_id = "msg_id",
            time_stamp = "time_stamp",
            type = report123
        },
        {ok, St} = ?WRITER:init(),
        {ok, NewSt} = ?WRITER:write(Entry, St),
        ok = ?WRITER:terminate(stop, NewSt)
    after
        config:delete("log", "syslog_host")
    end,

    ?assert(meck:called(gen_udp, open, 1)),
    Packet = lists:flatten(meck:capture(first, gen_udp, send, '_', 4)),
    [SeverityAndVsn, TS, _Host, AppId, Pid, MsgId, _ | Rest] = string:split(Packet, " ", all),
    ?assertEqual("<150>1", SeverityAndVsn),
    ?assertEqual("time_stamp", TS),
    ?assertEqual("couchdb", AppId),
    ?assertEqual("msg_id", MsgId),
    ?assert(is_pid(catch list_to_pid(Pid))),
    ?assertEqual("[report123@12345 foo=1] stuff\n", string:join(Rest, " ")),
    ?assert(meck:called(gen_udp, close, 1)),
    ?assert(meck:validate(gen_udp)).



facility_test() ->
    Names = [
        "kern",
        "user",
        "mail",
        "daemon",
        "auth",
        "syslog",
        "lpr",
        "news",
        "uucp",
        "clock",
        "authpriv",
        "ftp",
        "ntp",
        "audit",
        "alert",
        "cron",
        "local0",
        "local1",
        "local2",
        "local3",
        "local4",
        "local5",
        "local6",
        "local7"
    ],
    lists:foldl(
        fun(Name, Id) ->
            IdStr = lists:flatten(io_lib:format("~w", [Id])),
            ?assertEqual(Id bsl 3, couch_log_writer_syslog:get_facility(Name)),
            ?assertEqual(Id bsl 3, couch_log_writer_syslog:get_facility(IdStr)),
            Id + 1
        end,
        0,
        Names
    ),
    ?assertEqual(23 bsl 3, couch_log_writer_syslog:get_facility("foo")),
    ?assertEqual(23 bsl 3, couch_log_writer_syslog:get_facility("-1")),
    ?assertEqual(23 bsl 3, couch_log_writer_syslog:get_facility("24")).

level_test() ->
    Levels = [
        emergency,
        alert,
        critical,
        error,
        warning,
        notice,
        info,
        debug
    ],
    lists:foldl(
        fun(Name, Id) ->
            ?assertEqual(Id, couch_log_writer_syslog:get_level(Name)),
            Id + 1
        end,
        0,
        Levels
    ),
    ?assertEqual(3, couch_log_writer_syslog:get_level(foo)).
