% Copyright 2011 Cloudant
%
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

-module(twig_event_handler).

-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2,
        code_change/3]).

-import(twig_util, [get_env/2]).

-record(state, {
    socket,
    host,
    port,
    hostname,
    os_pid,
    appid,
    facility,
    level
}).

-include("twig_int.hrl").

init([]) ->
    {ok, Socket} = gen_udp:open(0),
    {ok, ok, State} = handle_call(load_config, #state{socket=Socket}),
    {ok, State}.

handle_event(#twig{level=Level, msgid=MsgId, msg=Msg, pid=Pid}, State) ->
    write(Level, MsgId, Msg, Pid, State),
    {ok, State};

% OTP standard events
handle_event({Class, _GL, {Pid, Format, Args}}, #state{level=Max} = State) ->
    case otp_event_level(Class, Format) of
        undefined ->
            {ok, State};
        Level when Level > Max ->
            {ok, State};
        Level ->
            {MsgId, Msg} = message(Format, Args),
            write(Level, MsgId, Msg, Pid, State),
            {ok, State}
    end;

handle_event(_Event, State) ->
    {ok, State}.

handle_call({set_level, Level}, State) ->
    {ok, ok, State#state{level = Level}};

handle_call(load_config, State) ->
    Host = case inet:getaddr(get_env(host, undefined), inet) of
    {ok, Address} ->
        Address;
    {error, _} ->
        undefined
    end,
    NewState = State#state{
        host = Host,
        port = get_env(port, 514),
        hostname = net_adm:localhost(),
        os_pid = os:getpid(),
        appid = get_env(appid, "twig"),
        facility = twig_util:facility(get_env(facility, local2)),
        level = twig_util:level(get_env(level, info))
    },
    {ok, ok, NewState};

handle_call(_Call, State) ->
    {ok, ignored, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, State) ->
    gen_udp:close(State#state.socket).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

write(Level, undefined, Msg, Pid, State) ->
    write(Level, "--------", Msg, Pid, State);
write(Level, MsgId, Msg, Pid, State) when is_list(Msg); is_binary(Msg) ->
    #state{facility=Facil, appid=App, hostname=Hostname, host=Host, port=Port,
        socket=Socket} = State,
    Pre = io_lib:format("<~B>~B ~s ~s ~s ~p ~s - ", [Facil bor Level,
        ?SYSLOG_VERSION, twig_util:iso8601_timestamp(), Hostname, App, Pid,
        MsgId]),
    send(Socket, Host, Port, [Pre, Msg, $\n]).

send(_, undefined, _, Packet) ->
    io:put_chars(Packet);
send(Socket, Host, Port, Packet) ->
    gen_udp:send(Socket, Host, Port, Packet).

message(crash_report, Report) ->
    Msg = case erts_debug:flat_size(Report) > get_env(max_term_size, 8192) of
        true ->
            MaxString = get_env(max_message_size, 16000),
            ["*Truncated* - ", trunc_io:print(Report, MaxString)];
        false ->
            proc_lib:format(Report)
    end,
    {crash_report, Msg};
message(supervisor_report, Report) ->
    Name = get_value(supervisor, Report),
    Error = get_value(errorContext, Report),
    Reason = get_value(reason, Report),
    Offender = get_value(offender, Report),
    ChildPid = get_value(pid, Offender),
    ChildName = get_value(name, Offender),
    case get_value(mfa, Offender) of
        undefined ->
            {M,F,_} = get_value(mfargs, Offender);
        {M,F,_} ->
            ok
    end,
    {supervisor_report, twig_util:format("~p ~p (~p) child: ~p [~p] ~p:~p",
            [Name, Error, Reason, ChildName, ChildPid, M, F])};
message(Type, Report) when Type == std_error;
                           Type == std_info;
                           Type == std_warning;
                           Type == progress_report;
                           Type == progress ->
    {Type, twig_util:format("~2048.0p", [Report])};
message(Format, Args) when is_list(Format) ->
    {msg, twig_util:format(Format, Args)};
message(Format, Args) ->
    {unknown, twig_util:format("~2048.0p ~2048.0p", [Format, Args])}.

otp_event_level(_, crash_report) ->         ?LEVEL_CRIT;
otp_event_level(_, supervisor_report) ->    ?LEVEL_WARN;
otp_event_level(_, supervisor) ->           ?LEVEL_WARN;
otp_event_level(_, progress_report) ->      ?LEVEL_DEBUG;
otp_event_level(_, progress) ->             ?LEVEL_DEBUG;
otp_event_level(error, _) ->                ?LEVEL_ERR;
otp_event_level(warning_msg, _) ->          ?LEVEL_WARN;
otp_event_level(info_msg, _) ->             ?LEVEL_NOTICE;
otp_event_level(error_report, _) ->         ?LEVEL_ERR;
otp_event_level(warning_report, _) ->       ?LEVEL_WARN;
otp_event_level(info_report, _) ->          ?LEVEL_NOTICE;
otp_event_level(_, _) ->                    ?LEVEL_DEBUG.

get_value(Key, Props) ->
    case lists:keyfind(Key, 1, Props) of
        {Key, Value} ->
            Value;
        false ->
            undefined
    end.
