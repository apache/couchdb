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

-module(couch_log_writer_syslog).
-behavior(couch_log_writer).

-export([
    init/0,
    terminate/2,
    write/2
]).

-include("couch_log.hrl").

-record(st, {
    socket,
    host,
    port,
    hostname,
    os_pid,
    appid,
    facility,
    report_level,
    enterprise_number
}).

-define(SYSLOG_VERSION, 1).

-ifdef(TEST).
-export([
    get_facility/1,
    get_level/1
]).
-endif.

init() ->
    {ok, Socket} = gen_udp:open(0),

    Host =
        case config:get("log", "syslog_host") of
            undefined ->
                undefined;
            SysLogHost ->
                case inet:getaddr(SysLogHost, inet) of
                    {ok, Address} ->
                        Address;
                    _ ->
                        undefined
                end
        end,
    Level = list_to_atom(config:get("log", "report_level", "info")),
    {ok, #st{
        socket = Socket,
        host = Host,
        port = config:get_integer("log", "syslog_port", 514),
        hostname = net_adm:localhost(),
        os_pid = os:getpid(),
        appid = config:get("log", "syslog_appid", "couchdb"),
        facility = get_facility(config:get("log", "syslog_facility", "local2")),
        report_level = Level,
        enterprise_number = config:get("log", "syslog_enterprise_number")
    }}.

terminate(_Reason, St) ->
    gen_udp:close(St#st.socket).

write(#log_entry{level = report} = Entry, #st{enterprise_number = undefined} = St) ->
    do_write(Entry#log_entry{level = St#st.report_level}, St);
write(#log_entry{level = report, type = Type} = Entry0, #st{report_level = Level} = St) ->
    % append @${enterprise_number} to the type to conform with
    % https://www.rfc-editor.org/rfc/rfc5424.html#page-15
    TypeSDID = lists:flatten(io_lib:format("~s-DB@~s", [Type, St#st.enterprise_number])),
    Entry = Entry0#log_entry{
        type = TypeSDID,
        level = Level
    },
    do_write(Entry, St);
write(#log_entry{} = Entry, #st{} = St) ->
    do_write(Entry, St).

do_write(Entry, St) ->
    #log_entry{
        level = Level,
        pid = Pid,
        msg = Msg,
        msg_id = MsgId,
        time_stamp = TimeStamp
    } = couch_log_util:maybe_format_type(Entry),
    Fmt = "<~B>~B ~s ~s ~s ~p ~s - ",
    Args = [
        St#st.facility bor get_level(Level),
        ?SYSLOG_VERSION,
        TimeStamp,
        St#st.hostname,
        St#st.appid,
        Pid,
        MsgId
    ],
    Pre = io_lib:format(Fmt, Args),
    ok = send(St, [Pre, Msg, $\n]),
    {ok, St}.

send(#st{host = undefined}, Packet) ->
    io:format(standard_error, "~s", [Packet]);
send(St, Packet) ->
    #st{
        socket = Socket,
        host = Host,
        port = Port
    } = St,
    gen_udp:send(Socket, Host, Port, Packet).

get_facility(Name) ->
    FacId =
        case Name of
            % Kernel messages
            "kern" ->
                0;
            % Random user-level messages
            "user" ->
                1;
            % Mail system
            "mail" ->
                2;
            % System daemons
            "daemon" ->
                3;
            % Security/Authorization messages
            "auth" ->
                4;
            % Internal Syslog messages
            "syslog" ->
                5;
            % Line printer subsystem
            "lpr" ->
                6;
            % Network news subsystems
            "news" ->
                7;
            % UUCP subsystem
            "uucp" ->
                8;
            % Clock daemon
            "clock" ->
                9;
            % Security/Authorization messages
            "authpriv" ->
                10;
            % FTP daemon
            "ftp" ->
                11;
            % NTP subsystem
            "ntp" ->
                12;
            % Log audit
            "audit" ->
                13;
            % Log alert
            "alert" ->
                14;
            % Scheduling daemon
            "cron" ->
                15;
            % Local use 0
            "local0" ->
                16;
            % Local use 1
            "local1" ->
                17;
            % Local use 2
            "local2" ->
                18;
            % Local use 3
            "local3" ->
                19;
            % Local use 4
            "local4" ->
                20;
            % Local use 5
            "local5" ->
                21;
            % Local use 6
            "local6" ->
                22;
            % Local use 7
            "local7" ->
                23;
            _ ->
                try list_to_integer(Name) of
                    N when N >= 0, N =< 23 -> N;
                    _ -> 23
                catch
                    _:_ ->
                        23
                end
        end,
    FacId bsl 3.

get_level(Name) when is_atom(Name) ->
    case Name of
        debug -> 7;
        info -> 6;
        notice -> 5;
        warning -> 4;
        error -> 3;
        critical -> 2;
        alert -> 1;
        emergency -> 0;
        _ -> 3
    end.
