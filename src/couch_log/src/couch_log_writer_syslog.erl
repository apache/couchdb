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
    facility
}).


-define(SYSLOG_VERSION, 1).


-ifdef(TEST).
-compile(export_all).
-endif.


init() ->
    {ok, Socket} = gen_udp:open(0),

    Host = case config:get("log", "syslog_host") of
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

    {ok, #st{
        socket = Socket,
        host = Host,
        port = config:get_integer("log", "syslog_port", 514),
        hostname = net_adm:localhost(),
        os_pid = os:getpid(),
        appid = config:get("log", "syslog_appid", "couchdb"),
        facility = get_facility(config:get("log", "syslog_facility", "local2"))
    }}.


terminate(_Reason, St) ->
    gen_udp:close(St#st.socket).


write(Entry, St) ->
    #log_entry{
        level = Level,
        pid = Pid,
        msg = Msg,
        msg_id = MsgId,
        time_stamp = TimeStamp
    } = Entry,
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


send(#st{host=undefined}, Packet) ->
    io:format(standard_error, "~s", [Packet]);

send(St, Packet) ->
    #st{
        socket = Socket,
        host = Host,
        port = Port
    } = St,
    gen_udp:send(Socket, Host, Port, Packet).


get_facility(Name) ->
    FacId = case Name of
        "kern"      ->  0; % Kernel messages
        "user"      ->  1; % Random user-level messages
        "mail"      ->  2; % Mail system
        "daemon"    ->  3; % System daemons
        "auth"      ->  4; % Security/Authorization messages
        "syslog"    ->  5; % Internal Syslog messages
        "lpr"       ->  6; % Line printer subsystem
        "news"      ->  7; % Network news subsystems
        "uucp"      ->  8; % UUCP subsystem
        "clock"     ->  9; % Clock daemon
        "authpriv"  -> 10; % Security/Authorization messages
        "ftp"       -> 11; % FTP daemon
        "ntp"       -> 12; % NTP subsystem
        "audit"     -> 13; % Log audit
        "alert"     -> 14; % Log alert
        "cron"      -> 15; % Scheduling daemon
        "local0"    -> 16; % Local use 0
        "local1"    -> 17; % Local use 1
        "local2"    -> 18; % Local use 2
        "local3"    -> 19; % Local use 3
        "local4"    -> 20; % Local use 4
        "local5"    -> 21; % Local use 5
        "local6"    -> 22; % Local use 6
        "local7"    -> 23; % Local use 7
        _ ->
            try list_to_integer(Name) of
                N when N >= 0, N =< 23 -> N;
                _ -> 23
            catch _:_ ->
                23
            end
    end,
    FacId bsl 3.


get_level(Name) when is_atom(Name) ->
    case Name of
        debug       -> 7;
        info        -> 6;
        notice      -> 5;
        warning     -> 4;
        error       -> 3;
        critical    -> 2;
        alert       -> 1;
        emergency   -> 0;
        _           -> 3
    end.
