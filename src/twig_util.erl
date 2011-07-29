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

-module(twig_util).

-export([format/2, get_env/2, level/1, facility/1, iso8601_timestamp/0]).

level(debug) ->     7;
level(info) ->      6;
level(notice) ->    5;
level(warn) ->      4;
level(warning) ->   4;
level(err) ->       3;
level(error) ->     3;
level(crit) ->      2;
level(alert) ->     1;
level(emerg) ->     0;
level(panic) ->     0;

level(I) when is_integer(I), I >= 0, I =< 7 ->
    I;
level(_BadLevel) ->
    3.

facility(kern)     -> (0 bsl 3) ; % kernel messages
facility(user)     -> (1 bsl 3) ; % random user-level messages
facility(mail)     -> (2 bsl 3) ; % mail system
facility(daemon)   -> (3 bsl 3) ; % system daemons
facility(auth)     -> (4 bsl 3) ; % security/authorization messages
facility(syslog)   -> (5 bsl 3) ; % messages generated internally by syslogd
facility(lpr)      -> (6 bsl 3) ; % line printer subsystem
facility(news)     -> (7 bsl 3) ; % network news subsystem
facility(uucp)     -> (8 bsl 3) ; % UUCP subsystem
facility(cron)     -> (9 bsl 3) ; % clock daemon
facility(authpriv) -> (10 bsl 3); % security/authorization messages (private)
facility(ftp)      -> (11 bsl 3); % ftp daemon

facility(local0)   -> (16 bsl 3);
facility(local1)   -> (17 bsl 3);
facility(local2)   -> (18 bsl 3);
facility(local3)   -> (19 bsl 3);
facility(local4)   -> (20 bsl 3);
facility(local5)   -> (21 bsl 3);
facility(local6)   -> (22 bsl 3);
facility(local7)   -> (23 bsl 3).


iso8601_timestamp() ->
    {_,_,Micro} = Now = os:timestamp(),
    {{Year,Month,Date},{Hour,Minute,Second}} = calendar:now_to_datetime(Now),
    Format = "~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B.~6.10.0BZ",
    io_lib:format(Format, [Year, Month, Date, Hour, Minute, Second, Micro]).

format(Format, Data) ->
    MaxTermSize = get_env(max_term_size, 8192),
    case erts_debug:flat_size(Data) > MaxTermSize of
        true ->
            MaxString = get_env(max_message_size, 16000),
            {Truncated, _} = trunc_io:print(Data, MaxString),
            ["*Truncated* ", Format, " - ", Truncated];
        false ->
            io_lib:format(Format, Data)
    end.

get_env(Key, Default) ->
    case application:get_env(twig, Key) of
        {ok, Value} ->
            Value;
        undefined ->
            Default
    end.
