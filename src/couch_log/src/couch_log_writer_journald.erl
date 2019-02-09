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

-module(couch_log_writer_journald).
-behaviour(couch_log_writer).


-export([
    init/0,
    terminate/2,
    write/2
]).


-include("couch_log.hrl").


init() ->
    {ok, nil}.


terminate(_, _St) ->
    ok.


% log level mapping from sd-daemon(3)
% https://www.freedesktop.org/software/systemd/man/sd-daemon.html
-spec level_to_integer(atom() | string() | integer()) -> integer().
level_to_integer(debug)                 -> 7;
level_to_integer(info)                  -> 6;
level_to_integer(notice)                -> 5;
level_to_integer(warning)               -> 4;
level_to_integer(warn)                  -> 4;
level_to_integer(error)                 -> 3;
level_to_integer(err)                   -> 3;
level_to_integer(critical)              -> 2;
level_to_integer(crit)                  -> 2;
level_to_integer(alert)                 -> 1;
level_to_integer(emergency)             -> 0;
level_to_integer(emerg)                 -> 0;
level_to_integer(none)                  -> 6.

write(Entry, St) ->
    #log_entry{
        level = Level,
        pid = Pid,
        msg = Msg,
        msg_id = MsgId
    } = Entry,
    Fmt = "<~B>~s ~p ~s ",
    Args = [
        level_to_integer(Level),
        node(),
        Pid,
        MsgId
    ],
    MsgSize = couch_log_config:get(max_message_size),
    Data = couch_log_trunc_io:format(Fmt, Args, MsgSize),
    io:format(standard_error, [Data, Msg, "\n"], []),
    {ok, St}.
