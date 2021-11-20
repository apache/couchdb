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

write(Entry, St) ->
    #log_entry{
        level = Level,
        pid = Pid,
        msg = Msg,
        msg_id = MsgId
    } = Entry,
    Fmt = "<~B>~s ~p ~s ",
    Args = [
        level_for_journald(Level),
        node(),
        Pid,
        MsgId
    ],
    MsgSize = couch_log_config:get(max_message_size),
    Data = couch_log_trunc_io:format(Fmt, Args, MsgSize),
    io:format(standard_error, [Data, Msg, "\n"], []),
    {ok, St}.

% log level mapping from sd-daemon(3)
% https://www.freedesktop.org/software/systemd/man/sd-daemon.html
-spec level_for_journald(atom()) -> integer().
level_for_journald(Level) when is_atom(Level) ->
    case Level of
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
