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

-module(couch_log_writer_otp).
-behaviour(couch_log_writer).

-export([
    init/0,
    terminate/2,
    write/2
]).

-include("couch_log.hrl").

init() ->
    FilePath = config:get("log", "file_new", "./couch_new.log"),
    logger:add_handler(
        my_new_disk_log,
        logger_disk_log_h,
        #{ config => #{
                    level = info,
                    file => FilePath,
                    max_no_files => 5},
                    formatter => {
                        logger_formatter,
                        #{}
                    }
        }),
    {ok, nil}.

terminate(_, _St) ->
    ok.

write(#log_entry{} = Entry, St) ->
    #log_entry{
        level = Level,
        pid = Pid,
        msg = Msg,
        msg_id = MsgId,
        time_stamp = TimeStamp
    } = couch_log_util:maybe_format_type(Entry),
    Fmt = "TEST [~s] ~s ~s ~p ~s ",
    Args = [
        couch_log_util:level_to_string(Level),
        TimeStamp,
        config:node_name(),
        Pid,
        MsgId
    ],
    MsgSize = couch_log_config:get(max_message_size),
    Data = couch_log_trunc_io:format(Fmt, Args, MsgSize),
    logger:notice([Data, Msg, "\n"]),
    %io:format(standard_error, [Data, Msg, "\n"], []),
    {ok, St}.
