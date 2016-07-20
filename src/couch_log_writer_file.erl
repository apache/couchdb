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

-module(couch_log_writer_file).
-behaviour(couch_log_writer).


-export([
    init/0,
    terminate/2,
    write/2
]).


-include_lib("kernel/include/file.hrl").
-include("couch_log.hrl").


-record(st, {
    file_path,
    fd,
    inode,
    last_check
}).


-define(CHECK_INTERVAL, 30000000).


-ifdef(TEST).
-compile(export_all).
-endif.


init() ->
    FilePath = config:get("log", "file", "./couch.log"),
    Opts = [append, raw] ++ buffer_opt(),
    case filelib:ensure_dir(FilePath) of
        ok ->
            case file:open(FilePath, Opts) of
                {ok, Fd} ->
                    case file:read_file_info(FilePath) of
                        {ok, FInfo} ->
                            {ok, #st{
                                file_path = FilePath,
                                fd = Fd,
                                inode = FInfo#file_info.inode,
                                last_check = os:timestamp()
                            }};
                        FInfoError ->
                            ok = file:close(Fd),
                            FInfoError
                    end;
                OpenError ->
                    OpenError
            end;
        EnsureDirError ->
            EnsureDirError
    end.


terminate(_, St) ->
    % Apparently delayed_write can require two closes
    file:close(St#st.fd),
    file:close(St#st.fd),
    ok.


write(Entry, St) ->
    {ok, NewSt} = maybe_reopen(St),
    #log_entry{
        level = Level,
        pid = Pid,
        msg = Msg,
        msg_id = MsgId,
        time_stamp = TimeStamp
    } = Entry,
    Fmt = "[~s] ~s ~s ~p ~s ",
    Args = [
        couch_log_util:level_to_string(Level),
        TimeStamp,
        node(),
        Pid,
        MsgId
    ],
    MsgSize = couch_log_config:get(max_message_size),
    Data = couch_log_trunc_io:format(Fmt, Args, MsgSize),
    ok = file:write(NewSt#st.fd, [Data, Msg, "\n"]),
    {ok, NewSt}.


buffer_opt() ->
    WriteBuffer = config:get_integer("log", "write_buffer", 0),
    WriteDelay = config:get_integer("log", "write_delay", 0),
    case {WriteBuffer, WriteDelay} of
        {B, D} when is_integer(B), is_integer(D), B > 0, D > 0 ->
            [{delayed_write, B, D}];
        _ ->
            []
    end.


maybe_reopen(St) ->
    #st{
        last_check = LastCheck
    } = St,
    Now = os:timestamp(),
    case timer:now_diff(Now, LastCheck) > ?CHECK_INTERVAL of
        true -> reopen(St);
        false -> {ok, St}
    end.


reopen(St) ->
    case file:read_file_info(St#st.file_path) of
        {ok, FInfo} ->
            NewINode = FInfo#file_info.inode,
            case NewINode == St#st.inode of
                true ->
                    % No rotate necessary
                    {ok, St};
                false ->
                    % File was moved and re-created
                    terminate(rotating, St),
                    init()
            end;
        _ ->
            % File was moved or deleted
            terminate(rotating, St),
            init()
    end.
