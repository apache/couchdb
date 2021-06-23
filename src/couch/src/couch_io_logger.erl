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

-module(couch_io_logger).

-export([
    start/1,
    log_output/1,
    log_input/1,
    stop_noerror/0,
    stop_error/1
]).

start(undefined) ->
    ok;
start(Dir) ->
    case filelib:is_dir(Dir) of
        true ->
            Name = log_name(),
            Path = Dir ++ "/" ++ Name,
            OPath = Path ++ ".out.log_",
            IPath = Path ++ ".in.log_",
            {ok, OFd} = file:open(OPath, [read, write, raw]),
            {ok, IFd} = file:open(IPath, [read, write, raw]),
            ok = file:delete(OPath),
            ok = file:delete(IPath),
            put(logger_path, Path),
            put(logger_out_fd, OFd),
            put(logger_in_fd, IFd),
            ok;
        false ->
            ok
    end.

stop_noerror() ->
    case get(logger_path) of
        undefined ->
            ok;
        _Path ->
            close_logs()
    end.

stop_error(Err) ->
    case get(logger_path) of
        undefined ->
            ok;
        Path ->
            save_error_logs(Path, Err),
            close_logs()
    end.

log_output(Data) ->
    log(get(logger_out_fd), Data).

log_input(Data) ->
    log(get(logger_in_fd), Data).

unix_time() ->
    {Mega, Sec, USec} = os:timestamp(),
    UnixTs = (Mega * 1000000 + Sec) * 1000000 + USec,
    integer_to_list(UnixTs).

log_name() ->
    Ts = unix_time(),
    Pid0 = erlang:pid_to_list(self()),
    Pid1 = string:strip(Pid0, left, $<),
    Pid2 = string:strip(Pid1, right, $>),
    lists:flatten(io_lib:format("~s_~s", [Ts, Pid2])).

close_logs() ->
    file:close(get(logger_out_fd)),
    file:close(get(logger_in_fd)).

save_error_logs(Path, Err) ->
    Otp = erlang:system_info(otp_release),
    Msg = io_lib:format("Error: ~p~nNode: ~p~nOTP: ~p~n", [Err, node(), Otp]),
    file:write_file(Path ++ ".meta", Msg),
    IFd = get(logger_out_fd),
    OFd = get(logger_in_fd),
    file:position(IFd, 0),
    file:position(OFd, 0),
    file:copy(IFd, Path ++ ".out.log"),
    file:copy(OFd, Path ++ ".in.log").

log(undefined, _Data) ->
    ok;
log(Fd, Data) ->
    ok = file:write(Fd, [Data, io_lib:nl()]).
