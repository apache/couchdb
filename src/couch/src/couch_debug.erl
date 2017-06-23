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

-module(couch_debug).

-export([
    opened_files/0,
    opened_files_by_regexp/1,
    opened_files_contains/1
]).

-export([
    tired_by_kind/2,
    tired_by_kind/3,
    process_by_kind/1,
    process_by_kind/2
]).



-spec opened_files() ->
    [{port(), CouchFilePid :: pid(), Fd :: pid() | tuple(), FilePath :: string()}].

opened_files() ->
    Info = [couch_file_port_info(Port)
        || Port <- erlang:ports(),
            {name, "efile"} =:= erlang:port_info(Port, name)],
    [I || I <- Info, is_tuple(I)].

couch_file_port_info(Port) ->
    {connected, Pid} = erlang:port_info(Port, connected),
    case couch_file:process_info(Pid) of
        {Fd, FilePath} ->
            {Port, Pid, Fd, FilePath};
        undefined ->
            undefined
    end.

-spec opened_files_by_regexp(FileRegExp :: iodata()) ->
    [{port(), CouchFilePid :: pid(), Fd :: pid() | tuple(), FilePath :: string()}].
opened_files_by_regexp(FileRegExp) ->
    {ok, RegExp} = re:compile(FileRegExp),
    lists:filter(fun({_Port, _Pid, _Fd, Path}) ->
        re:run(Path, RegExp) =/= nomatch
    end, couch_debug:opened_files()).

-spec opened_files_contains(FileNameFragment :: iodata()) ->
    [{port(), CouchFilePid :: pid(), Fd :: pid() | tuple(), FilePath :: string()}].
opened_files_contains(FileNameFragment) ->
    lists:filter(fun({_Port, _Pid, _Fd, Path}) ->
        string:str(Path, FileNameFragment) > 0
    end, couch_debug:opened_files()).

tired_by_kind(Kind, Threshold) ->
    tired_by_kind(Kind, erlang:processes(), Threshold).

tired_by_kind(Kind, Pids, Threshold) ->
    lists:filter(fun(Pid) ->
        is_kind(Pid, Kind) andalso is_tired(Pid, Threshold)
    end, Pids).

process_by_kind(Kind) ->
    process_by_kind(Kind, erlang:processes()).

process_by_kind(Kind, Pids) ->
    lists:filter(fun(Pid) ->
        is_kind(Pid, Kind)
    end, Pids).

is_tired(Pid, Threshold) ->
    case erlang:process_info(Pid, message_queue_len) of
        {message_queue_len, Len} -> Len >= Threshold;
        _ -> false
    end.

is_kind(Pid, Kind) ->
    couch_util:process_dict_get(Pid, kind) =:= Kind.
