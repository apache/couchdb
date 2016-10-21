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
    opened_files_by_regexp/1
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
