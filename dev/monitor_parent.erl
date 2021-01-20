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

-module(monitor_parent).

-export([start/0]).


start() ->
    {ok, [[PPid]]} = init:get_argument(parent_pid),
    spawn(fun() -> monitor_parent(PPid) end).


monitor_parent(PPid) ->
    timer:sleep(1000),
    case os:type() of
        {unix, _} ->
            case os:cmd("kill -0 " ++ PPid) of
                "" ->
                    monitor_parent(PPid);
                _Else ->
                    % Assume _Else is a no such process error
                    init:stop()
            end;
        {win32, _} ->
            Fmt = "tasklist /fi \"PID eq ~s\" /fo csv /nh",
            Retval = os:cmd(io_lib:format(Fmt, [PPid])),
            case re:run(Retval, "^\"python.exe\",*") of
                {match, _} ->
                    monitor_parent(PPid);
                nomatch ->
                    init:stop()
            end
    end.
