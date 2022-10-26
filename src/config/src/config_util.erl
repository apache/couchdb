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

-module(config_util).

-export([abs_pathname/1]).
-export([abs_pathname/2]).
-export([implode/2]).

% given a pathname "../foo/bar/" it gives back the fully qualified
% absolute pathname.
abs_pathname(" " ++ Filename) ->
    % strip leading whitspace
    abs_pathname(Filename);
abs_pathname([$/ | _] = Filename) ->
    Filename;
abs_pathname(Filename) ->
    {ok, Cwd} = file:get_cwd(),
    {Filename2, Args} = separate_cmd_args(Filename, ""),
    abs_pathname(Filename2, Cwd) ++ Args.

abs_pathname(Filename, Dir) ->
    Name = filename:absname(Filename, Dir ++ "/"),
    OutFilename = filename:join(fix_path_list(filename:split(Name), [])),
    % If the filename is a dir (last char slash, put back end slash
    case string:right(Filename, 1) of
        "/" ->
            OutFilename ++ "/";
        "\\" ->
            OutFilename ++ "/";
        _Else ->
            OutFilename
    end.

implode(List, Sep) ->
    implode(List, Sep, []).

implode([], _Sep, Acc) ->
    lists:flatten(lists:reverse(Acc));
implode([H], Sep, Acc) ->
    implode([], Sep, [H | Acc]);
implode([H | T], Sep, Acc) ->
    implode(T, Sep, [Sep, H | Acc]).

% if this as an executable with arguments, seperate out the arguments
% ""./foo\ bar.sh -baz=blah" -> {"./foo\ bar.sh", " -baz=blah"}
separate_cmd_args("", CmdAcc) ->
    {lists:reverse(CmdAcc), ""};
% handle skipped value
separate_cmd_args("\\ " ++ Rest, CmdAcc) ->
    separate_cmd_args(Rest, " \\" ++ CmdAcc);
separate_cmd_args(" " ++ Rest, CmdAcc) ->
    {lists:reverse(CmdAcc), " " ++ Rest};
separate_cmd_args([Char | Rest], CmdAcc) ->
    separate_cmd_args(Rest, [Char | CmdAcc]).

% takes a heirarchical list of dirs and removes the dots ".", double dots
% ".." and the corresponding parent dirs.
fix_path_list([], Acc) ->
    lists:reverse(Acc);
fix_path_list([".." | Rest], [_PrevAcc | RestAcc]) ->
    fix_path_list(Rest, RestAcc);
fix_path_list(["." | Rest], Acc) ->
    fix_path_list(Rest, Acc);
fix_path_list([Dir | Rest], Acc) ->
    fix_path_list(Rest, [Dir | Acc]).
