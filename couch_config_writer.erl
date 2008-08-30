% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License.  You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
% License for the specific language governing permissions and limitations under
% the License.

%% @doc Saves a Key/Value pair to a ini file. The Key consists of a Module
%%      and Variable combination. If that combination is found in the ini file
%%      the new value replaces the old value. If only the Module is found the
%%      Variable and value combination is appended to the Module. If the Module
%%      does not yet exist in the ini file, it is added and the Variable/Value
%%      pair is appended.
%% @see couch_config

-module(couch_config_writer).
-include("couch_db.hrl").

-export([save_to_file/2]).

%% @spec save_to_file(
%%           Config::{{Module::string(), Variable::string()}, Value::string()},
%%           File::filename()) -> ok
%% @doc Saves a Module/Key/Value triple to the ini file File::filename()
save_to_file({{Module, Variable}, Value}, File) ->

    ?LOG_DEBUG("saving to file '~s', Config: '~p'", [File, {{Module, Variable}, Value}]),

    % open file and create a list of lines
    {ok, Stream} = file:read_file(File),
    OldFileContents = binary_to_list(Stream),
    {ok, Lines} = regexp:split(OldFileContents, "\r\n|\n|\r|\032"),

    % prepare input variables
    ModuleName = "[" ++ Module ++ "]",
    VariableList = Variable,

    % produce the contents for the config file
    NewFileContents =
    case NewFileContents2 = save_loop({{ModuleName, VariableList}, Value}, Lines, "", "", []) of
        % we didn't change anything, that means we couldn't find a matching
        % [ini section] in which case we just append a new one.
        OldFileContents ->
            append_new_ini_section({{ModuleName, VariableList}, Value}, OldFileContents);
        _ ->
            NewFileContents2
    end,

    % do the save, close the config file and get out
    save_file(File, NewFileContents),
    file:close(Stream),
    ok.

%% @doc Iterates over the lines of an ini file and replaces or adds a new
%%      configuration directive.
save_loop({{Module, Variable}, Value}, [Line|Rest], OldCurrentModule, Contents, DoneVariables) ->

    % if we find a new [ini section] (Module), save that for reference
    NewCurrentModule = parse_module(Line, OldCurrentModule),

    % if the current Module is the one we want to change, try to match
    % each line with the Variable
    NewContents = case Module of
        NewCurrentModule ->
            % see if the current line matches the variable we want to substitute
            case parse_variable(Line, Variable, Value) of
                % nope, return original line
                nomatch ->
                    DoneVariables2 = DoneVariables,
                    Line;
                % got em! return new line
                NewLine ->
                    DoneVariables2 = [Variable|DoneVariables],
                    NewLine
            end;
        % if the variable we want to change couldn't be replaced, we append it
        % in the proper module section
        OldCurrentModule ->
            case lists:member(Variable, DoneVariables) of
                false ->
                    DoneVariables2 = [Variable|DoneVariables],
                    Variable ++ " = " ++ Value ++ "\n" ++ Line;
                true ->
                    DoneVariables2 = DoneVariables,
                    Line
            end;
        % otherwise we just print out the original line
        _ ->
            DoneVariables2 = DoneVariables,
            Line
        end,
    % clumsy way to only append a newline character
    % if the line is not empty. We need this to not
    % avoid haveing a newline inserted at the top
    % of the target file each time we save it.
    Contents2 = case Contents of "" -> ""; _ -> Contents ++ "\n" end,

    % go to next line
    save_loop({{Module, Variable}, Value}, Rest, NewCurrentModule, Contents2 ++ NewContents, DoneVariables2);

save_loop(_Config, [], _OldModule, NewFileContents, _DoneVariable) ->
    % we're out of new lines, just return the new file's contents
    NewFileContents.

append_new_ini_section({{ModuleName, Variable}, Value}, OldFileContents) ->
    OldFileContents ++ "\n\n" ++ ModuleName ++ "\n" ++  Variable ++ " = " ++ Value ++ "\n".

%% @spec parse_module(Lins::string(), OldModule::string()) -> string()
%% @doc Tries to match a line against a pattern specifying a ini module or
%%      section ("[Module]"). Returns OldModule if no match is found.
parse_module(Line, OldModule) ->
    case regexp:match(Line, "^\\[([a-zA-Z0-9_-]*)\\]$") of
        nomatch ->
            OldModule;
        {error, Error} ->
            io:format("ini file regex error module: '~s'~n", [Error]),
            OldModule;
        {match, Start, Length} ->
            string:substr(Line, Start, Length)
    end.

%% @spec parse_variable(Line::string(), Variable::string(), Value::string()) ->
%%         string() | nomatch
%% @doc Tries to match a variable assignment in Line. Returns nomatch if the
%%      Variable is not found. Returns a new line composed of the Variable and
%%      Value otherwise.
parse_variable(Line, Variable, Value) ->
    case regexp:match(Line, "^" ++ Variable ++ "\s?=") of
        nomatch ->
            nomatch;
        {error, Error}->
            io:format("ini file regex error variable: '~s'~n", [Error]),
            nomatch;
        {match, _Start, _Length} ->
            Variable ++ " = " ++ Value
    end.

%% @spec save_file(File::filename(), Contents::string()) ->
%%           ok | {error, Reason::string()}
%% @doc Writes Contents to File
save_file(File, Contents) ->
    file:write_file(File, list_to_binary(Contents)).