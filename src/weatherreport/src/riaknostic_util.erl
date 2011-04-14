%% -------------------------------------------------------------------
%%
%% riaknostic - automated diagnostic tools for Riak
%%
%% Copyright (c) 2011 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

%% @doc Utility functions for riaknostic.
%% @end
-module(riaknostic_util).
-export([short_name/1,
         run_command/1,
         log/2,log/3,
         binary_to_float/1]).

%% @doc Converts a check module name into a short name that can be
%% used to refer to a check on the command line.  For example,
%% <code>riaknostic_check_disk becomes</code> <code>"disk"</code>.
-spec short_name(module()) -> iodata() | unicode:charlist().
short_name(Mod) when is_atom(Mod) ->
    re:replace(atom_to_list(Mod), "riaknostic_check_", "", [{return, list}]).

%% @doc Runs a shell command and returns the output. stderr is
%% redirected to stdout so its output will be included.
-spec run_command(Command::iodata()) -> StdOut::iodata().
run_command(Command) ->
    riaknostic_util:log(debug, "Running shell command: ~s", [Command]),
    Port = erlang:open_port({spawn,Command},[exit_status, stderr_to_stdout]),
    do_read(Port, []).

do_read(Port, Acc) ->
    receive
        {Port, {data, StdOut}} ->
            riaknostic_util:log(debug, "Shell command output: ~n~s~n",[StdOut]),
            do_read(Port, Acc ++ StdOut);
        {Port, {exit_status, _}} -> 
            %%port_close(Port),
            Acc;
        Other -> 
            io:format("~w", [Other]),
            do_read(Port, Acc)
    end.
 
%% @doc Converts a binary containing a text representation of a float
%% into a float type.
-spec binary_to_float(binary()) -> float().
binary_to_float(Bin) ->
    list_to_float(binary_to_list(Bin)).

log(Level, Format, Terms) ->
    case should_log(Level) of
        true ->
            io:format(lists:concat(["[", Level, "] ", Format, "~n"]), Terms);
        false ->
            ok
    end,
    lager:log(Level, self(), Format, Terms).

log(Level, String) ->
    case should_log(Level) of
        true ->
            io:format(lists:concat(["[", Level, "] ", String, "~n"]));
        false ->
            ok
    end,
    lager:log(Level, self(), String).

should_log(Level) ->
    AppLevel = case application:get_env(riaknostic, log_level) of
        undefined -> info;
        {ok, L0} -> L0
    end,
    lager_util:level_to_num(AppLevel) >= lager_util:level_to_num(Level).

