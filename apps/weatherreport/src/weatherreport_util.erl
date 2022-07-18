%% -------------------------------------------------------------------
%%
%% derived from riaknostic - automated diagnostic tools for Riak
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
%%
%% File renamed from riaknostic_util.erl to weatherreport_util.erl
%% Copyright (c) 2014 Cloudant
%%
%% -------------------------------------------------------------------

%% @doc Utility functions for weatherreport.
%% @end
-module(weatherreport_util).
-export([
    short_name/1,
    run_command/1,
    binary_to_float/1,
    flush_stdout/0,
    check_proc_count/3
]).

%% @doc Converts a check module name into a short name that can be
%% used to refer to a check on the command line.  For example,
%% <code>weatherreport_check_memory_use becomes</code>
%% <code>"memory_use"</code>.
-spec short_name(module()) -> iodata() | unicode:charlist().
short_name(Mod) when is_atom(Mod) ->
    re:replace(atom_to_list(Mod), "weatherreport_check_", "", [{return, list}]).

%% @doc Runs a shell command and returns the output. stderr is
%% redirected to stdout so its output will be included.
-spec run_command(Command :: iodata()) -> StdOut :: iodata().
run_command(Command) ->
    weatherreport_log:log(
        node(),
        debug,
        "Running shell command: ~s",
        [Command]
    ),
    Port = erlang:open_port({spawn, Command}, [exit_status, stderr_to_stdout]),
    do_read(Port, []).

do_read(Port, Acc) ->
    receive
        {Port, {data, StdOut}} ->
            weatherreport_log:log(
                node(),
                debug,
                "Shell command output: ~n~s~n",
                [StdOut]
            ),
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

flush_stdout() ->
    timer:sleep(1000).

%% @doc Utility function to check processes based on an attribute returned
%% by recon:proc_count/2.
-spec check_proc_count(atom(), integer(), list()) -> [{atom(), term()}].
check_proc_count(Key, Threshold, Opts) ->
    Processes = recon:proc_count(Key, 10),
    procs_to_messages(Processes, Threshold, [], Opts).

%% @doc Utility function to convert the list of process info returned by
%% recon:proc_count/2 into a list of diagnostic messages.
-spec procs_to_messages(list(), integer(), list(), list()) -> [{atom(), term()}].
procs_to_messages([], _Threshold, Acc, _Opts) ->
    Acc;
procs_to_messages([{Pid, Value, Info} | T], Threshold, Acc, Opts) ->
    Level =
        case Value > Threshold of
            true -> warning;
            _ -> info
        end,
    Message =
        case {Level, proplists:get_value(expert, Opts)} of
            {warning, true} ->
                Pinfo = recon:info(Pid),
                {warning, {high, {Pid, Value, Info, Pinfo}}};
            {warning, _} ->
                {warning, {high, {Pid, Value, Info}}};
            {info, _} ->
                {info, {ok, {Pid, Value, Info}}}
        end,
    procs_to_messages(T, Threshold, [Message | Acc], Opts).
