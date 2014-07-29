%% -------------------------------------------------------------------
%%
%% weatherreport - automated diagnostic tools for CouchDB
%%
%% Copyright (c) 2014 Cloudant
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

%% @doc Diagnostic that checks for large numbers of processes sharing
%% the same current or initial function call
-module(weatherreport_check_process_calls).
-behaviour(weatherreport_check).

-export([description/0,
         valid/0,
         check/0,
         format/1]).

-define(THRESHOLD, 1000).

-spec description() -> string().
description() ->
    "Check for large numbers of processes with the same current/initial call".

-spec valid() -> boolean().
valid() ->
    weatherreport_node:can_connect().

fold_processes([], Acc, _Lim, _) ->
    Acc;
fold_processes(_, Acc, 0, _) ->
    Acc;
fold_processes([{Count, {M, F, A}} | T], Acc, Lim, CallType) ->
    Level = case Count > ?THRESHOLD of
        true ->
            warning;
        _ ->
            info
    end,
    case application:get_env(weatherreport, expert_mode) of
        {ok, true} ->
            PidFun = list_to_atom("find_by_" ++ CallType ++ "_call"),
            Pids = weatherreport_node:local_command(recon, PidFun, [M, F]),
            lists:map(fun(Pid) ->
                Pinfo = weatherreport_node:local_command(recon, info, [Pid]),
                weatherreport_util:log(
                    Level,
                    "Process info for ~w:~n~p",
                    [Pid, Pinfo]
                )
            end, lists:sublist(Pids, 10));
        _ ->
            ok
    end,
    Message = {Level, {process_count, {CallType, Count, M, F, A}}},
    fold_processes(T, [Message | Acc], Lim - 1, CallType).

-spec check() -> [{atom(), term()}].
check() ->
    CurrentCallCounts = weatherreport_node:local_command(
        recon,
        show_current_call_counts,
        []
    ),
    CurrentCallMessages = fold_processes(
        CurrentCallCounts,
        [],
        10,
        "current"
    ),
    FirstCallCounts = weatherreport_node:local_command(
        recon,
        show_first_call_counts,
        []
    ),
    lists:reverse(fold_processes(
        FirstCallCounts,
        CurrentCallMessages,
        10,
        "first"
    )).

-spec format(term()) -> {io:format(), [term()]}.
format({process_count, {CallType, Count, M, F, A}}) ->
    {"~w processes with ~s call ~w:~w/~w", [Count, CallType, M, F, A]}.
