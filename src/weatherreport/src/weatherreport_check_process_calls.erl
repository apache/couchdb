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
         check/1,
         format/1]).

-define(THRESHOLD, 1000).

-spec description() -> string().
description() ->
    "Check for large numbers of processes with the same current/initial call".

-spec valid() -> boolean().
valid() ->
    weatherreport_node:can_connect().

-spec total_to_level(integer()) -> atom().
total_to_level(Total) when Total > ?THRESHOLD ->
    notice;
total_to_level(_Total) ->
    info.

fold_processes([], Acc, _Lim, _CallType, _Opts) ->
    Acc;
fold_processes(_, Acc, 0, _CallType, _Opts) ->
    Acc;
fold_processes([{Count, undefined} | T], Acc, Lim, CallType, Opts) ->
    Level = total_to_level(Count),
    Message = {Level, {process_count, {CallType, Count, undefined}}},
    fold_processes(T, [Message | Acc], Lim - 1, CallType, Opts);
fold_processes([{Count, {M, F, A}} | T], Acc, Lim, CallType, Opts) ->
    Level = total_to_level(Count),
    Message = case proplists:get_value(expert, Opts) of
        true ->
            PidFun = list_to_atom("find_by_" ++ CallType ++ "_call"),
            Pids = erlang:apply(recon, PidFun, [M, F]),
            Pinfos = lists:map(fun(Pid) ->
                Pinfo = recon:info(Pid),
                {Pid, Pinfo}
            end, lists:sublist(Pids, 10)),
            {Level, {process_count, {CallType, Count, M, F, A, Pinfos}}};
        _ ->
            {Level, {process_count, {CallType, Count, M, F, A}}}
    end,
    fold_processes(T, [Message | Acc], Lim - 1, CallType, Opts).

-spec check(list()) -> [{atom(), term()}].
check(Opts) ->
    CurrentCallCounts = recon:show_current_call_counts(),
    CurrentCallMessages = fold_processes(
        CurrentCallCounts,
        [],
        10,
        "current",
        Opts
    ),
    FirstCallCounts = recon:show_first_call_counts(),
    lists:reverse(fold_processes(
        FirstCallCounts,
        CurrentCallMessages,
        10,
        "first",
        Opts
    )).

-spec format(term()) -> {io:format(), [term()]}.
format({process_count, {CallType, Count, undefined}}) ->
    {"~w processes with ~s call ~w", [Count, CallType, undefined]};
format({process_count, {CallType, Count, M, F, A}}) ->
    {"~w processes with ~s call ~w:~w/~w", [Count, CallType, M, F, A]};
format({process_count, {CallType, Count, M, F, A, Pinfos}}) ->
    {"~w processes with ~s call ~w:~w/~w ~w", [Count, CallType, M, F, A, Pinfos]}.
