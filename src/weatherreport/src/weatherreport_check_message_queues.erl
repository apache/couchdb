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

%% @doc Diagnostic that checks for processes with large mailboxes
%% and sends a warning message if one or more processes exceed the
%% threshold.
-module(weatherreport_check_message_queues).
-behaviour(weatherreport_check).

-export([description/0,
         valid/0,
         check/0,
         format/1]).

-define(THRESHOLD, 1000).

-spec description() -> string().
description() ->
    "Check for processes with large mailboxes".

-spec valid() -> boolean().
valid() ->
    weatherreport_node:can_connect().

fold_processes([], Acc) ->
    Acc;
fold_processes([{Pid, MBoxSize, Info} | T], Acc) when MBoxSize < ?THRESHOLD ->
    Message = {info, {mbox_ok, {Pid, MBoxSize, Info}}},
    fold_processes(T, [Message | Acc]);
fold_processes([{Pid, MBoxSize, Info} | T], Acc) ->
    case application:get_env(weatherreport, expert_mode) of
        {ok, true} ->
            Pinfo = weatherreport_node:local_command(recon, info, [Pid]),
            weatherreport_util:log(warning, "Process info for ~w:~n~p", [Pid, Pinfo]);
        _ ->
            ok
    end,
    Message = {warning, {mbox_large, {Pid, MBoxSize, Info}}},
    fold_processes(T, [Message | Acc]).

-spec check() -> [{atom(), term()}].
check() ->
    Processes = weatherreport_node:local_command(
        recon,
        proc_count,
        [message_queue_len, 10]
    ),
    fold_processes(Processes, []).

-spec format(term()) -> {io:format(), [term()]}.
format({mbox_large, {Pid, MBoxSize, Info}}) ->
    {"Process ~w has excessive mailbox size of ~w: ~w", [Pid, MBoxSize, Info]};
format({mbox_ok, {Pid, MBoxSize, Info}}) ->
    {"Process ~w has mailbox size of ~w: ~w", [Pid, MBoxSize, Info]}.

