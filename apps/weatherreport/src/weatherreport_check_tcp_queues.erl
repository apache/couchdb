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

%% @doc Diagnostic that checks the current tcp recv and send queues.
%% If the queues are high a warning message will be send, otherwise
%% only an informational message.
-module(weatherreport_check_tcp_queues).
-behaviour(weatherreport_check).

-export([
    description/0,
    valid/0,
    check/1,
    format/1
]).

-define(THRESHOLD, 1000000).

-spec description() -> string().
description() ->
    "Measure the length of tcp queues in the kernel".

-spec valid() -> boolean().
valid() ->
    weatherreport_node:can_connect().

%% @doc Converts the raw text output of netstat into the sum of the
%% tcp recv and send queues.
-spec sum_queues(string()) -> {integer(), integer()}.
sum_queues(Netstats) ->
    sum_queues(string:tokens(Netstats, "\n"), {0, 0}).

%% @doc Converts the rows of text output of netstat into the sum of
%% the tcp recv and send queues. Note that this function is tightly coupled
%% to the output of the netstat command provided by the system OS (tested
%% with netstat 1.42).
-spec sum_queues([string()], {integer(), integer()}) -> {integer(), integer()}.
sum_queues([], Acc) ->
    Acc;
sum_queues([Row | Rest], {SumRecvQ, SumSendQ}) ->
    {RecvQ, SendQ} =
        case string:tokens(Row, " ") of
            [[$t, $c, $p | _] | _] = Cols ->
                {Rq, Sq} = {lists:nth(2, Cols), lists:nth(3, Cols)},
                {list_to_integer(Rq), list_to_integer(Sq)};
            _ ->
                {0, 0}
        end,
    sum_queues(Rest, {RecvQ + SumRecvQ, SendQ + SumSendQ}).

%% @doc Converts the sum of queue lengths to a log message at the approriate
%% level, given ?THRESHOLD
-spec sum_to_message(integer(), string()) -> {atom(), term()}.
sum_to_message(Sum, Prefix) when Sum > ?THRESHOLD ->
    {warning, {list_to_atom(Prefix ++ "_high"), Sum}};
sum_to_message(Sum, Prefix) ->
    {info, {list_to_atom(Prefix ++ "_ok"), Sum}}.

-spec check(list()) -> [{atom(), term()}].
check(_Opts) ->
    Netstats = weatherreport_util:run_command("netstat"),
    {SumRecvQ, SumSendQ} = sum_queues(Netstats),
    [sum_to_message(SumRecvQ, "recv_q"), sum_to_message(SumSendQ, "send_q")].

-spec format(term()) -> {io:format(), [term()]}.
format({recv_q_high, QLen}) ->
    {"Total TCP Recv-Q is HIGH: ~w", [QLen]};
format({recv_q_ok, QLen}) ->
    {"Total TCP Recv-Q is ok: ~w", [QLen]};
format({send_q_high, QLen}) ->
    {"Total TCP Send-Q is HIGH: ~w", [QLen]};
format({send_q_ok, QLen}) ->
    {"Total TCP Send-Q is ok: ~w", [QLen]}.
