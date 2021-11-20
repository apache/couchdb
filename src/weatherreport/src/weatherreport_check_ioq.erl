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

%% @doc Diagnostic that checks the total number of IOQ requests. If
%% the total exceeds a configured threshold a warning message will be
%% sent, otherwise only an information message.
-module(weatherreport_check_ioq).
-behaviour(weatherreport_check).

-export([
    description/0,
    valid/0,
    check/1,
    format/1
]).

-define(THRESHOLD, 500).

-spec description() -> string().
description() ->
    "Check the total number of active IOQ requests".

-spec valid() -> boolean().
valid() ->
    weatherreport_node:can_connect().

-spec total_to_level(integer()) -> atom().
total_to_level(Total) when Total > ?THRESHOLD ->
    warning;
total_to_level(_Total) ->
    info.

-spec sum_channels(list(), list()) -> list().
sum_channels([], Acc) ->
    Acc;
sum_channels([{_Name, Value} | Rest], Acc) ->
    sum_channels(Rest, Acc + lists:sum(Value)).

-spec sum_queues(list(), list()) -> list().
sum_queues([], Acc) ->
    Acc;
sum_queues([{channels, {Channels}} | Rest], Acc) ->
    sum_queues(Rest, sum_channels(Channels, Acc));
sum_queues([{_Name, Value} | Rest], Acc) ->
    sum_queues(Rest, Acc + Value).

-spec check(list()) -> [{atom(), term()}].
check(Opts) ->
    case erlang:function_exported(ioq, get_queue_lengths, 0) of
        true ->
            case ioq:get_queue_lengths() of
                Queues when is_map(Queues) ->
                    Total = maps:fold(
                        fun(_Key, Val, Acc) ->
                            Val + Acc
                        end,
                        0,
                        Queues
                    ),
                    [{total_to_level(Total), {ioq_requests, Total, Queues}}];
                Error ->
                    [{warning, {ioq_requests_unknown, Error}}]
            end;
        false ->
            check_legacy(Opts)
    end.

-spec check_legacy(list()) -> [{atom(), term()}].
check_legacy(_Opts) ->
    case ioq:get_disk_queues() of
        Queues when is_list(Queues) ->
            Total = sum_queues(Queues, 0),
            [{total_to_level(Total), {ioq_requests, Total, Queues}}];
        Error ->
            [{warning, {ioq_requests_unknown, Error}}]
    end.

-spec format(term()) -> {io:format(), [term()]}.
format({ioq_requests_unknown, Error}) ->
    {"Could not determine total number of IOQ requests: ~w~n", [Error]};
format({ioq_requests, Total, Queues}) ->
    {"Total number of active IOQ requests is: ~w ~w", [Total, Queues]}.
