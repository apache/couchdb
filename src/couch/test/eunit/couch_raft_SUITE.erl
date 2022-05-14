% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
% http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-

-module(couch_raft_SUITE).

-behaviour(ct_suite).

-export([all/0]).
-export([three_nodes/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [three_nodes].

three_nodes(Config) when is_list(Config) ->
    N = 3,
    Args = ["-pa", filename:dirname(code:which(craft))],
    Peers = [?CT_PEER(#{wait_boot => {self(), tag}, args => Args}) || _ <- lists:seq(1, N)],
    Cohort = [receive {tag, {started, Node, Peer}} -> Node end || {ok, Peer} <- Peers],

    Crafts = [erpc:call(Node, craft3, start, [foo, Cohort]) || Node <- Cohort],

    % wait for leader election
    timer:sleep(500),

    % verify only one leader elected
    [{leader, FirstLeader}] = lists:filter(fun({State, _Pid}) -> State == leader end,
    [{element(1, sys:get_state(Pid)), Pid} || {ok, Pid} <- Crafts]),

    % make a series of calls
    Hash1 = crypto:hash(sha256, <<0, 1>>),
    ?assertEqual(Hash1, craft3:call(FirstLeader, <<1>>)),

    Hash2 = crypto:hash(sha256, <<Hash1/binary, 2>>),
    ?assertEqual(Hash2, craft3:call(FirstLeader, <<2>>)),

    Hash3 = crypto:hash(sha256, <<Hash2/binary, 3>>),
    ?assertEqual(Hash3, craft3:call(FirstLeader, <<3>>)),

    % force a re-election
    craft3:stop(FirstLeader),
    timer:sleep(500),

    % verify new leader elected
    [{leader, SecondLeader}] = lists:filter(fun({State, _Pid}) -> State == leader end,
        [{element(1, sys:get_state(Pid)), Pid} || {ok, Pid} <- Crafts, Pid /= FirstLeader]),
    ?assertNotEqual(FirstLeader, SecondLeader),

    % make another call
    Hash4 = crypto:hash(sha256, <<Hash3/binary, 4>>),
    ?assertEqual(Hash4, craft3:call(SecondLeader, <<4>>)),

    [craft3:stop(Pid) || {ok, Pid} <- Crafts, Pid /= FirstLeader],
    [peer:stop(Peer) || {ok, Peer} <- Peers].
