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

% Use pg process groups to reduce the chance of duplicate replication jobs
% running on the same cluster.
%
% A custom replicator pg group is started via start_link/0. Then, replication
% jobs check if they would be leaders before starting. If, by chance, two jobs
% with the same RepId start anyway, then replication jobs would do an extra
% check before each checkpoint. If the they are not leaders any longer, they
% should stop running. The "leader" is just the first sorted element in the
% [node(Pid), ...] list.

-module(couch_replicator_pg).

-export([
    start_link/0,
    join/2,
    leave/2,
    pids/1,
    should_start/2,
    should_run/2
]).

% Start a custom pg group. Should be called from the replication supervisor.
%
start_link() ->
    pg:start_link(?MODULE).

% Join a replication job pid to a RepId group
%
join({_, _} = RepId, Pid) when is_pid(Pid) ->
    pg:join(?MODULE, id(RepId), Pid).

% Leave a replication RepId group. This doesn't have to be called explicitly as
% the processes are monitored and automatically removed by pg. It may be nice,
% to call it from terminate/2 to speed things along a bit and clear the group
% quicker.
%
leave({_, _} = RepId, Pid) when is_pid(Pid) ->
    pg:leave(?MODULE, id(RepId), Pid).

% Determine if a replication job should start on a particular node. If it
% should, return `yes`, otherwise return `{no, OtherPid}`. `OtherPid` is
% the pid of the replication job that is already running.
%
should_start({_, _} = RepId, Node) when is_atom(Node) ->
    no_other_nodes(Node, pids(RepId)).

% Determine if the replication job should keep running as the main job for that
% RepId. If it is, return yes, otherwise return `{no, OtherPid}`. `OtherPid` is
% the pid of the replication job that should stay running instead of this one.
%
should_run({_, _} = RepId, Pid) when is_pid(Pid) ->
    case pids(RepId) of
        [OtherPid | _] when OtherPid =/= Pid -> {no, OtherPid};
        _ -> yes
    end.

% Sort all the pids by node first to get some deterministic order. For all we
% know, pids may already sort that way, but we're just making it explicit here
% in case it somehow changes in the future.
%
pids({_, _} = RepId) ->
    NodePids = [{node(P), P} || P <- pg:get_members(?MODULE, id(RepId))],
    {_, Pids} = lists:unzip(lists:usort(NodePids)),
    Pids.

id({Base, Ext}) ->
    iolist_to_binary([Base, Ext]).

no_other_nodes(_, []) ->
    yes;
no_other_nodes(Node, [Pid | _]) when Node =/= node(Pid) ->
    {no, Pid};
no_other_nodes(Node, [Pid | Pids]) when Node =:= node(Pid) ->
    no_other_nodes(Node, Pids).

-ifdef(TEST).

-include_lib("couch/include/couch_eunit.hrl").

couch_replicator_pg_test_() ->
    {
        foreach,
        fun setup/0,
        fun teardown/1,
        [
            ?TDEF_FE(t_start_stop),
            ?TDEF_FE(t_join_leave),
            ?TDEF_FE(t_should_start),
            ?TDEF_FE(t_should_run)
        ]
    }.

setup() ->
    {ok, PGPid} = start_link(),
    PGPid.

teardown(PGPid) when is_pid(PGPid) ->
    ?assertEqual(ok, gen_server:stop(PGPid)).

t_start_stop(PGPid) ->
    ?assert(is_process_alive(PGPid)),
    ?assertEqual([], pg:which_groups(?MODULE)).

t_join_leave(_) ->
    RepId = {"a", "+b"},
    ?assertEqual([], pids(RepId)),
    Pid = self(),
    ?assertEqual(ok, join(RepId, Pid)),
    ?assertEqual([id(RepId)], pg:which_groups(?MODULE)),
    ?assertEqual([Pid], pids(RepId)),
    ?assertEqual(ok, leave(RepId, Pid)),
    ?assertEqual(not_joined, leave(RepId, Pid)),
    ?assertEqual([], pids(RepId)),
    ?assertEqual([], pg:which_groups(?MODULE)).

t_should_start(_) ->
    RepId = {"a", "+b"},
    ?assertEqual(yes, should_start(RepId, node())),
    ?assertEqual(yes, should_start(RepId, 'foo@bar.bogus.net')),
    Pid = self(),
    ok = join(RepId, Pid),
    % On the same node we let it start, it will blow up anyway in the
    % supervisor.
    ?assertEqual(yes, should_start(RepId, node())),
    ?assertEqual({no, Pid}, should_start(RepId, 'foo@bar.bogus42.net')).

t_should_run(_) ->
    RepId = {"a", "+b"},
    Pid = self(),
    % This is odd case, somehow a job asks if it should run but it hasn't
    % registered. We just choose to let it run.
    ?assertEqual(yes, should_run(RepId, Pid)),
    ok = join(RepId, Pid),
    % The only job registered is itself
    ?assertEqual(yes, should_run(RepId, Pid)),
    % Let's add <0.0.0> init so it can sort lower
    InitPid = whereis(init),
    ok = join(RepId, InitPid),
    ?assertEqual({no, InitPid}, should_run(RepId, Pid)).

-endif.
