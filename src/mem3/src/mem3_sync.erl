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

-module(mem3_sync).
-behaviour(gen_server).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

-export([
    start_link/0,
    get_active/0,
    get_queue/0,
    push/1, push/2,
    remove_node/1,
    remove_shard/1,
    initial_sync/1,
    get_backlog/0,
    nodes_db/0,
    shards_db/0,
    users_db/0,
    find_next_node/0
]).
-export([
    local_dbs/0
]).

-import(queue, [in/2, out/1, to_list/1, join/2, from_list/1, is_empty/1]).

-include_lib("mem3/include/mem3.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(DEFAULT_CONCURRENCY, 10).
-define(DEFAULT_BATCH_SIZE, 500).
-define(DEFAULT_BATCH_COUNT, 5).

-record(state, {
    active = [],
    count = 0,
    limit = ?DEFAULT_CONCURRENCY,
    dict = dict:new(),
    waiting = queue:new()
}).

-record(job, {name, node, count = nil, pid = nil}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_active() ->
    gen_server:call(?MODULE, get_active).

get_queue() ->
    gen_server:call(?MODULE, get_queue).

get_backlog() ->
    gen_server:call(?MODULE, get_backlog).

push(#shard{name = Name}, Target) ->
    push(Name, Target);
push(Name, #shard{node = Node}) ->
    push(Name, Node);
push(Name, Node) ->
    push(#job{name = Name, node = Node}).

push(#job{node = Node} = Job) when Node =/= node() ->
    gen_server:cast(?MODULE, {push, Job});
push(_) ->
    ok.

remove_node(Node) ->
    gen_server:cast(?MODULE, {remove_node, Node}).

remove_shard(Shard) ->
    gen_server:cast(?MODULE, {remove_shard, Shard}).

init([]) ->
    process_flag(trap_exit, true),
    Concurrency = config:get_integer("mem3", "sync_concurrency", ?DEFAULT_CONCURRENCY),
    gen_event:add_handler(mem3_events, mem3_sync_event, []),
    initial_sync(),
    {ok, #state{limit = Concurrency}}.

handle_call({push, Job}, From, State) ->
    handle_cast({push, Job#job{pid = From}}, State);
handle_call(get_active, _From, State) ->
    {reply, State#state.active, State};
handle_call(get_queue, _From, State) ->
    {reply, to_list(State#state.waiting), State};
handle_call(get_backlog, _From, #state{active = A, waiting = WQ} = State) ->
    CA = lists:sum([C || #job{count = C} <- A, is_integer(C)]),
    CW = lists:sum([C || #job{count = C} <- to_list(WQ), is_integer(C)]),
    {reply, CA + CW, State}.

handle_cast({push, DbName, Node}, State) ->
    handle_cast({push, #job{name = DbName, node = Node}}, State);
handle_cast({push, Job}, #state{count = Count, limit = Limit} = State) when
    Count >= Limit
->
    {noreply, add_to_queue(State, Job)};
handle_cast({push, Job}, State) ->
    #state{active = L, count = C} = State,
    #job{name = DbName, node = Node} = Job,
    case is_running(DbName, Node, L) of
        true ->
            {noreply, add_to_queue(State, Job)};
        false ->
            Pid = start_push_replication(Job),
            {noreply, State#state{active = [Job#job{pid = Pid} | L], count = C + 1}}
    end;
handle_cast({remove_node, Node}, #state{waiting = W0} = State) ->
    {Alive, Dead} = lists:partition(fun(#job{node = N}) -> N =/= Node end, to_list(W0)),
    Dict = remove_entries(State#state.dict, Dead),
    [
        exit(Pid, die_now)
     || #job{node = N, pid = Pid} <- State#state.active,
        N =:= Node
    ],
    {noreply, State#state{dict = Dict, waiting = from_list(Alive)}};
handle_cast({remove_shard, Shard}, #state{waiting = W0} = State) ->
    {Alive, Dead} = lists:partition(
        fun(#job{name = S}) ->
            S =/= Shard
        end,
        to_list(W0)
    ),
    Dict = remove_entries(State#state.dict, Dead),
    [
        exit(Pid, die_now)
     || #job{name = S, pid = Pid} <- State#state.active,
        S =:= Shard
    ],
    {noreply, State#state{dict = Dict, waiting = from_list(Alive)}}.

handle_info({'EXIT', Active, normal}, State) ->
    handle_replication_exit(State, Active);
handle_info({'EXIT', Active, die_now}, State) ->
    % we forced this one ourselves, do not retry
    handle_replication_exit(State, Active);
handle_info({'EXIT', Active, {{not_found, no_db_file}, _Stack}}, State) ->
    % target doesn't exist, do not retry
    handle_replication_exit(State, Active);
handle_info({'EXIT', Active, Reason}, State) ->
    NewState =
        case lists:keyfind(Active, #job.pid, State#state.active) of
            #job{name = OldDbName, node = OldNode} = Job ->
                couch_log:warning("~s ~s ~s ~w", [?MODULE, OldDbName, OldNode, Reason]),
                case Reason of
                    {pending_changes, Count} ->
                        maybe_resubmit(State, Job#job{pid = nil, count = Count});
                    _ ->
                        case is_job_current(Job, nodes(), mem3:nodes()) of
                            true ->
                                timer:apply_after(5000, ?MODULE, push, [Job#job{pid = nil}]);
                            false ->
                                % no need to retry (db deleted or recreated)
                                ok
                        end,
                        State
                end;
            false ->
                State
        end,
    handle_replication_exit(NewState, Active);
handle_info(Msg, State) ->
    couch_log:notice("unexpected msg at replication manager ~p", [Msg]),
    {noreply, State}.

terminate(_Reason, State) ->
    [exit(Pid, shutdown) || #job{pid = Pid} <- State#state.active],
    ok.

maybe_resubmit(State, #job{name = DbName, node = Node} = Job) ->
    case lists:member(DbName, local_dbs()) of
        true ->
            case find_next_node() of
                Node when Node =/= node() ->
                    add_to_queue(State, Job);
                _ ->
                    % don't resubmit b/c we have a new replication target and
                    % don't push to self, as it's a no-op (push/2 has a guard
                    % for it but the topology might have changed since)
                    State
            end;
        false ->
            add_to_queue(State, Job)
    end.

handle_replication_exit(State, Pid) ->
    #state{active = Active, limit = Limit, dict = D, waiting = Waiting} = State,
    Active1 = lists:keydelete(Pid, #job.pid, Active),
    case is_empty(Waiting) of
        true ->
            {noreply, State#state{active = Active1, count = length(Active1)}};
        _ ->
            Count = length(Active1),
            NewState =
                if
                    Count < Limit ->
                        case next_replication(Active1, Waiting, queue:new()) of
                            % all waiting replications are also active
                            nil ->
                                State#state{active = Active1, count = Count};
                            {#job{name = DbName, node = Node} = Job, StillWaiting} ->
                                NewPid = start_push_replication(Job),
                                State#state{
                                    active = [Job#job{pid = NewPid} | Active1],
                                    count = Count + 1,
                                    dict = dict:erase({DbName, Node}, D),
                                    waiting = StillWaiting
                                }
                        end;
                    true ->
                        State#state{active = Active1, count = Count}
                end,
            {noreply, NewState}
    end.

start_push_replication(#job{name = Name, node = Node, pid = From}) ->
    if
        From =/= nil -> gen_server:reply(From, ok);
        true -> ok
    end,
    spawn_link(fun() ->
        BatchSize = config:get_integer("mem3", "sync_batch_size", ?DEFAULT_BATCH_SIZE),
        BatchCount = config:get_integer("mem3", "sync_batch_count", ?DEFAULT_BATCH_COUNT),
        Opts = [{batch_size, BatchSize}, {batch_count, BatchCount}],
        case mem3_rep:go(Name, maybe_redirect(Node), Opts) of
            {ok, Pending} when Pending > 0 ->
                exit({pending_changes, Pending});
            _ ->
                ok
        end
    end).

add_to_queue(State, #job{name = DbName, node = Node, pid = From} = Job) ->
    #state{dict = D, waiting = WQ} = State,
    case dict:is_key({DbName, Node}, D) of
        true ->
            if
                From =/= nil -> gen_server:reply(From, ok);
                true -> ok
            end,
            State;
        false ->
            couch_log:debug("adding ~s -> ~p to mem3_sync queue", [DbName, Node]),
            State#state{
                dict = dict:store({DbName, Node}, ok, D),
                waiting = in(Job, WQ)
            }
    end.

sync_nodes_and_dbs() ->
    Node = find_next_node(),
    [push(Db, Node) || Db <- local_dbs()].

initial_sync() ->
    mem3_sync_nodes:add(nodes()).

initial_sync(Live) ->
    sync_nodes_and_dbs(),
    Acc = {config:node_name(), Live, []},
    {_, _, Shards} = mem3_shards:fold(fun initial_sync_fold/2, Acc),
    submit_replication_tasks(config:node_name(), Live, Shards).

initial_sync_fold(#shard{dbname = Db} = Shard, {LocalNode, Live, AccShards}) ->
    case AccShards of
        [#shard{dbname = AccDb} | _] when Db =/= AccDb ->
            submit_replication_tasks(LocalNode, Live, AccShards),
            {LocalNode, Live, [Shard]};
        _ ->
            {LocalNode, Live, [Shard | AccShards]}
    end.

submit_replication_tasks(LocalNode, Live, Shards) ->
    SplitFun = fun(#shard{node = Node}) -> Node =:= LocalNode end,
    {Local, Remote} = lists:partition(SplitFun, Shards),
    lists:foreach(
        fun(#shard{name = ShardName}) ->
            [
                sync_push(ShardName, N)
             || #shard{node = N, name = Name} <- Remote,
                Name =:= ShardName,
                lists:member(N, Live)
            ]
        end,
        Local
    ).

sync_push(ShardName, N) ->
    gen_server:call(mem3_sync, {push, #job{name = ShardName, node = N}}, infinity).

find_next_node() ->
    Self = node(),
    LiveNodes = [Self | nodes()],
    Mem3Nodes = mem3:nodes(),
    find_next_node(Self, LiveNodes, Mem3Nodes).

find_next_node(Self, LiveNodes, Mem3Nodes) ->
    SortedMem3Nodes = lists:sort(Mem3Nodes),
    LiveMem3Nodes = [N || N <- SortedMem3Nodes, lists:member(N, LiveNodes)],
    case LiveMem3Nodes of
        [] ->
            % No live mem3 nodes. Strange. Return self as we can always
            % replicate to ourselves, which is just a no-op.
            Self;
        [H | _] ->
            % Append the first item to the end so dropwhile/2 can loop around
            % and find it, if it's supposed to be the result. For instance in
            % case of [x, n] and the current node as n, next node would be x,
            % so we make [x, n, x], so dropwhile/2 drops x and n, and stops at
            % [n, x = Next | _] then returns x.
            LiveMem3Nodes1 = LiveMem3Nodes ++ [H],
            case lists:dropwhile(fun(N) -> N =/= Self end, LiveMem3Nodes1) of
                [] ->
                    % We're not in the mem3 set of nodes. Also, strange. Return
                    % self anyway as we can replicate to ourselves.
                    Self;
                [_Self, Next | _] ->
                    Next
            end
    end.

%% @doc Finds the next {DbName,Node} pair in the list of waiting replications
%% which does not correspond to an already running replication
-spec next_replication([#job{}], queue:queue(_), queue:queue(_)) ->
    {#job{}, queue:queue(_)} | nil.
next_replication(Active, Waiting, WaitingAndRunning) ->
    case is_empty(Waiting) of
        true ->
            nil;
        false ->
            {{value, #job{name = S, node = N} = Job}, RemQ} = out(Waiting),
            case is_running(S, N, Active) of
                true ->
                    next_replication(Active, RemQ, in(Job, WaitingAndRunning));
                false ->
                    {Job, join(RemQ, WaitingAndRunning)}
            end
    end.

is_running(DbName, Node, ActiveList) ->
    [] =/= [true || #job{name = S, node = N} <- ActiveList, S =:= DbName, N =:= Node].

remove_entries(Dict, Entries) ->
    lists:foldl(
        fun(#job{name = S, node = N}, D) ->
            dict:erase({S, N}, D)
        end,
        Dict,
        Entries
    ).

local_dbs() ->
    UsersDb = users_db(),
    % users db might not have been created so don't include it unless it exists
    case couch_server:exists(UsersDb) of
        true -> [nodes_db(), shards_db(), UsersDb];
        false -> [nodes_db(), shards_db()]
    end.

nodes_db() ->
    ?l2b(config:get("mem3", "nodes_db", "_nodes")).

shards_db() ->
    ?l2b(config:get("mem3", "shards_db", "_dbs")).

users_db() ->
    ?l2b(config:get("couch_httpd_auth", "authentication_db", "_users")).

maybe_redirect(Node) ->
    case config:get("mem3.redirects", atom_to_list(Node)) of
        undefined ->
            Node;
        Redirect ->
            couch_log:debug("Redirecting push from ~p to ~p", [Node, Redirect]),
            list_to_existing_atom(Redirect)
    end.

% Check that the db exists and node is either connected or part of the cluster.
%
is_job_current(#job{name = Name, node = Node}, ConnectedNodes, Mem3Nodes) ->
    DbCurrent = mem3:db_is_current(Name),
    Connected = lists:member(Node, ConnectedNodes),
    InMem3Nodes = lists:member(Node, Mem3Nodes),
    DbCurrent andalso (Connected orelse InMem3Nodes).

-ifdef(TEST).

-include_lib("couch/include/couch_eunit.hrl").

find_next_node_test() ->
    ?assertEqual(n, find_next_node(n, [n], [])),
    ?assertEqual(n, find_next_node(n, [n], [n])),
    ?assertEqual(x, find_next_node(n, [a, n, x], [a, n, x])),
    ?assertEqual(a, find_next_node(n, [a, n], [a, n])),
    ?assertEqual(n, find_next_node(n, [a, n], [a])),
    ?assertEqual(x, find_next_node(n, [n, x], [x, n])),
    ?assertEqual(x, find_next_node(n, [n, x], [n, x])),
    ?assertEqual(a, find_next_node(n, [a, n, x], [a, n, y])).

is_job_current_test_() ->
    {
        foreach,
        fun setup/0,
        fun teardown/1,
        [
            ?TDEF_FE(t_is_job_current)
        ]
    }.

setup() ->
    Ctx = test_util:start_couch(),
    DbName = ?tempdb(),
    {ok, Db} = couch_db:create(DbName, [?ADMIN_CTX]),
    couch_db:close(Db),
    {Ctx, DbName}.

teardown({Ctx, DbName}) ->
    ok = couch_server:delete(DbName, [?ADMIN_CTX]),
    test_util:stop_couch(Ctx).

t_is_job_current({_, DbName}) ->
    Job = #job{name = DbName, node = n1},
    ?assert(is_job_current(Job, [], [n1])),
    ?assert(is_job_current(Job, [n1], [])),
    ?assert(is_job_current(Job, [n1], [n1])),
    ?assertNot(is_job_current(Job, [n2], [])),
    ?assertNot(is_job_current(Job, [], [n2])),
    ?assertNot(is_job_current(Job, [], [])),
    ?assertNot(is_job_current(Job, [n2], [n2])),
    ?assertNot(is_job_current(Job#job{name = <<"x">>}, [n1], [n1])),
    ?assertNot(is_job_current(Job#job{name = <<"x">>}, [], [])).

-endif.
