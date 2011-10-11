% Copyright 2010 Cloudant
%
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
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-export([start_link/0, get_active/0, get_queue/0, push/1, push/2,
    remove_node/1, initial_sync/1]).

-include("mem3.hrl").
-include_lib("couch/include/couch_db.hrl").

-record(state, {
    active = [],
    count = 0,
    limit,
    dict = dict:new(),
    waiting = [],
    update_notifier
}).

-record(job, {name, node, count=nil, pid=nil}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_active() ->
    gen_server:call(?MODULE, get_active).

get_queue() ->
    gen_server:call(?MODULE, get_queue).

push(#shard{name = Name}, Target) ->
    push(Name, Target);
push(Name, #shard{node=Node}) ->
    push(Name, Node);
push(Name, Node) ->
    push(#job{name = Name, node = Node}).

push(#job{node = Node} = Job) when Node =/= node() ->
    gen_server:cast(?MODULE, {push, Job});
push(_) ->
    ok.

remove_node(Node) ->
    gen_server:cast(?MODULE, {remove_node, Node}).

init([]) ->
    process_flag(trap_exit, true),
    Concurrency = couch_config:get("mem3", "sync_concurrency", "10"),
    gen_event:add_handler(mem3_events, mem3_sync_event, []),
    {ok, Pid} = start_update_notifier(),
    spawn(fun initial_sync/0),
    {ok, #state{limit = list_to_integer(Concurrency), update_notifier=Pid}}.

handle_call(get_active, _From, State) ->
    {reply, State#state.active, State};

handle_call(get_queue, _From, State) ->
    {reply, State#state.waiting, State};

handle_call(get_backlog, _From, #state{active=A, waiting=W} = State) ->
    CA = lists:sum([C || #job{count=C} <- A, is_integer(C)]),
    CW = lists:sum([C || #job{count=C} <- W, is_integer(C)]),
    {reply, CA+CW, State}.

handle_cast({push, DbName, Node}, State) ->
    handle_cast({push, #job{name = DbName, node = Node}}, State);

handle_cast({push, Job}, #state{count=Count, limit=Limit} = State)
        when Count >= Limit ->
    {noreply, add_to_queue(State, Job)};

handle_cast({push, Job}, State) ->
    #state{active = L, count = C} = State,
    #job{name = DbName, node = Node} = Job,
    case is_running(DbName, Node, L) of
    true ->
        {noreply, add_to_queue(State, Job)};
    false ->
        Pid = start_push_replication(Job),
        {noreply, State#state{active=[Job#job{pid=Pid}|L], count=C+1}}
    end;

handle_cast({remove_node, Node}, #state{waiting = W0} = State) ->
    {Alive, Dead} = lists:partition(fun(#job{node=N}) -> N =/= Node end, W0),
    Dict = remove_entries(State#state.dict, Dead),
    [exit(Pid, die_now) || #job{node=N, pid=Pid} <- State#state.active,
        N =:= Node],
    {noreply, State#state{dict = Dict, waiting = Alive}};

handle_cast({remove_shard, Shard}, #state{waiting = W0} = State) ->
    {Alive, Dead} = lists:partition(fun(#job{name=S}) -> S =/= Shard end, W0),
    Dict = remove_entries(State#state.dict, Dead),
    [exit(Pid, die_now) || #job{name=S, pid=Pid} <- State#state.active,
        S =:= Shard],
    {noreply, State#state{dict = Dict, waiting = Alive}}.

handle_info({'EXIT', Pid, _}, #state{update_notifier=Pid} = State) ->
    {ok, NewPid} = start_update_notifier(),
    {noreply, State#state{update_notifier=NewPid}};

handle_info({'EXIT', Active, normal}, State) ->
    handle_replication_exit(State, Active);

handle_info({'EXIT', Active, die_now}, State) ->
    % we forced this one ourselves, do not retry
    handle_replication_exit(State, Active);

handle_info({'EXIT', Active, {{not_found, no_db_file}, _Stack}}, State) ->
    % target doesn't exist, do not retry
    handle_replication_exit(State, Active);

handle_info({'EXIT', Active, Reason}, State) ->
    NewState = case lists:keyfind(Active, #job.pid, State#state.active) of
        #job{name=OldDbName, node=OldNode} = Job ->
        twig:log(warn, "~p ~s -> ~p ~p", [?MODULE, OldDbName, OldNode,
            Reason]),
        case Reason of {pending_changes, Count} ->
            add_to_queue(State, Job#job{pid = nil, count = Count});
        _ ->
            timer:apply_after(5000, ?MODULE, push, [Job#job{pid=nil}]),
            State
        end;
    false -> State end,
    handle_replication_exit(NewState, Active);

handle_info(Msg, State) ->
    twig:log(notice, "unexpected msg at replication manager ~p", [Msg]),
    {noreply, State}.

terminate(_Reason, State) ->
    [exit(Pid, shutdown) || #job{pid=Pid} <- State#state.active],
    ok.

code_change(_, #state{waiting = [{_,_}|_] = W, active=A} = State, _) ->
    Waiting = [#job{name=Name, node=Node} || {Name,Node} <- W],
    Active = [#job{name=Name, node=Node, pid=Pid} || {Name,Node,Pid} <- A],
    {ok, State#state{active = Active, waiting = Waiting}};

code_change(_, State, _) ->
    {ok, State}.

handle_replication_exit(#state{waiting=[]} = State, Pid) ->
    NewActive = lists:keydelete(Pid, #job.pid, State#state.active),
    {noreply, State#state{active=NewActive, count=length(NewActive)}};
handle_replication_exit(State, Pid) ->
    #state{active=Active, limit=Limit, dict=D, waiting=Waiting} = State,
    Active1 = lists:keydelete(Pid, #job.pid, Active),
    Count = length(Active1),
    NewState = if Count < Limit ->
        case next_replication(Active1, Waiting) of
        nil -> % all waiting replications are also active
            State#state{active = Active1, count = Count};
        {#job{name=DbName, node=Node} = Job, StillWaiting} ->
            NewPid = start_push_replication(Job),
            State#state{
                active = [Job#job{pid = NewPid} | Active1],
                count = Count+1,
                dict = dict:erase({DbName,Node}, D),
                waiting = StillWaiting
            }
        end;
    true ->
        State#state{active = Active1, count=Count}
    end,
    {noreply, NewState}.

start_push_replication(#job{name=Name, node=Node}) ->
    spawn_link(mem3_rep, go, [Name, Node]).

add_to_queue(State, #job{name=DbName, node=Node} = Job) ->
    #state{dict=D, waiting=Waiting} = State,
    case dict:is_key({DbName, Node}, D) of
    true ->
        State;
    false ->
        twig:log(debug, "adding ~s -> ~p to mem3_sync queue", [DbName, Node]),
        State#state{
            dict = dict:store({DbName,Node}, ok, D),
            waiting = Waiting ++ [Job]
        }
    end.

sync_nodes_and_dbs() ->
    Db1 = couch_config:get("mem3", "node_db", "nodes"),
    Db2 = couch_config:get("mem3", "shard_db", "dbs"),
    Db3 = couch_config:get("couch_httpd_auth", "authentication_db", "_users"),
    Dbs = [Db1, Db2, Db3],
    Nodes = mem3:nodes(),
    Live = nodes(),
    [[push(?l2b(Db), N) || Db <- Dbs] || N <- Nodes, lists:member(N, Live)].

initial_sync() ->
    [net_kernel:connect_node(Node) || Node <- mem3:nodes()],
    sync_nodes_and_dbs(),
    initial_sync(nodes()).

initial_sync(Live) ->
    Self = node(),
    {ok, AllDbs} = fabric:all_dbs(),
    lists:foreach(fun(Db) ->
        LocalShards = [S || #shard{node=N} = S <- mem3:shards(Db), N =:= Self],
        lists:foreach(fun(#shard{name=ShardName}) ->
            Targets = [S || #shard{node=N, name=Name} = S <- mem3:shards(Db),
                N =/= Self, Name =:= ShardName],
            [?MODULE:push(ShardName, N) || #shard{node=N} <- Targets,
                lists:member(N, Live)]
        end, LocalShards)
    end, AllDbs).

start_update_notifier() ->
    Db1 = ?l2b(couch_config:get("mem3", "node_db", "nodes")),
    Db2 = ?l2b(couch_config:get("mem3", "shard_db", "dbs")),
    Db3 = ?l2b(couch_config:get("couch_httpd_auth", "authentication_db",
        "_users")),
    couch_db_update_notifier:start_link(fun
    ({updated, Db}) when Db == Db1; Db == Db2; Db == Db3 ->
        Nodes = mem3:nodes(),
        Live = nodes(),
        [?MODULE:push(Db, N) || N <- Nodes, lists:member(N, Live)];
    ({updated, <<"shards/", _/binary>> = ShardName}) ->
        % TODO deal with split/merged partitions by comparing keyranges
        try mem3:shards(mem3:dbname(ShardName)) of
        Shards ->
            Targets = [S || #shard{node=N, name=Name} = S <- Shards,
                N =/= node(), Name =:= ShardName],
            Live = nodes(),
            [?MODULE:push(ShardName,N) || #shard{node=N} <- Targets,
                lists:member(N, Live)]
        catch error:database_does_not_exist ->
            ok
        end;
    ({deleted, <<"shards/", _:18/binary, _/binary>> = ShardName}) ->
        gen_server:cast(?MODULE, {remove_shard, ShardName});
    (_) -> ok end).

%% @doc Finds the next {DbName,Node} pair in the list of waiting replications
%% which does not correspond to an already running replication
-spec next_replication([#job{}], [#job{}]) -> {#job{}, [#job{}]} | nil.
next_replication(Active, Waiting) ->
    Fun = fun(#job{name=S, node=N}) -> is_running(S,N,Active) end,
    case lists:splitwith(Fun, Waiting) of
    {_, []} ->
        nil;
    {Running, [Job|Rest]} ->
        {Job, Running ++ Rest}
    end.

is_running(DbName, Node, ActiveList) ->
    [] =/= [true || #job{name=S, node=N} <- ActiveList, S=:=DbName, N=:=Node].

remove_entries(Dict, Entries) ->
    lists:foldl(fun(Entry, D) -> dict:erase(Entry, D) end, Dict, Entries).
