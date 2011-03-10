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

-export([start_link/0, get_active/0, get_queue/0, push/2, remove_node/1,
         initial_sync/1]).

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
    gen_server:cast(?MODULE, {push, Name, Node}).

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
    {reply, State#state.waiting, State}.

handle_cast({push, DbName, Node}, #state{count=Count, limit=Limit} = State)
        when Count >= Limit ->
    {noreply, add_to_queue(State, DbName, Node)};

handle_cast({push, DbName, Node}, State) ->
    #state{active = L, count = C} = State,
    case is_running(DbName, Node, L) of
    true ->
        {noreply, add_to_queue(State, DbName, Node)};
    false ->
        Pid = start_push_replication(DbName, Node),
        {noreply, State#state{active=[{DbName, Node, Pid}|L], count=C+1}}
    end;

handle_cast({remove_node, Node}, #state{waiting = W0} = State) ->
    {Alive, Dead} = lists:partition(fun({_,N}) -> N =/= Node end, W0),
    Dict = remove_entries(State#state.dict, Dead),
    [exit(Pid, die_now) || {_,N,Pid} <- State#state.active, N =:= Node],
    {noreply, State#state{dict = Dict, waiting = Alive}};

handle_cast({remove_shard, Shard}, #state{waiting = W0} = State) ->
    {Alive, Dead} = lists:partition(fun({S,_}) -> S =/= Shard end, W0),
    Dict = remove_entries(State#state.dict, Dead),
    [exit(Pid, die_now) || {S,_,Pid} <- State#state.active, S =:= Shard],
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
    case lists:keyfind(Active, 3, State#state.active) of
    {OldDbName, OldNode, _} ->
        twig:log(warn, "~p ~s -> ~p died: ~p", [?MODULE, OldDbName, OldNode,
            Reason]),
        timer:apply_after(5000, ?MODULE, push, [OldDbName, OldNode]);
    false -> ok end,
    handle_replication_exit(State, Active);

handle_info(Msg, State) ->
    twig:log(notice, "unexpected msg at replication manager ~p", [Msg]),
    {noreply, State}.

terminate(_Reason, State) ->
    [exit(Pid, shutdown) || {_,_,Pid} <- State#state.active],
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_replication_exit(#state{waiting=[]} = State, Pid) ->
    NewActive = lists:keydelete(Pid, 3, State#state.active),
    {noreply, State#state{active=NewActive, count=length(NewActive)}};
handle_replication_exit(State, Pid) ->
    #state{active=Active, limit=Limit, dict=D, waiting=Waiting} = State,
    Active1 = lists:keydelete(Pid, 3, Active),
    Count = length(Active1),
    NewState = if Count < Limit ->
        case next_replication(Active1, Waiting) of
        nil -> % all waiting replications are also active
            State#state{active = Active1, count = Count};
        {DbName, Node, StillWaiting} ->
            NewPid = start_push_replication(DbName, Node),
            State#state{
                active = [{DbName, Node, NewPid} | Active1],
                count = Count+1,
                dict = dict:erase({DbName,Node}, D),
                waiting = StillWaiting
            }
        end;
    true ->
        State#state{active = Active1, count=Count}
    end,
    {noreply, NewState}.

start_push_replication(Name, Node) ->
    spawn_link(mem3_rep, go, [Name, Node]).

add_to_queue(State, DbName, Node) ->
    #state{dict=D, waiting=Waiting} = State,
    case dict:is_key({DbName, Node}, D) of
    true ->
        State;
    false ->
        twig:log(debug, "adding ~s -> ~p to mem3_sync queue", [DbName, Node]),
        State#state{
            dict = dict:store({DbName,Node}, ok, D),
            waiting = Waiting ++ [{DbName,Node}]
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
        Shards = mem3:shards(mem3:dbname(ShardName)),
        Targets = [S || #shard{node=N, name=Name} = S <- Shards, N =/= node(),
            Name =:= ShardName],
        Live = nodes(),
        [?MODULE:push(ShardName,N) || #shard{node=N} <- Targets,
            lists:member(N, Live)];
    ({deleted, <<"shards/", _:18/binary, _/binary>> = ShardName}) ->
        gen_server:cast(?MODULE, {remove_shard, ShardName});
    (_) -> ok end).

%% @doc Finds the next {DbName,Node} pair in the list of waiting replications
%% which does not correspond to an already running replication
-spec next_replication(list(), list()) -> {binary(),node(),list()} | nil.
next_replication(Active, Waiting) ->
    case lists:splitwith(fun({S,N}) -> is_running(S,N,Active) end, Waiting) of
    {_, []} ->
        nil;
    {Running, [{DbName,Node}|Rest]} ->
        {DbName, Node, Running ++ Rest}
    end.

is_running(DbName, Node, ActiveList) ->
    [] =/= [true || {S,N,_} <- ActiveList, S=:=DbName, N=:=Node].

remove_entries(Dict, Entries) ->
    lists:foldl(fun(Entry, D) -> dict:erase(Entry, D) end, Dict, Entries).
