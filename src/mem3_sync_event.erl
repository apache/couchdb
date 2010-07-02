-module(mem3_sync_event).
-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2,
    code_change/3]).

init(_) ->
    {ok, nil}.

handle_event({add_node, Node}, State) ->
    Db1 = list_to_binary(couch_config:get("mem3", "node_db", "nodes")),
    Db2 = list_to_binary(couch_config:get("mem3", "shard_db", "dbs")),
    [mem3_sync:push(Db, Node) || Db <- [Db1, Db2]],
    {ok, State};

handle_event({nodeup, Node}, State) ->
    case lists:member(Node, mem3:nodes()) of
    true ->
        Db1 = list_to_binary(couch_config:get("mem3", "node_db", "nodes")),
        Db2 = list_to_binary(couch_config:get("mem3", "shard_db", "dbs")),
        [mem3_sync:push(Db, Node) || Db <- [Db1, Db2]];
    false ->
        ok
    end,
    {ok, State};

handle_event({Down, Node}, State) when Down == nodedown; Down == remove_node ->
    mem3_sync:remove_node(Node),
    {ok, State};

handle_event(_Event, State) ->
    {ok, State}.

handle_call(_Request, State) ->
    {ok, ok, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
