-module(mem3_sync_event).
-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2,
    code_change/3]).

init(_) ->
    {ok, nil}.

handle_event({Up, Node}, State) when Up == nodeup; Up == node_join ->
    mem3_sync:add_node(Node);

handle_event({Down, Node}, State) when Down == nodedown; Down == node_leave ->
    mem3_sync:remove_node(Node);

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
