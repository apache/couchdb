-module(mem3_event).

-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2,
    code_change/3]).

-include("mem3.hrl").

init([]) ->
    {ok, []}.

handle_event({node_join, Node}, State) ->
    start_repl({node_join, Node}, State);

handle_event({nodeup, Node}, State) ->
    start_repl({nodeup, Node}, State);

handle_event({node_leave, Node}, State) ->
    stop_repl({node_leave, Node}, State);

handle_event({nodedown, Node}, State) ->
    stop_repl({nodedown, Node}, State);

handle_event(Event, State) ->
    ?LOG_ERROR("unexpected event in dbs handler ~p", [Event]),
    {ok, State}.

handle_call(Request, State) ->
    ?LOG_ERROR("unexpected call in dbs handler ~p", [Request]),
    {ok, ok, State}.

handle_info(Info, State) ->
    ?LOG_ERROR("unexpected msg in dbs handler ~p", [Info]),
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
%% internal
%%

start_repl({Reason, Node}, State) ->
    ChildSpec = dbs:childspec(Node),
    case supervisor:start_child(dbs, ChildSpec) of
    {ok, _} ->
        ok;
    {error, {already_started, _Child}} ->
        ok;
    {error, running} ->
        ok;
    {error, already_present} ->
        case supervisor:restart_child(dbs, ChildSpec) of
        {ok, _} ->
            ok;
        {error, running} ->
            ok;
        {error, Reason} ->
            ?LOG_ERROR("dbs repl restart failed ~p", [Reason])
        end;
    {error, Reason} ->
        ?LOG_ERROR("dbs repl start failed ~p", [Reason])
    end,
    {ok, State}.

stop_repl({Reason, Node}, State) ->
    ?LOG_INFO("dbs repl ~p --> ~p terminating (~p)", [node(), Node, Reason]),
    supervisor:terminate_child(dbs, Node),
    supervisor:delete_child(dbs, Node),
    {ok, State}.
