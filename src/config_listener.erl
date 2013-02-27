-module(config_listener).

-behaviour(gen_event).

%% Public interface
-export([start/2]).
-export([start/3]).

-export([behaviour_info/1]).

%% Required gen_event interface
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, 
    code_change/3]).

behaviour_info(callbacks) ->
    [{handle_config_change,5}];
behaviour_info(_) ->
    undefined.

start(Module, State) ->
    start(Module, Module, State).

start(Module, Id, State) ->
    gen_event:add_sup_handler(config_event, {?MODULE, Id}, {Module, State}).

init({Module, State}) ->
    {ok, {Module, State}}.

handle_event({config_change, Sec, Key, Value, Persist}, {Module, State}) ->
    case Module:handle_config_change(Sec, Key, Value, Persist, State) of
        {ok, NewState} ->
            {ok, {Module, NewState}};
        remove_handler ->
            remove_handler
    end.

handle_call(_Request, St) ->
    {ok, ignored, St}.

handle_info(_Info, St) ->
    {ok, St}.

terminate(_Reason, _St) ->
    ok.

code_change(_OldVsn, St, _Extra) ->
    {ok, St}.
