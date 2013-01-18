-module(config_listener).

-behaviour(gen_event).

%% Public interface
-export([start/2]).

-export([behaviour_info/1]).

%% Required gen_event interface
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, 
    code_change/3]).

behaviour_info(callbacks) ->
    [{handle_config_change,5}];
behaviour_info(_) ->
    undefined.

start(Module, State) ->
    gen_event:add_sup_handler(config_event, {?MODULE, Module}, {Module, State}).

init({Module, State}) ->
    {ok, {Module, State}}.

handle_event({config_change, Sec, Key, Value, Persist}, {Module, State}) ->
    case Module:handle_config_change(Sec, Key, Value, Persist, State) of
        {ok, NewState} ->
            {ok, {Module, NewState}};
        remove_handler ->
            remove_handler
    end.

handle_call(_Request, Module) ->
    {ok, ignored, St}.

handle_info(_Info, Module) ->
    {ok, St}.

terminate(_Reason, Module) ->
    ok.

code_change(_OldVsn, Module, _Extra) ->
    {ok, St}.
