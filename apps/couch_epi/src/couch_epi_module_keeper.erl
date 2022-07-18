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

-module(couch_epi_module_keeper).

-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/3, stop/1]).
-export([reload/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {
    codegen,
    module,
    key,
    type,
    handle,
    hash,
    kind,
    timer = {undefined, undefined}
}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Type, Key, Codegen) ->
    Handle = Codegen:get_handle(Key),
    gen_server:start_link(
        {local, Handle}, ?MODULE, [Type, Codegen, Key, Handle], []
    ).

stop(Server) ->
    catch gen_server:call(Server, stop).

reload(Server) ->
    gen_server:call(Server, reload).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Kind, Codegen, Key, Handle]) ->
    Type = type(Kind),
    State = #state{
        codegen = Codegen,
        key = Key,
        type = Type,
        handle = Handle,
        kind = Kind
    },
    compile_module(State).

handle_call(reload, _From, State0) ->
    {Reply, State1} = reload_if_updated(State0),
    {reply, Reply, State1};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(tick, State0) ->
    {_Res, State1} = reload_if_updated(State0),
    {noreply, State1};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State0, _Extra) ->
    {_Res, State1} = reload_if_updated(State0),
    {ok, State1}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

type(data_providers) -> couch_epi_data;
type(providers) -> couch_epi_functions;
type(services) -> couch_epi_functions.

reload_if_updated(#state{handle = Module} = State) ->
    case couch_epi_util:module_exists(Module) of
        true ->
            do_reload_if_updated(State);
        false ->
            {ok, State}
    end.

compile_module(State) ->
    do_reload_if_updated(State).

do_reload_if_updated(#state{} = State0) ->
    #state{
        hash = OldHash,
        type = Type,
        key = Key,
        kind = Kind
    } = State0,
    Defs = couch_epi_plugin:definitions(Kind, Key),
    case Type:data(Defs) of
        {ok, OldHash, _Data} ->
            {ok, State0};
        {ok, Hash, Data} ->
            {ok, OldData, State1} = safe_set(Hash, Data, State0),
            notify(Key, OldData, Data, Defs),
            State2 = update_interval(Type:interval(Defs), State1),
            {ok, State2};
        Else ->
            {Else, State0}
    end.

update_interval(undefined, #state{timer = Timer} = State) ->
    State#state{timer = cancel_timer(Timer)};
update_interval(Interval, #state{timer = Timer} = State) ->
    State#state{timer = start_timer(Interval, Timer)}.

start_timer(Interval, {undefined, undefined}) ->
    {ok, Timer} = timer:send_interval(Interval, self(), tick),
    {Timer, Interval};
start_timer(Interval, {Timer, _Interval}) ->
    start_timer(Interval, cancel_timer(Timer)).

cancel_timer({undefined, undefined}) ->
    {undefined, undefined};
cancel_timer({Timer, _Interval}) ->
    timer:cancel(Timer),
    {undefined, undefined}.

safe_set(Hash, Data, #state{} = State) ->
    #state{
        handle = Handle,
        codegen = CodeGen
    } = State,
    try
        OldData = CodeGen:get_current_definitions(Handle),
        ok = CodeGen:generate(Handle, Data),
        {ok, OldData, State#state{hash = Hash}}
    catch
        Class:Reason ->
            {{Class, Reason}, State}
    end.

notify(Key, OldData, NewData, Defs) ->
    Specs = [Spec || {_App, Spec} <- Defs],
    couch_epi_plugin:notify(Key, OldData, NewData, Specs),
    ok.
