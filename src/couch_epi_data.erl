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

-module(couch_epi_data).

-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([childspec/4]).
-export([start_link/4, reload/1]).
-export([wait/1, stop/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
    subscriber, module, key, hash, handle,
    initialized = false, pending = []}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

childspec(Id, App, EpiKey, Module) ->
    {
        Id,
        {?MODULE, start_link, [
            App,
            EpiKey,
            Module,
            []
        ]},
        permanent,
        5000,
        worker,
        [Module]
    }.

start_link(SubscriberApp, {epi_key, Key}, Module, Options) ->
    gen_server:start_link(?MODULE, [SubscriberApp, Module, Key, Options], []).

reload(Server) ->
    gen_server:call(Server, reload).

wait(Server) ->
    gen_server:call(Server, wait).

stop(Server) ->
    catch gen_server:call(Server, stop).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Subscriber, Module, Key, _Options]) ->
    gen_server:cast(self(), init),
    {ok, #state{
        subscriber = Subscriber,
        module = Module,
        key = Key,
        handle = couch_epi_data_gen:get_handle(Key)}}.

handle_call(wait, _From, #state{initialized = true} = State) ->
    {reply, ok, State};
handle_call(wait, From, #state{pending = Pending} = State) ->
    {noreply, State#state{pending = [From | Pending]}};
handle_call(reload, _From, State) ->
    {Res, NewState} = reload_if_updated(State),
    {reply, Res, NewState};
handle_call(stop, _From, State) ->
    {stop, normal, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(init, #state{pending = Pending} = State) ->
    {_, NewState} = reload_if_updated(State),
    [gen_server:reply(Client, ok) || Client <- Pending],
    {noreply, NewState#state{initialized = true, pending = []}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {_, NewState} = reload_if_updated(State),
    {ok, NewState}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

reload_if_updated(#state{hash = OldHash, module = Module} = State) ->
    case couch_epi_functions_gen:hash([Module]) of
        OldHash ->
            {ok, State};
        Hash ->
            safe_set(Hash, State)
    end.

safe_set(Hash, #state{} = State) ->
    #state{
        handle = Handle,
        subscriber = Subscriber,
        module = Module,
        key = Key} = State,
    try
        Data = get_from_module(Module),
        OldData = current(Handle, Subscriber),
        ok = couch_epi_data_gen:set(Handle, Subscriber, Data),
        couch_epi_server:notify(Subscriber, Key, {data, OldData}, {data, Data}),
        {ok, State#state{hash = Hash}}
    catch Class:Reason ->
        {{Class, Reason}, State}
    end.

get_from_module(Module) ->
    try
        Module:data()
    catch
        error:undef -> []
    end.

current(Handle, Subscriber) ->
    try
        case couch_epi_data_gen:by_source(Handle, Subscriber) of
            undefined -> [];
            Data -> Data
        end
    catch error:undef ->
        []
    end.
