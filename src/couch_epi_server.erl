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

-module(couch_epi_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).
-export([subscribe/3, subscribe/4, unsubscribe/1, unsubscribe/2]).
-export([notify/4, notify/5]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(epi_server_state, {subscriptions}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

subscribe(App, Key, MFA) ->
    subscribe(?SERVER, App, Key, MFA).

subscribe(Server, App, Key, {_M, _F, _A} = MFA) ->
    gen_server:call(Server, {subscribe, App, Key, MFA}).

unsubscribe(Subscription) ->
    unsubscribe(?SERVER, Subscription).

unsubscribe(Server, Subscription) ->
    gen_server:call(Server, {unsubscribe, Subscription}).

notify(App, Key, OldData, Data) ->
    notify(?SERVER, App, Key, OldData, Data).

notify(Server, App, Key, OldData, Data) ->
    gen_server:cast(Server, {notify, App, Key, OldData, Data}).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    State = #epi_server_state{subscriptions = dict:new()},
    {ok, State}.

handle_call({subscribe, App, Key, MFA}, {Pid, _Tag},
        #epi_server_state{subscriptions = Subscriptions0} = State0) ->
    {Subscription, Subscriptions1} = add(Pid, Subscriptions0, App, Key, MFA),
    State1 = State0#epi_server_state{subscriptions = Subscriptions1},
    {reply, {ok, Subscription}, State1};
handle_call({unsubscribe, Subscription}, _From,
        #epi_server_state{subscriptions = Subscriptions0} = State0) ->
    Subscriptions1 = remove(Subscriptions0, Subscription),
    State1 = State0#epi_server_state{subscriptions = Subscriptions1},
    {reply, ok, State1};
handle_call(_Request, _From, State) ->
    {stop, normal, State}.

handle_cast({notify, App, Key, OldData, Data},
        #epi_server_state{subscriptions = Subscriptions} = State) ->
    Subscribers = subscribers(Subscriptions, App, Key),
    notify_subscribers(Subscribers, App, Key, OldData, Data),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', MonitorRef, Type, Object, Info},
        #epi_server_state{subscriptions = Subscriptions0} = State0) ->
    Subscriptions1 = remove(Subscriptions0, MonitorRef),
    State1 = State0#epi_server_state{subscriptions = Subscriptions1},
    {noreply, State1};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

subscribers(Subscriptions, App, Key) ->
    case dict:find({App, Key}, Subscriptions) of
        error ->
            [];
        {ok, Subscribers} ->
            Subscribers
    end.

add(Pid, Subscriptions, App, Key, MFA) ->
    Subscription = erlang:monitor(process, Pid),
    {Subscription, dict:append({App, Key}, {Subscription, MFA}, Subscriptions)}.

remove(Subscriptions, SubscriptionId) ->
    case find(Subscriptions, SubscriptionId) of
        {App, Key} ->
            demonitor(SubscriptionId, [flush]),
            delete_subscriber(Subscriptions, App, Key, SubscriptionId);
        _ ->
            Subscriptions
    end.

find(Subscriptions, SubscriptionId) ->
    dict:fold(fun(Key, Subscribers, Acc) ->
        case [ok || {Id, _MFA} <- Subscribers, Id =:= SubscriptionId] of
            [_] ->
                Key;
            [] ->
                Acc
        end
    end, not_found, Subscriptions).

delete_subscriber(Subscriptions, App, Key, SubscriptionId) ->
    dict:update({App, Key}, fun(Subscribers) ->
        [{Id, MFA} || {Id, MFA} <- Subscribers, Id =/= SubscriptionId]
    end, Subscriptions).

notify_subscribers(Subscribers, App, Key, OldData, Data) ->
    [M:F(App, Key, OldData, Data, A) || {_Id, {M, F, A}} <- Subscribers].
