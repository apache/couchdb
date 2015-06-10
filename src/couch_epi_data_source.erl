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

-module(couch_epi_data_source).

-behaviour(gen_server).
-define(MONITOR_INTERVAL, 5000).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([childspec/5]).
-export([start_link/4, reload/1]).
-export([wait/1, stop/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
    subscriber, locator, key, hash, handle,
    initialized = false, pending = []}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

childspec(Id, App, EpiKey, Locator, Options) ->
    {
        Id,
        {?MODULE, start_link, [
            App,
            EpiKey,
            Locator,
            Options
        ]},
        permanent,
        5000,
        worker,
        [?MODULE]
    }.

start_link(SubscriberApp, {epi_key, Key}, Src, Options) ->
    {ok, Locator} = locate(SubscriberApp, Src),
    gen_server:start_link(?MODULE, [SubscriberApp, Locator, Key, Options], []).

reload(Server) ->
    gen_server:call(Server, reload).

wait(Server) ->
    gen_server:call(Server, wait).

stop(Server) ->
    catch gen_server:call(Server, stop).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Subscriber, Locator, Key, Options]) ->
    gen_server:cast(self(), init),
    Interval = proplists:get_value(interval, Options, ?MONITOR_INTERVAL),
    {ok, _Timer} = timer:send_interval(Interval, self(), tick),
    {ok, #state{
        subscriber = Subscriber,
        locator = Locator,
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

handle_info(tick, State0) ->
    {_Res, State1} = reload_if_updated(State0),
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

locate(App, {priv_file, FileName}) ->
    case priv_path(App, FileName) of
        {ok, FilePath} ->
            ok = ensure_exists(FilePath),
            {ok, {file, FilePath}};
        Else ->
            Else
    end;
locate(_App, {file, FilePath}) ->
    ok = ensure_exists(FilePath),
    {ok, {file, FilePath}}.

priv_path(AppName, FileName) ->
    case code:priv_dir(AppName) of
        {error, _Error} = Error ->
            Error;
        Dir ->
            {ok, filename:join(Dir, FileName)}
    end.

ensure_exists(FilePath) ->
    case filelib:is_regular(FilePath) of
        true ->
            ok;
        false ->
            {error, {notfound, FilePath}}
    end.

reload_if_updated(#state{hash = OldHash, locator = Locator} = State) ->
    case read(Locator) of
        {ok, OldHash, _Data} ->
            {ok, State};
        {ok, Hash, Data} ->
            safe_set(Hash, Data, State);
        Else ->
            {Else, State}
    end.

safe_set(Hash, Data, #state{} = State) ->
    #state{
        handle = Handle,
        subscriber = Subscriber,
        key = Key} = State,

    try
        OldData = current(Handle, Subscriber),
        ok = couch_epi_data_gen:set(Handle, Subscriber, Data),
        couch_epi_server:notify(Subscriber, Key, {data, OldData}, {data, Data}),
        {ok, State#state{hash = Hash}}
    catch Class:Reason ->
        {{Class, Reason}, State}
    end.

read({file, FilePath}) ->
    case file:consult(FilePath) of
        {ok, Data} ->
            {ok, hash_of_file(FilePath), Data};
        {error, Reason} ->
            {error, {FilePath, Reason}}
    end.

hash_of_file(FilePath) ->
    {ok, Data} = file:read_file(FilePath),
    crypto:hash(md5, Data).

current(Handle, Subscriber) ->
    try
        case couch_epi_data_gen:by_source(Handle, Subscriber) of
            undefined -> [];
            Data -> Data
        end
    catch error:undef ->
        []
    end.
