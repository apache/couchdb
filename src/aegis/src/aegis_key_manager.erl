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

-module(aegis_key_manager).

-behaviour(gen_server).

-vsn(1).


-type key() :: binary().
-type aegis_config() :: term().
-type key_manager_state() :: term().


-callback init() -> key_manager_state().


-callback generate_key(
    St :: key_manager_state(),
    Db :: #{},
    DbOptions :: list()) ->
        {ok, key(), aegis_config()} | {ok, false}.


-callback unwrap_key(
    St :: key_manager_state(),
    Db :: #{},
    AegisConfig :: aegis_config()) ->
        {ok, key(), aegis_config()}.


%% aegis_key_manager API
-export([
    start_link/0,
    generate_key/2,
    unwrap_key/2
]).


%% gen_server callbacks
-export([
    init/1,
    terminate/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3
]).



start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


generate_key(#{} = Db, Options) ->
    gen_server:call(?MODULE, {generate_key, Db, Options}).


unwrap_key(#{} = Db, AegisConfig) ->
    gen_server:call(?MODULE, {unwrap_key, Db, AegisConfig}).




init([]) ->
    process_flag(sensitive, true),
    Store = ets:new(?MODULE, [set, private]),
    State = ?AEGIS_KEY_MANAGER:init(),
    true = ets:insert(Store, {?AEGIS_KEY_MANAGER, State}),
    {ok, Store}.


terminate(_Reason, _Store) ->
    ok.


handle_call({Method, Db, Opts}, From, Store)
        when Method == generate_key; Method == unwrap_key ->
    [{?AEGIS_KEY_MANAGER, State}] = ets:lookup(Store, ?AEGIS_KEY_MANAGER),
    erlang:spawn(fun() ->
        process_flag(sensitive, true),
        try
           ?AEGIS_KEY_MANAGER:Method(State, Db, Opts)
        of
            Resp ->
                gen_server:reply(From, Resp)
        catch
            _:Error ->
                gen_server:reply(From, {error, Error})
        end
    end),
    {noreply, Store}.


handle_cast(_Msg, Store) ->
    {noreply, Store}.


handle_info(_Msg, Store) ->
    {noreply, Store}.


code_change(_OldVsn, Store, _Extra) ->
    {ok, Store}.
