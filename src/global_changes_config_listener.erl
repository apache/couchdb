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

-module(global_changes_config_listener).
-behaviour(gen_server).
-vsn(1).
-behavior(config_listener).


-export([
    start_link/0
]).

-export([
    init/1,
    terminate/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3
]).

-export([
    handle_config_change/5
]).


-define(LISTENER, global_changes_listener).
-define(SERVER, global_changes_server).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init([]) ->
    ok = config:listen_for_changes(?MODULE, nil),
    {ok, nil}.


terminate(_, _St) ->
    ok.


handle_call(Msg, _From, St) ->
    {stop, {invalid_call, Msg}, {invalid_call, Msg}, St}.


handle_cast(Msg, St) ->
    {stop, {invalid_cast, Msg}, St}.


handle_info({gen_event_EXIT, {config_listener, ?MODULE}, _Reason}, St) ->
    erlang:send_after(5000, self(), restart_config_listener),
    {noreply, St};
handle_info(restart_config_listener, St) ->
    ok = config:listen_for_changes(?MODULE, St),
    {noreply, St};
handle_info(_Msg, St) ->
    {noreply, St}.


code_change(_, St, _) ->
    {ok, St}.


handle_config_change("global_changes", "max_event_delay", MaxDelayStr, _, _) ->
    try list_to_integer(MaxDelayStr) of
        MaxDelay ->
            gen_server:cast(?LISTENER, {set_max_event_delay, MaxDelay})
    catch error:badarg ->
        ok
    end,
    {ok, nil};

handle_config_change("global_changes", "max_write_delay", MaxDelayStr, _, _) ->
    try list_to_integer(MaxDelayStr) of
        MaxDelay ->
            gen_server:cast(?SERVER, {set_max_write_delay, MaxDelay})
    catch error:badarg ->
        ok
    end,
    {ok, nil};

handle_config_change("global_changes", "update_db", "false", _, _) ->
    gen_server:cast(?LISTENER, {set_update_db, false}),
    gen_server:cast(?SERVER, {set_update_db, false}),
    {ok, nil};

handle_config_change("global_changes", "update_db", _, _, _) ->
    gen_server:cast(?LISTENER, {set_update_db, true}),
    gen_server:cast(?SERVER, {set_update_db, true}),
    {ok, nil};

handle_config_change(_, _, _, _, _) ->
    {ok, nil}.
