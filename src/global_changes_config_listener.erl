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

-vsn(2).
-behavior(config_listener).


-export([
    subscribe/0
]).

-export([
    handle_config_change/5,
    handle_config_terminate/3
]).


-define(LISTENER, global_changes_listener).
-define(SERVER, global_changes_server).

subscribe() ->
    config:listen_for_changes(?MODULE, nil).

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

handle_config_terminate(_, _, _) ->
    spawn(fun() ->
        timer:sleep(5000),
        config:listen_for_changes(?MODULE, nil)
    end).
