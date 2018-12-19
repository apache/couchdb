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

-module(couch_epi_dispatch).

-export([
    dispatch/2
]).

%% Exports needed for tests
-export([
    app/0,
    providers/0,
    services/0,
    data_providers/0,
    data_subscriptions/0,
    processes/0,
    notify/3
]).


%% ------------------------------------------------------------------
%% API functions definitions
%% ------------------------------------------------------------------

dispatch(ServiceId, CallbackModule) ->
    couch_tests:new(?MODULE, dispatch,
        setup_dispatch(ServiceId, CallbackModule), teardown_dispatch()).

%% ------------------------------------------------------------------
%% setups and teardowns
%% ------------------------------------------------------------------

setup_dispatch(ServiceId, CallbackModule) ->
    fun(Fixture, Ctx0) ->
        Plugins = application:get_env(couch_epi, plugins, []),
        Ctx1 = start_epi(Ctx0, [CallbackModule]),
        couch_tests:set_state(Fixture, Ctx1, {ServiceId, CallbackModule, Plugins})
    end.

teardown_dispatch() ->
    fun(Fixture, Ctx0) ->
        {ServiceId, _Module, Plugins} = couch_tests:get_state(Fixture, Ctx0),
        stop_epi(Ctx0, ServiceId, Plugins)
    end.

%% ------------------------------------------------------------------
%% Helper functions definitions
%% ------------------------------------------------------------------

start_epi(Ctx0, Plugins) ->
    %% stop in case it's started from other tests..
    Ctx1 = couch_tests:stop_applications([couch_epi], Ctx0),
    application:unload(couch_epi),
    ok = application:load(couch_epi),
    ok = application:set_env(couch_epi, plugins, Plugins),
    couch_tests:start_applications([couch_epi], Ctx1).

stop_epi(Ctx0, ServiceId, Plugins) ->
    ok = application:set_env(couch_epi, plugins, Plugins),
    Handle = couch_epi:get_handle(ServiceId),
    catch couch_epi_module_keeper:reload(Handle),
    Ctx1 = couch_tests:stop_applications([couch_epi], Ctx0),
    application:unload(couch_epi),
    Ctx1.

%% ------------------------------------------------------------------
%% Tests
%% ------------------------------------------------------------------

%% EPI behaviour callbacks
app() -> test_app.
providers() -> [].
services() -> [].
data_providers() -> [].
data_subscriptions() -> [].
processes() -> [].
notify(_, _, _) -> ok.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

dispatch_test() ->
    ?assert(couch_tests:validate_fixture(dispatch(test_service, ?MODULE))).

-endif.
