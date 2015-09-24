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

-module(couch_epi_plugin).

-include("couch_epi.hrl").

-export([
    definitions/1,
    definitions/2,
    grouped_definitions/1,
    plugin_processes/2,
    codegen/1
]).

-export([notify/4]).

%% ------------------------------------------------------------------
%% Types Definitions
%% ------------------------------------------------------------------

-type kind()
    :: providers
        | data_providers
        | services
        | data_subscriptions
    .

-type key()
    :: {ServiceId :: couch_epi:service_id(), Key :: couch_epi:key()}
        | couch_epi:service_id().

-callback app() -> couch_epi:app().
-callback providers() -> [{couch_epi:service_id(), module()}].
-callback services() -> [{couch_epi:service_id(), module()}].
-callback data_subscriptions() -> [{couch_epi:service_id(), couch_epi:key()}].
-callback data_providers() -> [{couch_epi:service_id(), couch_epi:data_spec()}].
-callback processes() -> [{couch_epi:plugin_id(), [supervisor:child_spec()]}].
-callback notify(Key :: term(), Old :: term(), New :: term()) -> ok.

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

definitions(Plugins) ->
    lists:append([extract_definitions(Plugin) || Plugin <- Plugins]).

plugin_processes(Plugin, Plugins) ->
    lists:append([
        Specs || P0 <- Plugins, {P1, Specs} <- P0:processes(), P1 =:= Plugin]).

grouped_definitions(Plugins) ->
    Defs = lists:append([extract_definitions(Plugin) || Plugin <- Plugins]),
    group_specs(Defs).

definitions(Kind, Key) ->
    Plugins = application:get_env(couch_epi, plugins, []),
    Definitions = definitions(Plugins),
    Filtered = filter_by_key(Definitions, Kind, Key),
    case group_specs(Filtered) of
        [] -> [];
        [{_, Defs}] -> Defs
    end.

notify(Key, OldData, NewData, Specs) ->
    Plugins = lists:usort([Plugin || #couch_epi_spec{behaviour = Plugin} <- Specs]),
    [notify_plugin(Plugin, Key, OldData, NewData) || Plugin <- Plugins],
    ok.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

notify_plugin(Plugin, Key, OldData, NewData) ->
    App = Plugin:app(),
    Plugin:notify(Key, app_data(App, OldData), app_data(App, NewData)).


app_data(App, Data) ->
    case lists:keyfind(App, 1, Data) of
        {App, AppData} -> AppData;
        false -> []
    end.

filter_by_key(Definitions, Kind, Key) ->
    lists:filter(fun(Spec) -> by_key(Spec, Kind, Key) end, Definitions).

by_key(#couch_epi_spec{kind = Kind, key = Key}, Kind, Key) -> true;
by_key(_, _, _) -> false.


extract_definitions(Plugin) ->
    specs(Plugin, providers)
        ++ specs(Plugin, data_providers)
        ++ specs(Plugin, services)
        ++ specs(Plugin, data_subscriptions).

-spec group_specs(Specs :: [#couch_epi_spec{}]) -> GroupedSpecs when
    GroupedSpecs ::
        [{{kind(), key()}, [{couch_epi:app(), #couch_epi_spec{}}]}].

group_specs(Specs) ->
    group(
        [{{Kind, Key}, group([{App, Spec}])}
            || #couch_epi_spec{kind = Kind, key = Key, app = App} = Spec <- Specs]).


group(KV) ->
    dict:to_list(lists:foldr(fun({K,V}, D) ->
        dict:append_list(K, V, D)
    end, dict:new(), KV)).

specs(Plugin, Kind) ->
    [spec(parse(Spec, Kind), Plugin, Kind) || Spec <- Plugin:Kind()].

spec({Key, Value, Options}, Plugin, Kind) ->
    App = Plugin:app(),
    #couch_epi_spec{
        app = App,
        behaviour = Plugin,
        kind = Kind,
        options = Options,
        key = Key,
        value = Value,
        codegen = codegen(Kind),
        type = type(Kind, Value)
    }.

parse({Key, Value}, Kind) ->
    parse({Key, Value, []}, Kind);
parse({Key, Value, Options}, data_subscriptions) ->
    {{Key, Value}, undefined, Options};
parse({_, _, _} = Tuple, _Kind) ->
    Tuple.

codegen(providers) -> couch_epi_functions_gen;
codegen(services) -> couch_epi_functions_gen;
codegen(data_providers) -> couch_epi_data_gen;
codegen(data_subscriptions) -> couch_epi_data_gen.

type(providers, _) -> couch_epi_functions;
type(services, _) -> couch_epi_functions;
type(data_providers, _) -> couch_epi_data;
type(data_subscriptions, _) -> undefined.


%% ------------------------------------------------------------------
%% Tests
%% ------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

plugin_module(foo_epi) ->
    "
        -compile([export_all]).

        app() -> foo.
        providers() ->
            [
                {chttpd_handlers, foo_provider},
                {bar_handlers, bar_provider}
            ].

        services() ->
            [
                {foo_handlers, foo_service}
            ].

        data_providers() ->
            [
                {{foo_service, data1}, {file, \"abs_file\"}, [{interval, 5000}]},
                {{foo_service, data2}, {priv_file, \"priv_file\"}},
                {{foo_service, data3}, {module, foo_data}}
            ].

        data_subscriptions() ->
            [
                {stats, foo_definitions}
            ].

        processes() -> [].

        notify(_, _, _) -> ok.
    ";
plugin_module(bar_epi) ->
    "
        -compile([export_all]).

        app() -> bar.
        providers() ->
            [
                {chttpd_handlers, bar_provider},
                {bar_handlers, bar_provider}
            ].

        services() ->
            [
                {bar_handlers, bar_service}
            ].

        data_providers() ->
            [].

        data_subscriptions() ->
            [
                {foo_service, data1}
            ].

        processes() -> [].

        notify(_, _, _) -> ok.
    ".

generate_module(Name, Body) ->
    Tokens = couch_epi_codegen:scan(Body),
    couch_epi_codegen:generate(Name, Tokens).

generate_modules(Kind, Providers) ->
    [generate_module(P, Kind(P)) || P <- Providers].

definitions_test() ->
    Expected = lists:sort([
        #couch_epi_spec{
            behaviour = bar_epi,
            app = bar,
            kind = providers,
            options = [],
            key = bar_handlers,
            value = bar_provider,
            codegen = couch_epi_functions_gen,
            type = couch_epi_functions
        },
        #couch_epi_spec{
            behaviour = bar_epi,
            app = bar,
            kind = services,
            options = [],
            key = bar_handlers,
            value = bar_service,
            codegen = couch_epi_functions_gen,
            type = couch_epi_functions
        },
        #couch_epi_spec{
            behaviour = bar_epi,
            app = bar,
            kind = providers,
            options = [],
            key = chttpd_handlers,
            value = bar_provider,
            codegen = couch_epi_functions_gen,
            type = couch_epi_functions
        },
        #couch_epi_spec{
            behaviour = bar_epi,
            app = bar,
            kind = data_subscriptions,
            options = [],
            key = {foo_service, data1},
            value = undefined,
            codegen = couch_epi_data_gen
        },
        #couch_epi_spec{
            behaviour = foo_epi,
            app = foo,
            kind = providers,
            options = [],
            key = bar_handlers,
            value = bar_provider,
            codegen = couch_epi_functions_gen,
            type = couch_epi_functions
        },
        #couch_epi_spec{
            behaviour = foo_epi,
            app = foo,
            kind = providers,
            options = [],
            key = chttpd_handlers,
            value = foo_provider,
            codegen = couch_epi_functions_gen,
            type = couch_epi_functions},
        #couch_epi_spec{
            behaviour = foo_epi,
            app = foo,
            kind = services,
            options = [],
            key = foo_handlers,
            value = foo_service,
            codegen = couch_epi_functions_gen,
            type = couch_epi_functions},
        #couch_epi_spec{
            behaviour = foo_epi,
            app = foo,
            kind = data_providers,
            options = [{interval, 5000}],
            key = {foo_service, data1},
            value = {file,"abs_file"},
            codegen = couch_epi_data_gen,
            type = couch_epi_data
        },
        #couch_epi_spec{
            behaviour = foo_epi,
            app = foo,
            kind = data_providers,
            options = [],
            key = {foo_service, data2},
            value = {priv_file, "priv_file"},
            codegen = couch_epi_data_gen,
            type = couch_epi_data
        },
        #couch_epi_spec{
            behaviour = foo_epi,
            app = foo,
            kind = data_providers,
            options = [],
            key = {foo_service, data3},
            value = {module, foo_data},
            codegen = couch_epi_data_gen,
            type = couch_epi_data
        },
        #couch_epi_spec{
            behaviour = foo_epi,
            app = foo,
            kind = data_subscriptions,
            options = [],
            key = {stats, foo_definitions},
            value = undefined,
            codegen = couch_epi_data_gen
        }
    ]),

    [ok,ok] = generate_modules(fun plugin_module/1, [foo_epi, bar_epi]),
    Tests = lists:zip(Expected, lists:sort(definitions([foo_epi, bar_epi]))),
    [?assertEqual(Expect, Result) || {Expect, Result} <- Tests],
    ok.
-endif.
