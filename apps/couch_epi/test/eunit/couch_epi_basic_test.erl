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

-module(couch_epi_basic_test).

-export([
    start_link/0
]).

-export([
    app/0,
    providers/0,
    services/0,
    data_providers/0,
    data_subscriptions/0,
    processes/0,
    notify/3
]).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

start_link() -> ok.

%% BEGIN couch_epi_plugin behaviour callbacks

app() -> test_app.

providers() ->
    [
        {my_service, provider1},
        {my_service, provider2}
    ].

services() ->
    [
        {my_service, ?MODULE}
    ].

data_providers() ->
    [
        {{test_app, descriptions}, {static_module, ?MODULE}, [{interval, 100}]}
    ].

data_subscriptions() ->
    [
        {test_app, descriptions}
    ].

processes() ->
    [
        {?MODULE, [?CHILD(extra_process, worker)]},
        {?MODULE, [{to_replace, {new, start_link, [bar]}, permanent, 5000, worker, [bar]}]},
        {?MODULE, [
            #{
                id => to_replace_map,
                start => {new, start_link, [bar]},
                modules => [bar]
            }
        ]}
    ].

notify(_Key, _OldData, _NewData) ->
    ok.

%% END couch_epi_plugin behaviour callbacks

parse_child_id(Id) when is_atom(Id) ->
    Id;
parse_child_id(Id) ->
    ["couch_epi_codechange_monitor", ServiceName, KindStr] =
        string:tokens(Id, "|"),
    Kind = list_to_atom(KindStr),
    case string:tokens(ServiceName, ":") of
        [ServiceId, Key] ->
            {{list_to_atom(ServiceId), list_to_atom(Key)}, Kind};
        [Key] ->
            {list_to_atom(Key), Kind}
    end.

-include_lib("eunit/include/eunit.hrl").

basic_test() ->
    Expected = [
        {extra_process, [], [extra_process]},
        {to_replace, [bar], [bar]},
        {to_replace_map, [bar], [bar]},
        {{my_service, providers}, [couch_epi_functions_gen_my_service], [
            couch_epi_codechange_monitor,
            couch_epi_functions_gen_my_service,
            provider1,
            provider2
        ]},
        {
            {my_service, services},
            [couch_epi_functions_gen_my_service],
            lists:sort([
                couch_epi_codechange_monitor,
                couch_epi_functions_gen_my_service,
                ?MODULE
            ])
        },
        {
            {{test_app, descriptions}, data_subscriptions},
            [couch_epi_data_gen_test_app_descriptions],
            lists:sort([
                couch_epi_codechange_monitor,
                couch_epi_data_gen_test_app_descriptions,
                ?MODULE
            ])
        },
        {
            {{test_app, descriptions}, data_providers},
            [couch_epi_data_gen_test_app_descriptions],
            lists:sort([
                couch_epi_codechange_monitor,
                couch_epi_data_gen_test_app_descriptions,
                ?MODULE
            ])
        }
    ],

    ToReplace = [
        {to_replace, {old, start_link, [foo]}, permanent, 5000, worker, [foo]},
        #{id => to_replace_map, start => {old, start_link, [foo]}}
    ],
    Children = lists:sort(
        couch_epi_sup:plugin_childspecs(
            ?MODULE, [?MODULE], ToReplace
        )
    ),

    Results = lists:map(
        fun
            ({Id, {_M, _F, Args}, _, _, _, Modules}) ->
                {parse_child_id(Id), Args, lists:sort(Modules)};
            (#{id := Id, start := {_M, _F, Args}, modules := Modules}) ->
                {parse_child_id(Id), Args, lists:sort(Modules)}
        end,
        Children
    ),

    Tests = lists:zip(lists:sort(Expected), lists:sort(Results)),
    [?assertEqual(Expect, Result) || {Expect, Result} <- Tests],

    ExpectedChild = {to_replace, {new, start_link, [bar]}, permanent, 5000, worker, [bar]},
    ?assertEqual(
        ExpectedChild,
        lists:keyfind(to_replace, 1, Children)
    ),

    ExpectedMapChildSpec = #{
        id => to_replace_map,
        start => {new, start_link, [bar]},
        modules => [bar]
    },
    [MapChildSpec] = [E || #{id := to_replace_map} = E <- Children],
    ?assertEqual(ExpectedMapChildSpec, MapChildSpec),
    ok.
