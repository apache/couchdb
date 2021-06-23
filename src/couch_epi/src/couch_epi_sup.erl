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

-module(couch_epi_sup).

%% --------------------
%% Important assumption
%% ====================
%% Keeper and codechange_monitor childspecs rely on undocumented behaviour.
%% According to supervisor docs:
%%    ...if the child process is a supervisor, gen_server, or gen_fsm, this
%%    should be a list with one element [Module].
%% However it is perfectly fine to have more than one module in the list.
%% Modules property is used to determine if process is suspendable.
%% Only suspendable processes are hot code upgraded, others are killed.
%% The check looks like `lists:member(Module, Modules)`
%% The assumption is that it is indeed underdocumented fact and not
%% an implementation detail.

-behaviour(supervisor).

-include("couch_epi.hrl").

%% API
-export([start_link/0]).
-export([plugin_childspecs/2]).

%% Supervisor callbacks
-export([init/1]).

%% For testing
-export([
    plugin_childspecs/3
]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

plugin_childspecs(Plugin, Children) ->
    Plugins = application:get_env(couch_epi, plugins, []),
    plugin_childspecs(Plugin, Plugins, Children).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, {{one_for_one, 5, 10}, keepers()}}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

keepers() ->
    Plugins = application:get_env(couch_epi, plugins, []),
    Definitions = couch_epi_plugin:grouped_definitions(Plugins),
    Children = keeper_childspecs(Definitions),
    remove_duplicates(Children).

plugin_childspecs(Plugin, Plugins, Children) ->
    Definitions = couch_epi_plugin:grouped_definitions([Plugin]),
    ExtraChildren = couch_epi_plugin:plugin_processes(Plugin, Plugins),
    merge(ExtraChildren, Children) ++ childspecs(Definitions).

childspecs(Definitions) ->
    lists:map(
        fun({{Kind, Key}, Defs}) ->
            CodeGen = couch_epi_plugin:codegen(Kind),
            Handle = CodeGen:get_handle(Key),
            Modules = lists:append([modules(Spec) || {_App, Spec} <- Defs]),
            Name = service_name(Key) ++ "|" ++ atom_to_list(Kind),
            code_monitor(Name, [Handle], [Handle | Modules])
        end,
        Definitions
    ).

%% ------------------------------------------------------------------
%% Helper Function Definitions
%% ------------------------------------------------------------------

remove_duplicates(Definitions) ->
    lists:ukeysort(1, Definitions).

keeper_childspecs(Definitions) ->
    lists:map(
        fun({{Kind, Key}, _Specs}) ->
            Name = service_name(Key) ++ "|keeper",
            CodeGen = couch_epi_plugin:codegen(Kind),
            Handle = CodeGen:get_handle(Key),
            keeper(Name, [provider_kind(Kind), Key, CodeGen], [Handle])
        end,
        Definitions
    ).

keeper(Name, Args, Modules) ->
    {
        "couch_epi|" ++ Name,
        {couch_epi_module_keeper, start_link, Args},
        permanent,
        5000,
        worker,
        Modules
    }.

code_monitor(Name, Args, Modules0) ->
    Modules = [couch_epi_codechange_monitor | Modules0],
    {
        "couch_epi_codechange_monitor|" ++ Name,
        {couch_epi_codechange_monitor, start_link, Args},
        permanent,
        5000,
        worker,
        Modules
    }.

provider_kind(services) -> providers;
provider_kind(data_subscriptions) -> data_providers;
provider_kind(Kind) -> Kind.

service_name({ServiceId, Key}) ->
    atom_to_list(ServiceId) ++ ":" ++ atom_to_list(Key);
service_name(ServiceId) ->
    atom_to_list(ServiceId).

modules(#couch_epi_spec{kind = providers, value = Module}) ->
    [Module];
modules(#couch_epi_spec{kind = services, value = Module}) ->
    [Module];
modules(#couch_epi_spec{kind = data_providers, value = Value}) ->
    case Value of
        {static_module, Module} -> [Module];
        {callback_module, Module} -> [Module];
        _ -> []
    end;
modules(#couch_epi_spec{kind = data_subscriptions, behaviour = Module}) ->
    [Module].

merge([], Children) ->
    Children;
merge([{Id, _, _, _, _, _} = Spec | Rest], Children) ->
    merge(Rest, lists:keystore(Id, 1, Children, Spec));
merge([#{id := Id} = Spec | Rest], Children) ->
    Replace = fun
        (#{id := I}) when I == Id -> Spec;
        (E) -> E
    end,
    merge(Rest, lists:map(Replace, Children)).
