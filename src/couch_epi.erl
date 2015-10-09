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

-module(couch_epi).

-export([get_handle/1]).
-export([register_service/2]).

%% queries and introspection
-export([
    dump/1, get/2, get_value/3,
    by_key/1, by_key/2, by_source/1, by_source/2,
    keys/1, subscribers/1]).

%% apply
-export([apply/5, decide/5]).
-export([any/5, all/5]).

-export([is_configured/3]).

%% ------------------------------------------------------------------
%% Types Definitions
%% ------------------------------------------------------------------

-export_type([
    service_id/0,
    app/0,
    key/0,
    handle/0,
    plugin_id/0,
    data_spec/0
]).

-type app() :: atom().
-type key() :: term().
-type service_id() :: atom().

-type properties() :: [{key(), term()}].

-type plugin_id() :: module().

-opaque handle() :: module().

-type apply_opt()
    :: ignore_errors
        | concurrent
        | pipe.

-type apply_opts() :: [apply_opt()].

-type data_spec()
    :: {module, module()}
        | {priv_file, FileName :: string()}
        | {file, FileName :: string()}.


%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec dump(Handle :: handle()) ->
    [Config :: properties()].

dump(Handle) when Handle /= undefined ->
    couch_epi_data_gen:get(Handle).

-spec get(Handle :: handle(), Key :: key()) ->
    [Config :: properties()].

get(Handle, Key) when Handle /= undefined ->
    couch_epi_data_gen:get(Handle, Key).

-spec get_value(Handle :: handle(), Subscriber :: app(), Key :: key()) ->
    properties().

get_value(Handle, Subscriber, Key) when Handle /= undefined ->
    couch_epi_data_gen:get(Handle, Subscriber, Key).


-spec by_key(Handle :: handle()) ->
    [{Key :: key(), [{Source :: app(), properties()}]}].

by_key(Handle) when Handle /= undefined ->
    couch_epi_data_gen:by_key(Handle).


-spec by_key(Handle :: handle(), Key :: key()) ->
    [{Source :: app(), properties()}].

by_key(Handle, Key) when Handle /= undefined ->
    couch_epi_data_gen:by_key(Handle, Key).


-spec by_source(Handle :: handle()) ->
    [{Source :: app(), [{Key :: key(), properties()}]}].

by_source(Handle) when Handle /= undefined ->
    couch_epi_data_gen:by_source(Handle).


-spec by_source(Handle :: handle(), Subscriber :: app()) ->
    [{Key :: key(), properties()}].

by_source(Handle, Subscriber) when Handle /= undefined ->
    couch_epi_data_gen:by_source(Handle, Subscriber).


-spec keys(Handle :: handle()) ->
    [Key :: key()].

keys(Handle) when Handle /= undefined ->
    couch_epi_data_gen:keys(Handle).


-spec subscribers(Handle :: handle()) ->
    [Subscriber :: app()].

subscribers(Handle) when Handle /= undefined ->
    couch_epi_data_gen:subscribers(Handle).

-spec apply(Handle :: handle(), ServiceId :: atom(), Function :: atom(),
    Args :: [term()], Opts :: apply_opts()) -> ok.

apply(Handle, ServiceId, Function, Args, Opts) when Handle /= undefined ->
    couch_epi_functions_gen:apply(Handle, ServiceId, Function, Args, Opts).

-spec get_handle({ServiceId :: service_id(), Key :: key()}) -> handle();
                (ServiceId :: service_id()) -> handle().

get_handle({_ServiceId, _Key} = EPIKey) ->
    couch_epi_data_gen:get_handle(EPIKey);
get_handle(ServiceId) when is_atom(ServiceId) ->
    couch_epi_functions_gen:get_handle(ServiceId).

-spec any(Handle :: handle(), ServiceId :: atom(), Function :: atom(),
    Args :: [term()], Opts :: apply_opts()) -> boolean().

any(Handle, ServiceId, Function, Args, Opts) when Handle /= undefined ->
    Replies = apply(Handle, ServiceId, Function, Args, Opts),
    [] /= [Reply || Reply <- Replies, Reply == true].

-spec all(Handle :: handle(), ServiceId :: atom(), Function :: atom(),
    Args :: [term()], Opts :: apply_opts()) -> boolean().

all(Handle, ServiceId, Function, Args, Opts) when Handle /= undefined ->
    Replies = apply(Handle, ServiceId, Function, Args, Opts),
    [] == [Reply || Reply <- Replies, Reply == false].

-spec is_configured(
    Handle :: handle(), Function :: atom(), Arity :: pos_integer()) -> boolean().

is_configured(Handle, Function, Arity) when Handle /= undefined ->
    [] /= couch_epi_functions_gen:modules(Handle, Function, Arity).


-spec register_service(
    PluginId :: plugin_id(), Children :: [supervisor:child_spec()]) ->
        [supervisor:child_spec()].

register_service(Plugin, Children) ->
    couch_epi_sup:plugin_childspecs(Plugin, Children).

-spec decide(Handle :: handle(), ServiceId :: atom(), Function :: atom(),
    Args :: [term()], Opts :: apply_opts()) ->
        no_decision | {decided, term()}.

decide(Handle, ServiceId, Function, Args, Opts) when Handle /= undefined ->
    couch_epi_functions_gen:decide(Handle, ServiceId, Function, Args, Opts).
