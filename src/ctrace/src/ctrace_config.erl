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

-module(ctrace_config).

-vsn(1).

-behaviour(config_listener).

-export([
    is_enabled/0,
    update/0,

    filter_module_name/1
]).

-export([
    handle_config_change/5,
    handle_config_terminate/3
]).

-include("ctrace.hrl").


-spec is_enabled() -> boolean().
is_enabled() ->
    config:get_boolean("tracing", "enabled", false).


-spec update() -> ok.
update() ->
    case is_enabled() of
        true ->
            maybe_start_main_tracer(?MAIN_TRACER),

            CompiledFilters = get_compiled_filters(),

            RemovedFilters = lists:foldl(fun({OperationId, FilterDef}, Acc) ->
                case compile_filter(OperationId, FilterDef) of
                    true -> Acc -- [OperationId];
                    false -> Acc
                end
            end, CompiledFilters, config:get("tracing.filters")),

            lists:foreach(fun(OperationId) ->
                ModName = filter_module_name(OperationId),
                code:delete(ModName),
                code:purge(ModName)
            end, RemovedFilters),

            case config:get("tracing.filters", "all") of
                undefined -> compile_filter("all", "(#{}) -> false");
                _ -> ok
            end;

        false ->
            jaeger_passage:stop_tracer(?MAIN_TRACER)
    end,
    ok.


-spec filter_module_name(atom() | string()) -> atom().
filter_module_name(OperationId) when is_atom(OperationId) ->
    filter_module_name(atom_to_list(OperationId));
filter_module_name(OperationId) ->
    list_to_atom("ctrace_filter_" ++ OperationId).


handle_config_change("tracing", "enabled", _, _Persist, St) ->
    update(),
    {ok, St};
handle_config_change("tracing.filters", _Key, _Val, _Persist, St) ->
    update(),
    {ok, St};
handle_config_change(_Sec, _Key, _Val, _Persist, St) ->
    {ok, St}.

handle_config_terminate(_Server, _Reason, _State) ->
    update().


maybe_start_main_tracer(TracerId) ->
    case passage_tracer_registry:get_reporter(TracerId) of
        error ->
            start_main_tracer(TracerId);
        _ ->
            true
    end.


start_main_tracer(TracerId) ->
    Sampler = passage_sampler_all:new(),
    Options = [
        {thrift_format,
            list_to_atom(config:get("tracing", "thrift_format", "compact"))},
        {agent_host, config:get("tracing", "agent_host", "127.0.0.1")},
        {agent_port, config:get_integer("tracing", "agent_port", 6831)},
        {default_service_name,
            list_to_atom(config:get("tracing", "app_name", "couchdb"))}
    ],
    ok = jaeger_passage:start_tracer(TracerId, Sampler, Options).


compile_filter(OperationId, FilterDef) ->
    try
        couch_log:info("Compiling filter : ~s", [OperationId]),
        ctrace_dsl:compile(OperationId, FilterDef),
        true
    catch throw:{error, Reason} ->
        couch_log:error("Cannot compile ~s :: ~s~n", [OperationId, Reason]),
        false
    end.


get_compiled_filters() ->
    lists:foldl(fun({Mod, _Path}, Acc) ->
        ModStr = atom_to_list(Mod),
        case ModStr of
            "ctrace_filter_" ++ OpName ->
                [OpName | Acc];
            _ ->
                Acc
        end
    end, [], code:all_loaded()).
