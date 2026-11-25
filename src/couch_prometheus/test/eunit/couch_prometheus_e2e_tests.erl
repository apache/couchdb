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

-module(couch_prometheus_e2e_tests).

-behaviour(couch_prometheus_plugin).

% couch_prometheus_plugin callback
-export([
    metrics/0
]).

-include_lib("couch/include/couch_eunit.hrl").

-define(USER, "prometheus_test_admin").
-define(PASS, "pass").
-define(AUTH, {basic_auth, {?USER, ?PASS}}).
-define(PROM_PORT, "17986").
-define(CONTENT_JSON, {"Content-Type", "application/json"}).

e2e_test_() ->
    {
        "With dedicated port",
        {
            setup,
            fun() ->
                setup_prometheus(true)
            end,
            fun(Ctx) ->
                test_util:stop_couch(Ctx)
            end,
            {
                foreach,
                fun() ->
                    mochiweb_socket_server:get(chttpd, port)
                end,
                [
                    ?TDEF_FE(t_chttpd_port),
                    ?TDEF_FE(t_prometheus_port),
                    ?TDEF_FE(t_metric_updated),
                    ?TDEF_FE(t_no_duplicate_metrics),
                    ?TDEF_FE(t_starts_with_couchdb),
                    ?TDEF_FE(t_survives_mem3_sync_termination),
                    ?TDEF_FE(t_system_stats_metrics_present),
                    ?TDEF_FE(t_plugin_metrics),
                    ?TDEF_FE(t_plugin_disabled),
                    ?TDEF_FE(t_plugin_failures_are_skipped)
                ]
            }
        }
    }.

reject_test_() ->
    {
        "Without dedicated port",
        {
            setup,
            fun() ->
                setup_prometheus(false)
            end,
            fun(Ctx) ->
                test_util:stop_couch(Ctx)
            end,
            {
                foreach,
                fun() ->
                    ?PROM_PORT
                end,
                [
                    ?TDEF_FE(t_reject_prometheus_port)
                ]
            }
        }
    }.

setup_prometheus(WithAdditionalPort) ->
    Ctx = test_util:start_couch([mem3, chttpd, couch_prometheus]),
    Persist = false,
    Hashed = couch_passwords:hash_admin_password(?PASS),
    ok = config:set("admins", ?USER, binary_to_list(Hashed), Persist),
    ok = config:set_integer("stats", "interval", 1, Persist),
    ok = config:set_integer("prometheus", "interval", 1, Persist),
    ok = config:set_boolean(
        "prometheus",
        "additional_port",
        WithAdditionalPort,
        Persist
    ),
    ok = config:set("prometheus", "port", ?PROM_PORT, Persist),
    ok = config:set("prometheus_plugins", ?MODULE_STRING, "true", Persist),
    % It's already started by default, so restart to pick up config
    ok = application:stop(couch_prometheus),
    ok = application:start(couch_prometheus),
    Ctx.

t_chttpd_port(Port) ->
    {ok, RC, _, _} = test_request:get(node_local_url(Port), [?CONTENT_JSON, ?AUTH]),
    ?assertEqual(200, RC).

t_prometheus_port(_) ->
    Url = node_local_url(?PROM_PORT),
    {ok, RC1, _, _} = test_request:get(Url, [?CONTENT_JSON, ?AUTH]),
    ?assertEqual(200, RC1),
    % Since this port doesn't require auth, this should work
    {ok, RC2, _, _} = test_request:get(Url, [?CONTENT_JSON]),
    ?assertEqual(200, RC2).

t_reject_prometheus_port(Port) ->
    Response = test_request:get(node_local_url(Port), [?CONTENT_JSON, ?AUTH]),
    ?assertEqual({error, econnrefused}, Response).

t_no_duplicate_metrics(Port) ->
    Url = node_local_url(Port),
    Stats = get_stats(Url),
    Lines = re:split(Stats, "\n"),
    % Filter the result to only the lines containing the metric
    % definition, not the values. These lines always start with
    % a # character.
    MetricDefs = lists:filter(fun(S) -> string:find(S, "#") =:= S end, Lines),
    ?assertNotEqual(length(MetricDefs), 0),
    Diff = get_duplicates(MetricDefs),
    ?assertEqual(length(Diff), 0).

get_duplicates(List) ->
    List -- sets:to_list(couch_util:set_from_list(List)).

t_metric_updated(Port) ->
    % The passage of time should increment this metric
    Metric = "couchdb_uptime_seconds",
    Url = node_local_url(Port),
    % We may need to wait until the metric appears
    InitialValue = test_util:wait(
        fun() ->
            Stats = get_stats(Url),
            case metric_value(Stats, Metric) of
                not_found -> wait;
                Val -> Val
            end
        end
    ),
    test_util:wait(
        fun() ->
            NewValue = metric_value(get_stats(Url), Metric),
            case NewValue > InitialValue of
                true -> ok;
                false -> wait
            end
        end
    ).

t_starts_with_couchdb(Port) ->
    Url = node_local_url(Port),
    Stats = get_stats(Url),
    Lines = re:split(Stats, "\n"),
    lists:foreach(
        fun(Line) ->
            ?assert(is_binary(Line)),
            Trimmed = string:trim(Line),
            Expect = "^(#|couchdb_|$)",
            case re:run(Trimmed, Expect) of
                {match, _} ->
                    ok;
                nomatch ->
                    error(
                        {assertRegexp_failed, [
                            {module, ?MODULE},
                            {line, ?LINE},
                            {regexp, (Trimmed)},
                            {expected_to_match, Expect},
                            {result, nomatch}
                        ]}
                    )
            end
        end,
        Lines
    ).

t_survives_mem3_sync_termination(_) ->
    ?assertNotEqual(undefined, whereis(mem3_sync)),
    ok = supervisor:terminate_child(mem3_sup, mem3_sync),
    ?assertEqual(undefined, whereis(mem3_sync)),
    ?assertMatch(
        [_, _, <<"couchdb_internal_replication_jobs 0">>],
        couch_prometheus:get_internal_replication_jobs_stat()
    ),
    {ok, _} = supervisor:restart_child(mem3_sup, mem3_sync),
    ?assertMatch(
        [_, _, <<"couchdb_internal_replication_jobs", _/binary>>],
        couch_prometheus:get_internal_replication_jobs_stat()
    ).

t_system_stats_metrics_present(Port) ->
    Url = node_local_url(Port),
    Stats = get_stats(Url),
    % Verify system stats metrics exist in the scrape output
    ?assertNotEqual(not_found, metric_value(Stats, "couchdb_clouseau_connected")),
    ?assertNotEqual(not_found, metric_value(Stats, "couchdb_database_count")),
    ?assertNotEqual(not_found, metric_value(Stats, "couchdb_ioq_total_requests")),
    ?assertNotEqual(not_found, metric_value(Stats, "couchdb_smoosh_channel_count")).

t_plugin_metrics(Port) ->
    lists:foreach(
        fun(P) ->
            Stats = get_stats(node_local_url(P)),
            ?assertEqual(42, metric_value(Stats, "couchdb_test_plugin_metric")),
            ?assertNotEqual(nomatch, string:find(Stats, "# TYPE couchdb_test_plugin_metric gauge"))
        end,
        [Port, ?PROM_PORT]
    ).

t_plugin_disabled(Port) ->
    Url = node_local_url(Port),
    try
        ok = config:set("prometheus_plugins", ?MODULE_STRING, "false", false),
        ?assertEqual(not_found, metric_value(get_stats(Url), "couchdb_test_plugin_metric"))
    after
        ok = config:set("prometheus_plugins", ?MODULE_STRING, "true", false)
    end,
    ?assertEqual(42, metric_value(get_stats(Url), "couchdb_test_plugin_metric")).

t_plugin_failures_are_skipped(Port) ->
    Bad = [
        {couch_prometheus_missing_plugin, undefined},
        {couch_prometheus_crash_plugin, fun() -> error(potatoes) end},
        {couch_prometheus_junk_plugin, fun() -> ["a string", {badness}] end}
    ],
    Mods = [Mod || {Mod, Fun} <- Bad, is_function(Fun)],
    try
        lists:foreach(
            fun
                ({Mod, undefined}) ->
                    ok = config:set("prometheus_plugins", atom_to_list(Mod), "true", false);
                ({Mod, Fun}) ->
                    meck:new(Mod, [non_strict]),
                    meck:expect(Mod, metrics, Fun),
                    ok = config:set("prometheus_plugins", atom_to_list(Mod), "true", false)
            end,
            Bad
        ),
        {ok, RC, _, Stats} = test_request:get(node_local_url(Port), [?CONTENT_JSON, ?AUTH]),
        ?assertEqual(200, RC),
        % We did call the bad ones
        [?assert(meck:called(Mod, metrics, [])) || Mod <- Mods],
        % Core metrics are there we didn't break all the things (tm)
        ?assertNotEqual(not_found, metric_value(Stats, "couchdb_uptime_seconds")),
        ?assertEqual(42, metric_value(Stats, "couchdb_test_plugin_metric"))
    after
        [config:delete("prometheus_plugins", atom_to_list(Mod), false) || {Mod, _} <- Bad],
        meck:unload(Mods)
    end.

metrics() ->
    Instance = {[{source, ~"plugin"}], 42},
    couch_prometheus_util:to_prom(test_plugin_metric, gauge, "test plugin metric", Instance).

node_local_url(Port) ->
    Addr = config:get("chttpd", "bind_address", "127.0.0.1"),
    lists:concat(["http://", Addr, ":", Port, "/_node/_local/_prometheus"]).

get_stats(Url) ->
    {ok, _, _, Body} = test_request:get(Url, [?CONTENT_JSON, ?AUTH]),
    Body.

metric_value(StatsBin, Metric) ->
    % Prefix metric with newline to avoid matching lines starting with "# TYPE"
    case string:find(StatsBin, "\n" ++ Metric, trailing) of
        nomatch ->
            not_found;
        Leading ->
            Trimmed = string:trim(Leading, leading),
            [Line, _Rest] = string:split(Trimmed, "\n"),
            [_MetricBin, Value] = string:split(Line, " "),
            binary_to_integer(Value)
    end.
