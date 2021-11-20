-module(dreyfus_epi).

-behaviour(couch_epi_plugin).

-export([
    app/0,
    providers/0,
    services/0,
    data_subscriptions/0,
    data_providers/0,
    processes/0,
    notify/3
]).

-define(DATA_INTERVAL, 1000).

app() ->
    dreyfus.

providers() ->
    [
        {couch_db, dreyfus_plugin_couch_db},
        {chttpd_handlers, dreyfus_httpd_handlers}
    ].

services() ->
    [].

data_subscriptions() ->
    [{dreyfus, black_list}].

data_providers() ->
    [
        {{dreyfus, black_list}, {callback_module, dreyfus_config}, [{interval, ?DATA_INTERVAL}]}
    ].

processes() ->
    [].

notify(_Key, _Old, _New) ->
    Listeners = application:get_env(dreyfus, config_listeners, []),
    lists:foreach(
        fun(L) ->
            L ! dreyfus_config_change_finished
        end,
        Listeners
    ).
