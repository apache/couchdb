-module(mem3_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, []) ->
    DbName = couch_config:get("mem3", "db", "dbs"),
    couch_server:create(list_to_binary(DbName), []),
    mem3_sup:start_link().

stop([]) ->
    ok.
