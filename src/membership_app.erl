-module(membership_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, []) ->
    DbName = couch_config:get("membership", "db", "dbs"),
    couch_server:create(list_to_binary(DbName), []),
    membership_sup:start_link().

stop([]) ->
    ok.
