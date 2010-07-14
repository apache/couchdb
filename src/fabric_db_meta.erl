-module(fabric_db_meta).

-export([set_revs_limit/3, set_security/3]).

-include("fabric.hrl").
-include_lib("mem3/include/mem3.hrl").

set_revs_limit(DbName, Limit, Options) ->
    Shards = mem3:shards(DbName),
    Workers = fabric_util:submit_jobs(Shards, set_revs_limit, [Limit, Options]),
    Waiting = length(Workers) - 1,
    case fabric_util:recv(Workers, #shard.ref, fun handle_message/3, Waiting) of
    {ok, ok} ->
        ok;
    Error ->
        Error
    end.

set_security(DbName, SecObj, Options) ->
    Shards = mem3:shards(DbName),
    Workers = fabric_util:submit_jobs(Shards, set_security, [SecObj, Options]),
    Waiting = length(Workers) - 1,
    fabric_util:recv(Workers, #shard.ref, fun handle_message/3, Waiting).

handle_message(ok, _, 0) ->
    {stop, ok};
handle_message(ok, _, Waiting) ->
    {ok, Waiting - 1};
handle_message(Error, _, _Waiting) ->
    {error, Error}.