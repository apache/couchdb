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

-module(fabric_db_create).
-export([go/2]).

-include_lib("fabric/include/fabric.hrl").
-include_lib("mem3/include/mem3.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(DBNAME_REGEX, "^[a-z][a-z0-9\\_\\$()\\+\\-\\/]*$").

%% @doc Create a new database, and all its partition files across the cluster
%%      Options is proplist with user_ctx, n, q, validate_name
go(DbName, Options) ->
    case validate_dbname(DbName, Options) of
    ok ->
        {Shards, Doc} = generate_shard_map(DbName, Options),
        case {create_shard_files(Shards), create_shard_db_doc(Doc)} of
        {ok, {ok, Status}} ->
            Status;
        {file_exists, {ok, _}} ->
            {error, file_exists};
        {_, Error} ->
            Error
        end;
    Error ->
        Error
    end.

validate_dbname(DbName, Options) ->
    case couch_util:get_value(validate_name, Options, true) of
    false ->
        ok;
    true ->
        case re:run(DbName, ?DBNAME_REGEX, [{capture,none}, dollar_endonly]) of
        match ->
            ok;
        nomatch when DbName =:= <<"_users">> ->
            ok;
        nomatch when DbName =:= <<"_replicator">> ->
            ok;
        nomatch ->
            {error, illegal_database_name}
        end
    end.

generate_shard_map(DbName, Options) ->
    {MegaSecs, Secs, _} = now(),
    Suffix = "." ++ integer_to_list(MegaSecs*1000000 + Secs),
    Shards = mem3:choose_shards(DbName, [{shard_suffix,Suffix} | Options]),
    case mem3_util:open_db_doc(DbName) of
    {ok, Doc} ->
        % the DB already exists, and may have a different Suffix
        ok;
    {not_found, _} ->
        Doc = make_document(Shards, Suffix)
    end,
    {Shards, Doc}.

create_shard_files(Shards) ->
    Workers = fabric_util:submit_jobs(Shards, create_db, []),
    RexiMon = fabric_util:create_monitors(Shards),
    try fabric_util:recv(Workers, #shard.ref, fun handle_message/3, Workers) of
    {error, file_exists} ->
        file_exists;
    _ ->
        ok
    after
        rexi_monitor:stop(RexiMon)
    end.

handle_message(file_exists, _, _) ->
    {error, file_exists};

handle_message({rexi_DOWN, _, {_, Node}, _}, _, Workers) ->
    case lists:filter(fun(S) -> S#shard.node =/= Node end, Workers) of
    [] ->
        {stop, ok};
    RemainingWorkers ->
        {ok, RemainingWorkers}
    end;

handle_message(_, Worker, Workers) ->
    case lists:delete(Worker, Workers) of
    [] ->
        {stop, ok};
    RemainingWorkers ->
        {ok, RemainingWorkers}
    end.

create_shard_db_doc(Doc) ->
    Shards = [#shard{node=N} || N <- mem3:nodes()],
    RexiMon = fabric_util:create_monitors(Shards),
    Workers = fabric_util:submit_jobs(Shards, create_shard_db_doc, [Doc]),
    Acc0 = {length(Shards), fabric_dict:init(Workers, nil)},
    try fabric_util:recv(Workers, #shard.ref, fun handle_db_update/3, Acc0) of
    {timeout, _} ->
        {error, timeout};
    Else ->
        Else
    after
        rexi_monitor:stop(RexiMon)
    end.

handle_db_update({rexi_DOWN, _, {_, Node}, _}, _Worker, {W, Counters}) ->
    New = fabric_dict:filter(fun(S, _) -> S#shard.node =/= Node end, Counters),
    maybe_stop(W, New);

handle_db_update({rexi_EXIT, _Reason}, Worker, {W, Counters}) ->
    maybe_stop(W, fabric_dict:erase(Worker, Counters));

handle_db_update(conflict, _, _) ->
    % just fail when we get any conflicts
    {error, conflict};

handle_db_update(Msg, Worker, {W, Counters}) ->
    maybe_stop(W, fabric_dict:store(Worker, Msg, Counters)).

maybe_stop(W, Counters) ->
    case fabric_dict:any(nil, Counters) of
    true ->
        {ok, {W, Counters}};
    false ->
        case lists:sum([1 || {_, ok} <- Counters]) of
        W ->
            {stop, ok};
        NumOk when NumOk >= (W div 2 + 1) ->
            {stop, accepted};
        _ ->
            {error, internal_server_error}
        end
    end.

make_document([#shard{dbname=DbName}|_] = Shards, Suffix) ->
    {RawOut, ByNodeOut, ByRangeOut} =
    lists:foldl(fun(#shard{node=N, range=[B,E]}, {Raw, ByNode, ByRange}) ->
        Range = ?l2b([couch_util:to_hex(<<B:32/integer>>), "-",
            couch_util:to_hex(<<E:32/integer>>)]),
        Node = couch_util:to_binary(N),
        {[[<<"add">>, Range, Node] | Raw], orddict:append(Node, Range, ByNode),
            orddict:append(Range, Node, ByRange)}
    end, {[], [], []}, Shards),
    #doc{id=DbName, body = {[
        {<<"shard_suffix">>, Suffix},
        {<<"changelog">>, lists:sort(RawOut)},
        {<<"by_node">>, {[{K,lists:sort(V)} || {K,V} <- ByNodeOut]}},
        {<<"by_range">>, {[{K,lists:sort(V)} || {K,V} <- ByRangeOut]}}
    ]}}.

