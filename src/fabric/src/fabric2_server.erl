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

-module(fabric2_server).
-behaviour(gen_server).
-vsn(1).


-export([
    start_link/0,
    fetch/1,
    store/1,
    remove/1,
    fdb_directory/0,
    fdb_cluster/0
]).


-export([
    init/1,
    terminate/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3
]).


-include_lib("couch/include/couch_db.hrl").


-define(CLUSTER_FILE, "/usr/local/etc/foundationdb/fdb.cluster").
-define(FDB_DIRECTORY, fdb_directory).
-define(FDB_CLUSTER, fdb_cluster).
-define(DEFAULT_FDB_DIRECTORY, <<"couchdb">>).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


fetch(DbName) when is_binary(DbName) ->
    case ets:lookup(?MODULE, DbName) of
        [{DbName, #{} = Db}] -> Db;
        [] -> undefined
    end.


store(#{name := DbName} = Db0) when is_binary(DbName) ->
    Db1 = Db0#{
        tx := undefined,
        user_ctx := #user_ctx{}
    },
    true = ets:insert(?MODULE, {DbName, Db1}),
    ok.


remove(DbName) when is_binary(DbName) ->
    true = ets:delete(?MODULE, DbName),
    ok.


init(_) ->
    ets:new(?MODULE, [
            public,
            named_table,
            {read_concurrency, true},
            {write_concurrency, true}
        ]),

    {Cluster, Db} = case application:get_env(fabric, eunit_run) of
        {ok, true} ->
            {<<"eunit_test">>, erlfdb_util:get_test_db([empty])};
        undefined ->
            ClusterFileStr = config:get("erlfdb", "cluster_file", ?CLUSTER_FILE),
            {ok, ConnectionStr} = file:read_file(ClusterFileStr),
            DbHandle = erlfdb:open(iolist_to_binary(ClusterFileStr)),
            {string:trim(ConnectionStr), DbHandle}
    end,
    application:set_env(fabric, ?FDB_CLUSTER, Cluster),
    application:set_env(fabric, db, Db),

    Dir = case config:get("fabric", "fdb_directory") of
        Val when is_list(Val), length(Val) > 0 ->
            [?l2b(Val)];
        _ ->
            [?DEFAULT_FDB_DIRECTORY]
    end,
    application:set_env(fabric, ?FDB_DIRECTORY, Dir),

    {ok, nil}.


terminate(_, _St) ->
    ok.


handle_call(Msg, _From, St) ->
    {stop, {bad_call, Msg}, {bad_call, Msg}, St}.


handle_cast(Msg, St) ->
    {stop, {bad_cast, Msg}, St}.


handle_info(Msg, St) ->
    {stop, {bad_info, Msg}, St}.


code_change(_OldVsn, St, _Extra) ->
    {ok, St}.


fdb_directory() ->
    case get(?FDB_DIRECTORY) of
        undefined ->
            case application:get_env(fabric, ?FDB_DIRECTORY) of
                undefined ->
                    erlang:error(fabric_application_not_started);
                {ok, Dir} ->
                    put(?FDB_DIRECTORY, Dir),
                    Dir
            end;
        Dir ->
            Dir
    end.

fdb_cluster() ->
    case get(?FDB_CLUSTER) of
        undefined ->
            case application:get_env(fabric, ?FDB_CLUSTER) of
                undefined ->
                    erlang:error(fabric_application_not_started);
                {ok, Cluster} ->
                    put(?FDB_CLUSTER, Cluster),
                    Cluster
            end;
        Cluster ->
            Cluster
    end.
