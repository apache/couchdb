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
    fetch/2,
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
-define(TX_OPTIONS_SECTION, "fdb_tx_options").
-define(RELISTEN_DELAY, 1000).

-define(DEFAULT_TIMEOUT_MSEC, 60000).
-define(DEFAULT_RETRY_LIMIT, 100).

-define(TX_OPTIONS, #{
    machine_id                           => {binary,  undefined},
    datacenter_id                        => {binary,  undefined},
    transaction_logging_max_field_length => {integer, undefined},
    timeout                              => {integer, ?DEFAULT_TIMEOUT_MSEC},
    retry_limit                          => {integer, ?DEFAULT_RETRY_LIMIT},
    max_retry_delay                      => {integer, undefined},
    size_limit                           => {integer, undefined}
}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


fetch(DbName, UUID) when is_binary(DbName) ->
    case {UUID, ets:lookup(?MODULE, DbName)} of
        {_, []} -> undefined;
        {undefined, [{DbName, #{} = Db}]} -> Db;
        {<<_/binary>>, [{DbName, #{uuid := UUID} = Db}]} -> Db;
        {<<_/binary>>, [{DbName, #{} = _Db}]} -> undefined
    end.


store(#{name := DbName} = Db0) when is_binary(DbName) ->
    Db1 = Db0#{
        tx := undefined,
        user_ctx := #user_ctx{},
        security_fun := undefined
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
    {Cluster, Db} = get_db_and_cluster([empty]),
    application:set_env(fabric, ?FDB_CLUSTER, Cluster),
    application:set_env(fabric, db, Db),

    Dir = case config:get("fabric", "fdb_directory") of
        Val when is_list(Val), length(Val) > 0 ->
            [?l2b(Val)];
        _ ->
            [?DEFAULT_FDB_DIRECTORY]
    end,
    application:set_env(fabric, ?FDB_DIRECTORY, Dir),
    config:subscribe_for_changes([?TX_OPTIONS_SECTION]),
    {ok, nil}.


terminate(_, _St) ->
    ok.


handle_call(Msg, _From, St) ->
    {stop, {bad_call, Msg}, {bad_call, Msg}, St}.


handle_cast(Msg, St) ->
    {stop, {bad_cast, Msg}, St}.


handle_info({config_change, ?TX_OPTIONS_SECTION, _K, deleted, _}, St) ->
    % Since we don't know the exact default values to reset the options
    % to we recreate the db handle instead which will start with a default
    % handle and re-apply all the options
    {_Cluster, NewDb} = get_db_and_cluster([]),
    application:set_env(fabric, db, NewDb),
    {noreply, St};

handle_info({config_change, ?TX_OPTIONS_SECTION, K, V, _}, St) ->
    {ok, Db} = application:get_env(fabric, db),
    apply_tx_options(Db, [{K, V}]),
    {noreply, St};

handle_info({gen_event_EXIT, _Handler, _Reason}, St) ->
    erlang:send_after(?RELISTEN_DELAY, self(), restart_config_listener),
    {noreply, St};

handle_info(restart_config_listener, St) ->
    config:subscribe_for_changes([?TX_OPTIONS_SECTION]),
    {noreply, St};

handle_info(Msg, St) ->
    {stop, {bad_info, Msg}, St}.


code_change(_OldVsn, St, _Extra) ->
    {ok, St}.


fdb_directory() ->
    get_env(?FDB_DIRECTORY).

fdb_cluster() ->
    get_env(?FDB_CLUSTER).

get_env(Key) ->
    case get(Key) of
        undefined ->
            case application:get_env(fabric, Key) of
                undefined ->
                    erlang:error(fabric_application_not_started);
                {ok, Value} ->
                    put(Key, Value),
                    Value
            end;
        Value ->
            Value
    end.


get_db_and_cluster(EunitDbOpts) ->
     {Cluster, Db} = case application:get_env(fabric, eunit_run) of
        {ok, true} ->
            {<<"eunit_test">>, erlfdb_util:get_test_db(EunitDbOpts)};
        undefined ->
            ClusterFileStr = config:get("erlfdb", "cluster_file", ?CLUSTER_FILE),
            {ok, ConnectionStr} = file:read_file(ClusterFileStr),
            DbHandle = erlfdb:open(iolist_to_binary(ClusterFileStr)),
            {string:trim(ConnectionStr), DbHandle}
    end,
    apply_tx_options(Db, config:get(?TX_OPTIONS_SECTION)),
    {Cluster, Db}.


apply_tx_options(Db, Cfg) ->
    maps:map(fun(Option, {Type, Default}) ->
        case lists:keyfind(atom_to_list(Option), 1, Cfg) of
            false ->
                case Default of
                    undefined -> ok;
                    _Defined -> apply_tx_option(Db, Option, Default, Type)
                end;
            {_K, Val} ->
                apply_tx_option(Db, Option, Val, Type)
        end
    end, ?TX_OPTIONS).


apply_tx_option(Db, Option, Val, integer) ->
    try
        erlfdb:set_option(Db, Option, list_to_integer(Val))
    catch
        error:badarg ->
            Msg = "~p : Invalid integer tx option ~p = ~p",
            couch_log:error(Msg, [?MODULE, Option, Val])
    end;

apply_tx_option(Db, Option, Val, binary) ->
    BinVal = list_to_binary(Val),
    case size(BinVal) < 16 of
        true ->
            erlfdb:set_option(Db, Option, BinVal);
        false ->
            Msg = "~p : String tx option ~p is larger than 16 bytes",
            couch_log:error(Msg, [?MODULE, Option])
    end.
