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

-module(fabric_server).
-behaviour(gen_server).
-vsn(1).


-export([
    start_link/0,
    transactional/1,
    get_dir/1,

    debug_cluster/0
]).


-export([
    init/1,
    terminate/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3
]).


-record(st, {
    db
}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).



transactional(Fun) when is_function(Fun, 1) ->
    [{'$handle$', Db}] = ets:lookup(?MODULE, '$handle$'),
    erlfdb:transactional(Db, Fun).


get_dir(Name) ->
    case ets:lookup(?MODULE, Name) of
        [{Name, Dir}] -> Dir;
        [] -> not_found
    end.


debug_cluster() ->
    transactional(fun(Tx) ->
        lists:foreach(fun({Key, Val}) ->
            io:format("~s~n => ~s~n~n", [
                    erlfdb_util:repr(Key),
                    erlfdb_util:repr(Val)
                ])
        end, erlfdb:get_range(Tx, <<>>, <<16#FE, 16#FF>>))
    end).


init(_) ->
    ets:new(?MODULE, [
            protected,
            named_table,
            {read_concurrency, true}
        ]),

    ClusterStr = config:get("erlfdb", "cluster_file", "/usr/local/etc/foundationdb/fdb.cluster"),
    Db = erlfdb:open(iolist_to_binary(ClusterStr)),
    Dirs = init_cluster(Db),

    ets:insert(?MODULE, {'$handle$', Db}),
    lists:foreach(fun({K, V}) ->
        ets:insert(?MODULE, {K, V})
    end, Dirs),

    {ok, #st{db = Db}}.


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



init_cluster(Db) ->
    Dirs = erlfdb:transactional(Db, fun(Tx) ->
        Root = erlfdb_directory:root(),
        CouchDB = erlfdb_directory:create_or_open(Tx, Root, [<<"couchdb">>]),
        Dbs = erlfdb_directory:create_or_open(Tx, CouchDB, [<<"dbs">>]),
        Config = erlfdb_directory:create_or_open(Tx, CouchDB, [
                <<"meta">>,
                <<"config">>
            ]),
        [
            {root, Root},
            {config, Config},
            {dbs, Dbs}
        ]
    end),
    drain_ready(Dirs).


drain_ready(Dirs) ->
    receive
        {Ref, ready} when is_reference(Ref) ->
            drain_ready(Dirs)
    after 100 ->
        Dirs
    end.