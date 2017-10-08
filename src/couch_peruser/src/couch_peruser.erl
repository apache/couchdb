% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
% http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(couch_peruser).
-behaviour(gen_server).
-behaviour(mem3_cluster).

-include_lib("couch/include/couch_db.hrl").
-include_lib("mem3/include/mem3.hrl").

% gen_server callbacks
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

% cluster state notification callback
-export([notify_cluster_event/2]).

-export([init_changes_handler/1, changes_handler/3]).

% mem3_cluster callbacks
-export([
    cluster_stable/1,
    cluster_unstable/1
]).

-record(state, {
    parent,
    db_name,
    delete_dbs,
    changes_pid,
    changes_ref
}).

-record(clusterState, {
    parent,
    db_name,
    delete_dbs,
    states,
    mem3_cluster_pid,
    cluster_stable
}).

-define(USERDB_PREFIX, "userdb-").
-define(RELISTEN_DELAY, 5000).
-define(DEFAULT_QUIET_PERIOD, 60). % seconds
-define(DEFAULT_START_PERIOD, 5). % seconds

%%
%% Please leave in the commented-out couch_log:debug calls, thanks! — Jan
%%

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init() ->
    couch_log:debug("peruser: starting on node ~p in pid ~p", [node(), self()]),
    case config:get_boolean("couch_peruser", "enable", false) of
    false ->
        couch_log:debug("peruser: disabled on node ~p", [node()]),
        #clusterState{};
    true ->
        couch_log:debug("peruser: enabled on node ~p", [node()]),
        DbName = ?l2b(config:get(
                         "couch_httpd_auth", "authentication_db", "_users")),
        DeleteDbs = config:get_boolean("couch_peruser", "delete_dbs", false),

        % set up cluster-stable listener
        Period = abs(config:get_integer("couch_peruser", "cluster_quiet_period",
            ?DEFAULT_QUIET_PERIOD)),
        StartPeriod = abs(config:get_integer("couch_peruser", "cluster_start_period",
            ?DEFAULT_START_PERIOD)),

        {ok, Mem3Cluster} = mem3_cluster:start_link(?MODULE, self(), StartPeriod,
            Period),

        #clusterState{
            parent = self(),
            db_name = DbName,
            delete_dbs = DeleteDbs,
            mem3_cluster_pid = Mem3Cluster,
            cluster_stable = false
        }
    end.

% Cluster membership change notification callback
-spec notify_cluster_event(pid(), {cluster, any()}) -> ok.
notify_cluster_event(Server, {cluster, _} = Event) ->
    % couch_log:debug("peruser: received cluster event ~p on node ~p", [Event, node()]),
    gen_server:cast(Server, Event).

start_listening(#clusterState{states=States}=ClusterState) when length(States) > 0 ->
    % couch_log:debug("peruser: start_listening() already run on node ~p in pid ~p", [node(), self()]),
    ClusterState;
start_listening(#clusterState{db_name=DbName, delete_dbs=DeleteDbs} = ClusterState) ->
    % couch_log:debug("peruser: start_listening() on node ~p", [node()]),
    try
        States = lists:map(fun (A) ->
            S = #state{parent = ClusterState#clusterState.parent,
                       db_name = A#shard.name,
                       delete_dbs = DeleteDbs},
            {Pid, Ref} = spawn_opt(
                ?MODULE, init_changes_handler, [S], [link, monitor]),
            S#state{changes_pid=Pid, changes_ref=Ref}
        end, mem3:local_shards(DbName)),
        % couch_log:debug("peruser: start_listening() States ~p", [States]),

        ClusterState#clusterState{states = States, cluster_stable = true}
    catch error:database_does_not_exist ->
        couch_log:warning("couch_peruser can't proceed as underlying database (~s) is missing, disables itself.", [DbName]),
        config:set("couch_peruser", "enable", "false", lists:concat([binary_to_list(DbName), " is missing"]))
    end.

init_changes_handler(#state{db_name=DbName} = State) ->
    % couch_log:debug("peruser: init_changes_handler() on DbName ~p", [DbName]),
    try
        {ok, Db} = couch_db:open_int(DbName, [?ADMIN_CTX, sys_db]),
        FunAcc = {fun ?MODULE:changes_handler/3, State},
        (couch_changes:handle_db_changes(
             #changes_args{feed="continuous", timeout=infinity},
             {json_req, null},
             Db))(FunAcc)
    catch error:database_does_not_exist ->
        ok
    end.


changes_handler({change, {Doc}, _Prepend}, _ResType, State=#state{db_name=DbName}) ->
    % couch_log:debug("peruser: changes_handler() on DbName/Doc ~p/~p", [DbName, Doc]),

    case couch_util:get_value(<<"id">>, Doc) of
    <<"org.couchdb.user:",User/binary>>=DocId ->
        case should_handle_doc(DbName, DocId) of
        true ->
            case couch_util:get_value(<<"deleted">>, Doc, false) of
            false ->
                UserDb = ensure_user_db(User),
                ok = ensure_security(User, UserDb, fun add_user/3),
                State;
            true ->
                case State#state.delete_dbs of
                true ->
                    _UserDb = delete_user_db(User),
                    State;
                false ->
                    UserDb = user_db_name(User),
                    ok = ensure_security(User, UserDb, fun remove_user/3),
                    State
                end
            end;
        false ->
            State
        end;
    _ ->
        State
    end;
changes_handler(_Event, _ResType, State) ->
    State.


should_handle_doc(ShardName, DocId) ->
    case is_stable() of
    false ->
        % when the cluster is unstable, we have already stopped all Listeners
        % the next stable event will restart all listeners and pick up this
        % doc change
        couch_log:debug("peruser: skipping, cluster unstable ~s/~s", [ShardName, DocId]),
        false;
    true ->
        should_handle_doc_int(ShardName, DocId)
    end.

should_handle_doc_int(ShardName, DocId) ->
    DbName = mem3:dbname(ShardName),
    Live = [erlang:node() | erlang:nodes()],
    Shards = mem3:shards(DbName, DocId),
    Nodes = [N || #shard{node=N} <- Shards, lists:member(N, Live)],
    case mem3:owner(DbName, DocId, Nodes) of
    ThisNode when ThisNode =:= node() ->
        couch_log:debug("peruser: handling ~s/~s", [DbName, DocId]),
        true; % do the database action
    _OtherNode ->
        couch_log:debug("peruser: skipping ~s/~s", [DbName, DocId]),
        false
  end.


delete_user_db(User) ->
    UserDb = user_db_name(User),
    try
        case fabric:delete_db(UserDb, [?ADMIN_CTX]) of
        ok -> ok;
        accepted -> ok
        end
    catch error:database_does_not_exist ->
        ok
    end,
    UserDb.

ensure_user_db(User) ->
    UserDb = user_db_name(User),
    try
        {ok, _DbInfo} = fabric:get_db_info(UserDb)
    catch error:database_does_not_exist ->
        case fabric:create_db(UserDb, [?ADMIN_CTX]) of
        {error, file_exists} -> ok;
        ok -> ok;
        accepted -> ok
        end
    end,
    UserDb.

add_user(User, Prop, {Modified, SecProps}) ->
    {PropValue} = couch_util:get_value(Prop, SecProps, {[]}),
    Names = couch_util:get_value(<<"names">>, PropValue, []),
    case lists:member(User, Names) of
    true ->
        {Modified, SecProps};
    false ->
        {true,
         lists:keystore(
             Prop, 1, SecProps,
             {Prop,
              {lists:keystore(
                   <<"names">>, 1, PropValue,
                   {<<"names">>, [User | Names]})}})}
    end.

remove_user(User, Prop, {Modified, SecProps}) ->
    {PropValue} = couch_util:get_value(Prop, SecProps, {[]}),
    Names = couch_util:get_value(<<"names">>, PropValue, []),
    case lists:member(User, Names) of
    false ->
        {Modified, SecProps};
    true ->
        {true,
         lists:keystore(
             Prop, 1, SecProps,
             {Prop,
              {lists:keystore(
                   <<"names">>, 1, PropValue,
                   {<<"names">>, lists:delete(User, Names)})}})}
    end.

ensure_security(User, UserDb, TransformFun) ->
    case fabric:get_all_security(UserDb, [?ADMIN_CTX]) of
    {error, no_majority} ->
       % single node, ignore
       ok;
    {ok, Shards} ->
        {_ShardInfo, {SecProps}} = hd(Shards),
        % assert that shards have the same security object
        true = lists:all(fun ({_, {SecProps1}}) ->
            SecProps =:= SecProps1
        end, Shards),
        case lists:foldl(
               fun (Prop, SAcc) -> TransformFun(User, Prop, SAcc) end,
               {false, SecProps},
               [<<"admins">>, <<"members">>]) of
        {false, _} ->
            ok;
        {true, SecProps1} ->
            ok = fabric:set_security(UserDb, {SecProps1}, [?ADMIN_CTX])
        end
    end.

user_db_name(User) ->
    HexUser = list_to_binary(
        [string:to_lower(integer_to_list(X, 16)) || <<X>> <= User]),
    <<?USERDB_PREFIX,HexUser/binary>>.

exit_changes(ClusterState) ->
    lists:foreach(fun (State) ->
        demonitor(State#state.changes_ref, [flush]),
        exit(State#state.changes_pid, kill)
    end, ClusterState#clusterState.states).

-spec is_stable() -> true | false.
is_stable() ->
    gen_server:call(?MODULE, is_stable).

% Mem3 cluster callbacks

cluster_unstable(Server) ->
    gen_server:cast(Server, cluster_unstable),
    Server.

cluster_stable(Server) ->
    gen_server:cast(Server, cluster_stable),
    Server.

%% gen_server callbacks

init([]) ->
    ok = subscribe_for_changes(),
    {ok, init()}.

handle_call(is_stable, _From, #clusterState{cluster_stable = IsStable} = State) ->
    {reply, IsStable, State};
handle_call(_Msg, _From, State) ->
    {reply, error, State}.


handle_cast(update_config, ClusterState) when ClusterState#clusterState.states =/= undefined ->
    exit_changes(ClusterState),
    {noreply, init()};
handle_cast(update_config, _) ->
    {noreply, init()};
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(cluster_unstable, ClusterState) when ClusterState#clusterState.states =/= undefined ->
    exit_changes(ClusterState),
    {noreply, init()};
handle_cast(cluster_unstable, _) ->
    {noreply, init()};
handle_cast(cluster_stable, State) ->
    {noreply, start_listening(State)};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', Ref, _, _, _Reason}, #state{changes_ref=Ref} = State) ->
    {stop, normal, State};
handle_info({config_change, "couch_peruser", _, _, _}, State) ->
    handle_cast(update_config, State);
handle_info({config_change, "couch_httpd_auth", "authentication_db", _, _}, State) ->
    handle_cast(update_config, State);
handle_info({gen_event_EXIT, _Handler, _Reason}, State) ->
    erlang:send_after(?RELISTEN_DELAY, self(), restart_config_listener),
    {noreply, State};
handle_info({'EXIT', _Pid, _Reason}, State) ->
    erlang:send_after(?RELISTEN_DELAY, self(), restart_config_listener),
    {noreply, State};
handle_info(restart_config_listener, State) ->
    ok = subscribe_for_changes(),
    {noreply, State};
handle_info(_Msg, State) ->
    {noreply, State}.

subscribe_for_changes() ->
    config:subscribe_for_changes([
        {"couch_httpd_auth", "authentication_db"},
        "couch_peruser"
    ]).


terminate(_Reason, _State) ->
    %% Everything should be linked or monitored, let nature
    %% take its course.
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
