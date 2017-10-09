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

-export([init_changes_handler/1, changes_handler/3]).

% mem3_cluster callbacks
-export([
    cluster_stable/1,
    cluster_unstable/1
]).

-record(changes_state, {
    parent :: pid(),
    db_name :: binary(),
    delete_dbs :: boolean(),
    changes_pid :: pid(),
    changes_ref :: reference()
}).

-record(state, {
    parent :: pid(),
    db_name :: binary(),
    delete_dbs :: boolean(),
    states :: list(),
    mem3_cluster_pid :: pid(),
    cluster_stable :: boolean()
}).

-define(USERDB_PREFIX, "userdb-").
-define(RELISTEN_DELAY, 5000).
-define(DEFAULT_QUIET_PERIOD, 60). % seconds
-define(DEFAULT_START_PERIOD, 5). % seconds

%%
%% Please leave in the commented-out couch_log:debug calls, thanks! — Jan
%%
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec init_state() -> #state{}.
init_state() ->
    couch_log:debug("peruser: starting on node ~p in pid ~p", [node(), self()]),
    case config:get_boolean("couch_peruser", "enable", false) of
    false ->
        couch_log:debug("peruser: disabled on node ~p", [node()]),
        #state{};
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

        #state{
            parent = self(),
            db_name = DbName,
            delete_dbs = DeleteDbs,
            mem3_cluster_pid = Mem3Cluster,
            cluster_stable = false
        }
    end.


-spec start_listening(State :: #state{}) -> #state{} | ok.
start_listening(#state{states=ChangesStates}=State) when length(ChangesStates) > 0 ->
    % couch_log:debug("peruser: start_listening() already run on node ~p in pid ~p", [node(), self()]),
    State;
start_listening(#state{db_name=DbName, delete_dbs=DeleteDbs} = State) ->
    % couch_log:debug("peruser: start_listening() on node ~p", [node()]),
    try
        States = lists:map(fun (A) ->
            S = #changes_state{parent = State#state.parent,
                       db_name = A#shard.name,
                       delete_dbs = DeleteDbs},
            {Pid, Ref} = spawn_opt(
                ?MODULE, init_changes_handler, [S], [link, monitor]),
            S#changes_state{changes_pid=Pid, changes_ref=Ref}
        end, mem3:local_shards(DbName)),
        % couch_log:debug("peruser: start_listening() States ~p", [States]),

        State#state{states = States, cluster_stable = true}
    catch error:database_does_not_exist ->
        couch_log:warning("couch_peruser can't proceed as underlying database (~s) is missing, disables itself.", [DbName]),
        config:set("couch_peruser", "enable", "false", lists:concat([binary_to_list(DbName), " is missing"]))
    end.

-spec init_changes_handler(ChangesState :: #changes_state{}) -> ok.
init_changes_handler(#changes_state{db_name=DbName} = ChangesState) ->
    % couch_log:debug("peruser: init_changes_handler() on DbName ~p", [DbName]),
    try
        {ok, Db} = couch_db:open_int(DbName, [?ADMIN_CTX, sys_db]),
        FunAcc = {fun ?MODULE:changes_handler/3, ChangesState},
        (couch_changes:handle_db_changes(
             #changes_args{feed="continuous", timeout=infinity},
             {json_req, null},
             Db))(FunAcc)
    catch error:database_does_not_exist ->
        ok
    end.

-type db_change() :: {atom(), tuple(), binary()}.
-spec changes_handler(Change :: db_change(), ResultType :: any(), ChangesState :: #changes_state{}) -> #changes_state{}.
changes_handler({change, {Doc}, _Prepend}, _ResType, ChangesState=#changes_state{db_name=DbName}) ->
    % couch_log:debug("peruser: changes_handler() on DbName/Doc ~p/~p", [DbName, Doc]),

    case couch_util:get_value(<<"id">>, Doc) of
    <<"org.couchdb.user:",User/binary>> = DocId ->
        case should_handle_doc(DbName, DocId) of
        true ->
            case couch_util:get_value(<<"deleted">>, Doc, false) of
            false ->
                UserDb = ensure_user_db(User),
                ok = ensure_security(User, UserDb, fun add_user/3),
                ChangesState;
            true ->
                case ChangesState#changes_state.delete_dbs of
                true ->
                    _UserDb = delete_user_db(User),
                    ChangesState;
                false ->
                    UserDb = user_db_name(User),
                    ok = ensure_security(User, UserDb, fun remove_user/3),
                    ChangesState
                end
            end;
        false ->
            ChangesState
        end;
    _ ->
        ChangesState
    end;
changes_handler(_Event, _ResType, ChangesState) ->
    ChangesState.

-spec should_handle_doc(ShardName :: binary(), DocId::binary()) -> boolean().
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

-spec should_handle_doc_int(ShardName :: binary(), DocId :: binary()) -> boolean().
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

-spec delete_user_db(User :: binary()) -> binary().
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

-spec ensure_user_db(User :: binary()) -> binary().
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

-spec add_user(User :: binary(), Properties :: tuple(), Acc :: tuple()) -> tuple().
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

-spec remove_user(User :: binary(), Properties :: tuple(), Acc :: tuple()) -> tuple().
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

-spec ensure_security(User :: binary(), UserDb :: binary(), TransformFun :: fun()) -> ok.
ensure_security(User, UserDb, TransformFun) ->
    case fabric:get_all_security(UserDb, [?ADMIN_CTX]) of
    {error, no_majority} ->
       % TODO: make sure this is still true: single node, ignore
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

-spec user_db_name(User :: binary()) -> binary().
user_db_name(User) ->
    HexUser = list_to_binary(
        [string:to_lower(integer_to_list(X, 16)) || <<X>> <= User]),
    <<?USERDB_PREFIX,HexUser/binary>>.

-spec exit_changes(State :: #state{}) -> ok.
exit_changes(State) ->
    lists:foreach(fun (ChangesState) ->
        demonitor(State#changes_state.changes_ref, [flush]),
        exit(ChangesState#changes_state.changes_pid, kill)
    end, State#state.states).

-spec is_stable() -> true | false.
is_stable() ->
    gen_server:call(?MODULE, is_stable).

-spec subscribe_for_changes() -> ok.
subscribe_for_changes() ->
    config:subscribe_for_changes([
        {"couch_httpd_auth", "authentication_db"},
        "couch_peruser"
    ]).

% Mem3 cluster callbacks

% TODO: find out what type Server is
-spec cluster_unstable(Server :: any()) -> any().
cluster_unstable(Server) ->
    gen_server:cast(Server, cluster_unstable),
    Server.

% TODO: find out what type Server is
-spec cluster_stable(Server :: any()) -> any().
cluster_stable(Server) ->
    gen_server:cast(Server, cluster_stable),
    Server.

%% gen_server callbacks
-spec init(Options :: list()) -> {ok, #state{}}.
init([]) ->
    ok = subscribe_for_changes(),
    {ok, init_state()}.

handle_call(is_stable, _From, #state{cluster_stable = IsStable} = State) ->
    {reply, IsStable, State};
handle_call(_Msg, _From, State) ->
    {reply, error, State}.


handle_cast(update_config, State) when State#state.states =/= undefined ->
    exit_changes(State),
    {noreply, init_state()};
handle_cast(update_config, _) ->
    {noreply, init_state()};
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(cluster_unstable, State) when State#state.states =/= undefined ->
    exit_changes(State),
    {noreply, init_state()};
handle_cast(cluster_unstable, _) ->
    {noreply, init_state()};
handle_cast(cluster_stable, State) ->
    {noreply, start_listening(State)};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', Ref, _, _, _Reason}, #changes_state{changes_ref=Ref} = State) ->
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

terminate(_Reason, _State) ->
    %% Everything should be linked or monitored, let nature
    %% take its course.
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
