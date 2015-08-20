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
-behaviour(config_listener).

-include_lib("couch/include/couch_db.hrl").

-define(USERDB_PREFIX, "userdb-").

% gen_server callbacks
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

% config_listener callbacks
-export([handle_config_change/5, handle_config_terminate/3]).

-export([init_changes_handler/1, changes_handler/3]).

-record(state, {parent, db_name, delete_dbs, changes_pid, changes_ref}).


start_link() ->
    gen_server:start_link(?MODULE, [], []).

init() ->
    case config:get_boolean("couch_peruser", "enable", false) of
    false ->
        #state{};
    true ->
        DbName = ?l2b(config:get(
                         "couch_httpd_auth", "authentication_db", "_users")),
        DeleteDbs = config:get_boolean("couch_peruser", "delete_dbs", false),
        State = #state{parent = self(),
                       db_name = DbName,
                       delete_dbs = DeleteDbs},
        {Pid, Ref} = spawn_opt(
            ?MODULE, init_changes_handler, [State], [link, monitor]),
        State#state{changes_pid=Pid, changes_ref=Ref}
    end.

init_changes_handler(#state{db_name=DbName} = State) ->
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

changes_handler({change, {Doc}, _Prepend}, _ResType, State=#state{}) ->
    case couch_util:get_value(<<"id">>, Doc) of
    <<"org.couchdb.user:",User/binary>> ->
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
    _ ->
        State
    end;
changes_handler(_Event, _ResType, State) ->
    State.

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
    {ok, Shards} = fabric:get_all_security(UserDb, [?ADMIN_CTX]),
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
    end.

user_db_name(User) ->
    HexUser = list_to_binary(
        [string:to_lower(integer_to_list(X, 16)) || <<X>> <= User]),
    <<?USERDB_PREFIX,HexUser/binary>>.


%% gen_server callbacks

init([]) ->
    ok = config:listen_for_changes(?MODULE, self()),
    {ok, init()}.

handle_call(_Msg, _From, State) ->
    {reply, error, State}.

handle_cast(update_config, State) when State#state.changes_pid =/= undefined ->
    % we don't want to have multiple changes handler at the same time
    demonitor(State#state.changes_ref, [flush]),
    exit(State#state.changes_pid, kill),
    {noreply, init()};
handle_cast(update_config, _State) ->
    {noreply, init()};
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', Ref, _, _, _Reason}, #state{changes_ref=Ref} = State) ->
    {stop, normal, State};
handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    %% Everything should be linked or monitored, let nature
    %% take its course.
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% config_listener callbacks

handle_config_change("couch_httpd_auth", "authentication_db", _Value, _Persist, Server) ->
    ok = gen_server:cast(Server, update_config),
    {ok, Server};
handle_config_change("couch_peruser", _Key, _Value, _Persist, Server) ->
    ok = gen_server:cast(Server, update_config),
    {ok, Server};
handle_config_change(_Section, _Key, _Value, _Persist, Server) ->
    {ok, Server}.

handle_config_terminate(_Self, Reason, _Server) ->
    {stop, Reason}.
