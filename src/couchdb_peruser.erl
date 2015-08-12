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

-module(couchdb_peruser).
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

init([]) ->
    Server = self(),
    ok = config:listen_for_changes(?MODULE, Server),
    case config:get_boolean("couchdb_peruser", "enable", false) of
    false ->
        {ok, #state{parent = Server}};
    true ->
        DbName = ?l2b(config:get(
                         "couch_httpd_auth", "authentication_db", "_users")),
        DeleteDbs = config:get_boolean("couchdb_peruser", "delete_dbs", false),
        State = #state{parent = Server,
                       db_name = DbName,
                       delete_dbs = DeleteDbs},
        {Pid, Ref} = spawn_opt(
            ?MODULE, init_changes_handler, [State], [link, monitor]),
        {ok, State#state{changes_pid=Pid, changes_ref=Ref}}
    end.

handle_config_change("couch_httpd_auth", "authentication_db", _Value, _Persist, Server) ->
    gen_server:cast(Server, stop),
    remove_handler;
handle_config_change("couchdb_peruser", _Key, _Value, _Persist, Server) ->
    gen_server:cast(Server, stop),
    remove_handler;
handle_config_change(_Section, _Key, _Value, _Persist, Server) ->
    {ok, Server}.

handle_config_terminate(_Self, Reason, _Server) ->
    {stop, Reason}.

init_changes_handler(State) ->
    {ok, Db} = couch_db:open_int(State#state.db_name, [?ADMIN_CTX, sys_db]),
    FunAcc = {fun ?MODULE:changes_handler/3, State},
    (couch_changes:handle_db_changes(
         #changes_args{feed="continuous", timeout=infinity},
         {json_req, null},
         Db))(FunAcc).

changes_handler({change, {Doc}, _Prepend}, _ResType, State=#state{}) ->
    Deleted = couch_util:get_value(<<"deleted">>, Doc, false),
    case lists:keyfind(<<"id">>, 1, Doc) of
    {_Key, <<"org.couchdb.user:", User/binary>>} ->
        case Deleted of
        true ->
            case State#state.delete_dbs of
            true ->
                _UserDb = delete_user_db(User),
                State;
            false ->
                State
            end;
        false ->
            UserDb = ensure_user_db(User),
            ensure_security(User, UserDb),
            State
        end;
    _ ->
        State
    end;
changes_handler(_Event, _ResType, State) ->
    State.

terminate(_Reason, _State) ->
    %% Everything should be linked or monitored, let nature
    %% take its course.
    ok.

delete_user_db(User) ->
    UserDb = user_db_name(User),
    try
        fabric_db_delete:go(UserDb, [?ADMIN_CTX])
    catch error:database_does_not_exist ->
        ok
    end,
    UserDb.

ensure_user_db(User) ->
    UserDb = user_db_name(User),
    try
        fabric_db_info:go(UserDb)
    catch error:database_does_not_exist ->
        fabric_db_create:go(UserDb, [?ADMIN_CTX])
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

ensure_security(User, UserDb) ->
    {ok, Shards} = fabric_db_meta:get_all_security(UserDb, [?ADMIN_CTX]),
    {_ShardInfo, {SecProps}} = hd(Shards),
    % assert that shards have the same security object
    true = lists:all(fun({_, {SecProps1}}) ->
        SecProps =:= SecProps1
    end, Shards),
    case lists:foldl(
           fun(Prop, SAcc) -> add_user(User, Prop, SAcc) end,
           {false, SecProps},
           [<<"admins">>, <<"members">>]) of
    {false, _} ->
        ok;
    {true, SecProps1} ->
        fabric_db_meta:set_security(UserDb, {SecProps1}, [?ADMIN_CTX])
    end.

user_db_name(User) ->
    HexUser = list_to_binary(
        [string:to_lower(integer_to_list(X, 16)) || <<X>> <= User]),
    <<?USERDB_PREFIX, HexUser/binary>>.

handle_call(_Msg, _From, State) ->
    {reply, error, State}.

handle_cast(stop, State) ->
    % we don't want to have multiple changes handler at the same time
    exit(State#state.changes_pid, kill),
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', Ref, _, _, _Reason}, State=#state{changes_ref=Ref}) ->
    {stop, normal, State};
handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
