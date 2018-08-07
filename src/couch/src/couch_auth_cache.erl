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

-module(couch_auth_cache).
-behaviour(gen_server).
-vsn(3).
-behaviour(config_listener).

% public API
-export([get_user_creds/1, get_user_creds/2, update_user_creds/3]).
-export([get_admin/1, add_roles/2, auth_design_doc/1]).

% gen_server API
-export([start_link/0, init/1, handle_call/3, handle_info/2, handle_cast/2]).
-export([code_change/3, terminate/2]).

-export([handle_config_change/5, handle_config_terminate/3]).
-export([handle_db_event/3]).

-include_lib("couch/include/couch_db.hrl").
-include_lib("couch/include/couch_js_functions.hrl").

-define(STATE, auth_state_ets).
-define(BY_USER, auth_by_user_ets).
-define(BY_ATIME, auth_by_atime_ets).

-define(RELISTEN_DELAY, 5000).

-record(state, {
    max_cache_size = 0,
    cache_size = 0,
    db_notifier = nil,
    event_listener = nil
}).


-spec get_user_creds(UserName::string() | binary()) ->
    {ok, Credentials::list(), term()} | nil.

get_user_creds(UserName) ->
    get_user_creds(nil, UserName).

-spec get_user_creds(Req::#httpd{} | nil, UserName::string() | binary()) ->
    {ok, Credentials::list(), term()} | nil.

get_user_creds(Req, UserName) when is_list(UserName) ->
    get_user_creds(Req, ?l2b(UserName));

get_user_creds(_Req, UserName) ->
    UserCreds = case get_admin(UserName) of
    nil ->
        get_from_cache(UserName);
    Props ->
        case get_from_cache(UserName) of
        nil ->
            Props;
        UserProps when is_list(UserProps) ->
            add_roles(Props, couch_util:get_value(<<"roles">>, UserProps))
        end
    end,
    validate_user_creds(UserCreds).

update_user_creds(_Req, UserDoc, _AuthCtx) ->
    DbNameList = config:get("couch_httpd_auth", "authentication_db", "_users"),
    couch_util:with_db(?l2b(DbNameList), fun(UserDb) ->
        {ok, _NewRev} = couch_db:update_doc(UserDb, UserDoc, []),
        ok
    end).

add_roles(Props, ExtraRoles) ->
    CurrentRoles = couch_util:get_value(<<"roles">>, Props),
    lists:keyreplace(<<"roles">>, 1, Props, {<<"roles">>, CurrentRoles ++ ExtraRoles}).

get_admin(UserName) when is_binary(UserName) ->
    get_admin(?b2l(UserName));
get_admin(UserName) when is_list(UserName) ->
    case config:get("admins", UserName) of
    "-hashed-" ++ HashedPwdAndSalt ->
        % the name is an admin, now check to see if there is a user doc
        % which has a matching name, salt, and password_sha
        [HashedPwd, Salt] = string:tokens(HashedPwdAndSalt, ","),
        make_admin_doc(HashedPwd, Salt);
    "-pbkdf2-" ++ HashedPwdSaltAndIterations ->
        [HashedPwd, Salt, Iterations] = string:tokens(HashedPwdSaltAndIterations, ","),
        make_admin_doc(HashedPwd, Salt, Iterations);
    _Else ->
	nil
    end.

make_admin_doc(HashedPwd, Salt) ->
    [{<<"roles">>, [<<"_admin">>]},
     {<<"salt">>, ?l2b(Salt)},
     {<<"password_scheme">>, <<"simple">>},
     {<<"password_sha">>, ?l2b(HashedPwd)}].

make_admin_doc(DerivedKey, Salt, Iterations) ->
    [{<<"roles">>, [<<"_admin">>]},
     {<<"salt">>, ?l2b(Salt)},
     {<<"iterations">>, list_to_integer(Iterations)},
     {<<"password_scheme">>, <<"pbkdf2">>},
     {<<"derived_key">>, ?l2b(DerivedKey)}].

get_from_cache(UserName) ->
    exec_if_auth_db(
        fun(_AuthDb) ->
            maybe_refresh_cache(),
            case ets:lookup(?BY_USER, UserName) of
            [] ->
                gen_server:call(?MODULE, {fetch, UserName}, infinity);
            [{UserName, {Credentials, _ATime}}] ->
                couch_stats:increment_counter([couchdb, auth_cache_hits]),
                gen_server:cast(?MODULE, {cache_hit, UserName}),
                Credentials
            end
        end,
        nil
    ).


validate_user_creds(nil) ->
    nil;
validate_user_creds(UserCreds) ->
    case couch_util:get_value(<<"_conflicts">>, UserCreds) of
    undefined ->
        ok;
    _ConflictList ->
        throw({unauthorized,
            <<"User document conflicts must be resolved before the document",
              " is used for authentication purposes.">>
        })
    end,
    {ok, UserCreds, nil}.


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init(_) ->
    ?STATE = ets:new(?STATE, [set, protected, named_table]),
    ?BY_USER = ets:new(?BY_USER, [set, protected, named_table]),
    ?BY_ATIME = ets:new(?BY_ATIME, [ordered_set, private, named_table]),
    AuthDbName = config:get("couch_httpd_auth", "authentication_db"),
    process_flag(trap_exit, true),
    ok = config:listen_for_changes(?MODULE, nil),
    {ok, Listener} = couch_event:link_listener(
            ?MODULE, handle_db_event, nil, [{dbname, AuthDbName}]
        ),
    State = #state{
        event_listener = Listener,
        max_cache_size = list_to_integer(
            config:get("couch_httpd_auth", "auth_cache_size", "50")
        )
    },
    {ok, reinit_cache(State)}.


handle_db_event(_DbName, created, St) ->
    gen_server:call(?MODULE, reinit_cache, infinity),
    {ok, St};
handle_db_event(_DbName, compacted, St) ->
    gen_server:call(?MODULE, auth_db_compacted, infinity),
    {ok, St};
handle_db_event(_, _, St) ->
    {ok, St}.


handle_call(reinit_cache, _From, State) ->
    exec_if_auth_db(fun(AuthDb) -> catch couch_db:close(AuthDb) end),
    {reply, ok, reinit_cache(State)};

handle_call(auth_db_compacted, _From, State) ->
    exec_if_auth_db(
        fun(AuthDb) ->
            true = ets:insert(?STATE, {auth_db, reopen_auth_db(AuthDb)})
        end
    ),
    {reply, ok, State};

handle_call({new_max_cache_size, NewSize},
        _From, #state{cache_size = Size} = State) when NewSize >= Size ->
    {reply, ok, State#state{max_cache_size = NewSize}};

handle_call({new_max_cache_size, NewSize}, _From, State) ->
    free_mru_cache_entries(State#state.cache_size - NewSize),
    {reply, ok, State#state{max_cache_size = NewSize, cache_size = NewSize}};

handle_call({fetch, UserName}, _From, State) ->
    {Credentials, NewState} = case ets:lookup(?BY_USER, UserName) of
    [{UserName, {Creds, ATime}}] ->
        couch_stats:increment_counter([couchdb, auth_cache_hits]),
        cache_hit(UserName, Creds, ATime),
        {Creds, State};
    [] ->
        couch_stats:increment_counter([couchdb, auth_cache_misses]),
        Creds = get_user_props_from_db(UserName),
        ATime = couch_util:unique_monotonic_integer(),
        State1 = add_cache_entry(UserName, Creds, ATime, State),
        {Creds, State1}
    end,
    {reply, Credentials, NewState};

handle_call(refresh, _From, State) ->
    exec_if_auth_db(fun refresh_entries/1),
    {reply, ok, State}.


handle_cast({cache_hit, UserName}, State) ->
    case ets:lookup(?BY_USER, UserName) of
    [{UserName, {Credentials, ATime}}] ->
        cache_hit(UserName, Credentials, ATime);
    _ ->
        ok
    end,
    {noreply, State}.


handle_info({'EXIT', LPid, _Reason}, #state{event_listener=LPid}=State) ->
    erlang:send_after(5000, self(), restart_event_listener),
    {noreply, State#state{event_listener=undefined}};
handle_info(restart_event_listener, State) ->
    [{auth_db_name, AuthDbName}] = ets:lookup(?STATE, auth_db_name),
    {ok, NewListener} = couch_event:link_listener(
            ?MODULE, handle_db_event, nil, [{dbname, AuthDbName}]
        ),
    {noreply, State#state{event_listener=NewListener}};
handle_info({'DOWN', _Ref, _, _, shutdown}, State) ->
    {stop, shutdown, State};
handle_info({'DOWN', _Ref, _, _, _}, State) ->
    {noreply, reinit_cache(State)};
handle_info(restart_config_listener, State) ->
    ok = config:listen_for_changes(?MODULE, nil),
    {noreply, State}.



terminate(_Reason, #state{event_listener = Listener}) ->
    couch_event:stop_listener(Listener),
    exec_if_auth_db(fun(AuthDb) -> catch couch_db:close(AuthDb) end),
    true = ets:delete(?BY_USER),
    true = ets:delete(?BY_ATIME),
    true = ets:delete(?STATE).


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


handle_config_change("couch_httpd_auth", "auth_cache_size", SizeList, _, _) ->
    Size = list_to_integer(SizeList),
    {ok, gen_server:call(?MODULE, {new_max_cache_size, Size}, infinity)};
handle_config_change("couch_httpd_auth", "authentication_db", _DbName, _, _) ->
    {ok, gen_server:call(?MODULE, reinit_cache, infinity)};
handle_config_change(_, _, _, _, _) ->
    {ok, nil}.

handle_config_terminate(_, stop, _) ->
    ok;
handle_config_terminate(_Server, _Reason, _State) ->
    erlang:send_after(?RELISTEN_DELAY, whereis(?MODULE), restart_config_listener).

clear_cache(State) ->
    exec_if_auth_db(fun(AuthDb) -> catch couch_db:close(AuthDb) end),
    true = ets:delete_all_objects(?BY_USER),
    true = ets:delete_all_objects(?BY_ATIME),
    State#state{cache_size = 0}.


reinit_cache(#state{} = State) ->
    NewState = clear_cache(State),
    AuthDbName = ?l2b(config:get("couch_httpd_auth", "authentication_db")),
    true = ets:insert(?STATE, {auth_db_name, AuthDbName}),
    AuthDb = open_auth_db(),
    true = ets:insert(?STATE, {auth_db, AuthDb}),
    couch_db:monitor(AuthDb),
    NewState.


add_cache_entry(_, _, _, #state{max_cache_size = 0} = State) ->
    State;
add_cache_entry(UserName, Credentials, ATime, State) ->
    case State#state.cache_size >= State#state.max_cache_size of
    true ->
        free_mru_cache_entry();
    false ->
        ok
    end,
    true = ets:insert(?BY_ATIME, {ATime, UserName}),
    true = ets:insert(?BY_USER, {UserName, {Credentials, ATime}}),
    State#state{cache_size = couch_util:get_value(size, ets:info(?BY_USER))}.

free_mru_cache_entries(0) ->
    ok;
free_mru_cache_entries(N) when N > 0 ->
    free_mru_cache_entry(),
    free_mru_cache_entries(N - 1).

free_mru_cache_entry() ->
    MruTime = ets:last(?BY_ATIME),
    [{MruTime, UserName}] = ets:lookup(?BY_ATIME, MruTime),
    true = ets:delete(?BY_ATIME, MruTime),
    true = ets:delete(?BY_USER, UserName).


cache_hit(UserName, Credentials, ATime) ->
    NewATime = couch_util:unique_monotonic_integer(),
    true = ets:delete(?BY_ATIME, ATime),
    true = ets:insert(?BY_ATIME, {NewATime, UserName}),
    true = ets:insert(?BY_USER, {UserName, {Credentials, NewATime}}).


refresh_entries(AuthDb) ->
    case reopen_auth_db(AuthDb) of
    nil ->
        ok;
    AuthDb2 ->
        AuthDbSeq = couch_db:get_update_seq(AuthDb),
        AuthDb2Seq = couch_db:get_update_seq(AuthDb2),
        case AuthDb2Seq > AuthDbSeq of
        true ->
            Fun = fun(DocInfo, _) -> refresh_entry(AuthDb2, DocInfo) end,
            {ok, _} = couch_db:fold_changes(AuthDb2, AuthDbSeq, Fun, nil),
            true = ets:insert(?STATE, {auth_db, AuthDb2});
        false ->
            ok
        end
    end.


refresh_entry(Db, #full_doc_info{} = FDI) ->
    refresh_entry(Db, couch_doc:to_doc_info(FDI));
refresh_entry(Db, #doc_info{high_seq = DocSeq} = DocInfo) ->
    case is_user_doc(DocInfo) of
    {true, UserName} ->
        case ets:lookup(?BY_USER, UserName) of
        [] ->
            ok;
        [{UserName, {_OldCreds, ATime}}] ->
            {ok, Doc} = couch_db:open_doc(Db, DocInfo, [conflicts, deleted]),
            NewCreds = user_creds(Doc),
            true = ets:insert(?BY_USER, {UserName, {NewCreds, ATime}})
        end;
    false ->
        ok
    end,
    {ok, DocSeq}.


user_creds(#doc{deleted = true}) ->
    nil;
user_creds(#doc{} = Doc) ->
    {Creds} = couch_doc:to_json_obj(Doc, []),
    Creds.


is_user_doc(#doc_info{id = <<"org.couchdb.user:", UserName/binary>>}) ->
    {true, UserName};
is_user_doc(_) ->
    false.


maybe_refresh_cache() ->
    case cache_needs_refresh() of
    true ->
        ok = gen_server:call(?MODULE, refresh, infinity);
    false ->
        ok
    end.


cache_needs_refresh() ->
    exec_if_auth_db(
        fun(AuthDb) ->
            case reopen_auth_db(AuthDb) of
            nil ->
                false;
            AuthDb2 ->
                AuthDbSeq = couch_db:get_update_seq(AuthDb),
                AuthDb2Seq = couch_db:get_update_seq(AuthDb2),
                AuthDb2Seq > AuthDbSeq
            end
        end,
        false
    ).


reopen_auth_db(AuthDb) ->
    case (catch couch_db:reopen(AuthDb)) of
    {ok, AuthDb2} ->
        AuthDb2;
    _ ->
        nil
    end.


exec_if_auth_db(Fun) ->
    exec_if_auth_db(Fun, ok).

exec_if_auth_db(Fun, DefRes) ->
    case ets:lookup(?STATE, auth_db) of
    [{auth_db, AuthDb}] ->
        Fun(AuthDb);
    _ ->
        DefRes
    end.


open_auth_db() ->
    [{auth_db_name, DbName}] = ets:lookup(?STATE, auth_db_name),
    {ok, AuthDb} = ensure_users_db_exists(DbName, [sys_db]),
    AuthDb.


get_user_props_from_db(UserName) ->
    exec_if_auth_db(
        fun(AuthDb) ->
            Db = reopen_auth_db(AuthDb),
            DocId = <<"org.couchdb.user:", UserName/binary>>,
            try
                {ok, Doc} = couch_db:open_doc(Db, DocId, [conflicts]),
                {DocProps} = couch_doc:to_json_obj(Doc, []),
                DocProps
            catch
            _:_Error ->
                nil
            end
        end,
        nil
    ).

ensure_users_db_exists(DbName, Options) ->
    Options1 = [?ADMIN_CTX, nologifmissing | Options],
    case couch_db:open(DbName, Options1) of
    {ok, Db} ->
        ensure_auth_ddoc_exists(Db, <<"_design/_auth">>),
        {ok, Db};
    _Error ->
        {ok, Db} = couch_db:create(DbName, Options1),
        ok = ensure_auth_ddoc_exists(Db, <<"_design/_auth">>),
        {ok, Db}
    end.

ensure_auth_ddoc_exists(Db, DDocId) ->
    case couch_db:open_doc(Db, DDocId) of
    {not_found, _Reason} ->
        {ok, AuthDesign} = auth_design_doc(DDocId),
        {ok, _Rev} = couch_db:update_doc(Db, AuthDesign, []);
    {ok, Doc} ->
        {Props} = couch_doc:to_json_obj(Doc, []),
        case couch_util:get_value(<<"validate_doc_update">>, Props, []) of
            ?AUTH_DB_DOC_VALIDATE_FUNCTION ->
                ok;
            _ ->
                Props1 = lists:keyreplace(<<"validate_doc_update">>, 1, Props,
                    {<<"validate_doc_update">>,
                    ?AUTH_DB_DOC_VALIDATE_FUNCTION}),
                couch_db:update_doc(Db, couch_doc:from_json_obj({Props1}), [])
        end
    end,
    ok.

auth_design_doc(DocId) ->
    DocProps = [
        {<<"_id">>, DocId},
        {<<"language">>,<<"javascript">>},
        {<<"validate_doc_update">>, ?AUTH_DB_DOC_VALIDATE_FUNCTION}
    ],
    {ok, couch_doc:from_json_obj({DocProps})}.
