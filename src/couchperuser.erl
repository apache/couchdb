-module(couchperuser).
-behaviour(gen_server).
-behaviour(config_listener).

-include_lib("couch/include/couch_db.hrl").

% gen_server callbacks
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

% config_listener callbacks
-export([handle_config_change/5, handle_config_terminate/3]).

-export([init_changes/2, change_filter/3]).

%% Note that this doesn't actually depend on having a registered name
-define(NAME, ?MODULE).
%% db_name and changes_pid are useful information to have, but unused
-record(state, {db_name, changes_pid, changes_ref}).
%% the entire filter state is currently unused, but may be useful later
-record(filter, {server}).

start_link() ->
    gen_server:start_link({local, ?NAME}, ?MODULE, [], []).

init([]) ->
    couch_log:debug("couchperuser daemon: starting link.", []),
    Db_Name = ?l2b(config:get(
                     "couch_httpd_auth", "authentication_db", "_users")),
    Server = self(),
    ok = config:listen_for_changes(?MODULE, Server),
    {Pid, Ref} = spawn_opt(?MODULE, init_changes, [Server, Db_Name],
                           [link, monitor]),
    {ok, #state{db_name=Db_Name,
                changes_pid=Pid,
                changes_ref=Ref}}.

handle_config_change("couch_httpd_auth", "authentication_db", _Value, _Persist, State) ->
   gen_server:cast(State, stop),
   remove_handler;
handle_config_change(_Section, _Key, _Value, _Persist, State) ->
    {ok, State}.

handle_config_terminate(_, stop, _) -> ok;
handle_config_terminate(Self, _, _) ->
    spawn(fun() ->
        timer:sleep(5000),
        config:listen_for_changes(?MODULE, Self)
    end).

admin_ctx() ->
    {user_ctx, #user_ctx{roles=[<<"_admin">>]}}.

init_changes(Parent, Db_Name) ->
    {ok, Db} = couch_db:open_int(Db_Name, [admin_ctx(), sys_db]),
    FunAcc = {fun ?MODULE:change_filter/3, #filter{server=Parent}},
    (couch_changes:handle_db_changes(
       #changes_args{feed="continuous", timeout=infinity},
       {json_req, null},
       Db))(FunAcc).

change_filter({change, {Doc}, _Prepend}, _ResType, Acc=#filter{}) ->
    Deleted = couch_util:get_value(<<"deleted">>, Doc, false),
    case lists:keyfind(<<"id">>, 1, Doc) of
        {_Key, <<"org.couchdb.user:", User/binary>>} ->
            case Deleted of
                true ->
                    %% TODO: Let's not complicate this with GC for now!
                    Acc;
                false ->
                    UserDb = ensure_user_db(User),
                    ensure_security(User, UserDb),
                    Acc
            end;
        _ ->
            Acc
    end;
change_filter(_Event, _ResType, Acc) ->
    Acc.

terminate(_Reason, _State) ->
    %% Everything should be linked or monitored, let nature
    %% take its course.
    ok.

ensure_user_db(User) ->
    UserDb = user_db_name(User),
    try
        fabric_db_info:go(UserDb)
    catch error:database_does_not_exist ->
        fabric_db_create:go(UserDb, [admin_ctx()])
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
    {ok, Shards} = fabric_db_meta:get_all_security(UserDb, [admin_ctx()]),
    % We assume that all shards have the same security object, and
    % therefore just pick the first one.
    {_ShardInfo, {SecProps}} = hd(Shards),
    case lists:foldl(
           fun (Prop, SAcc) -> add_user(User, Prop, SAcc) end,
           {false, SecProps},
           [<<"admins">>, <<"members">>]) of
        {false, _} ->
            ok;
        {true, SecProps1} ->
            fabric_db_meta:set_security(UserDb, {SecProps1}, [admin_ctx()])
    end.

user_db_name(User) ->
    HexUser = list_to_binary([integer_to_list(X, 16) || <<X>> <= User]),
    <<"userdb-", HexUser/binary>>.

handle_call(_Msg, _From, State) ->
    {reply, error, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', Ref, _, _, _Reason}, State=#state{changes_ref=Ref}) ->
    {stop, normal, State};
handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
