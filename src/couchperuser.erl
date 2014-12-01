-module(couchperuser).
-behaviour(gen_server).

-include_lib("couch/include/couch_db.hrl").

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

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
    ?LOG_DEBUG("couchperuser daemon: starting link.", []),
    Db_Name = ?l2b(couch_config:get(
                     "couch_httpd_auth", "authentication_db", "_users")),
    ok = watch_config_changes(),
    {Pid, Ref} = spawn_opt(?MODULE, init_changes, [self(), Db_Name],
                           [link, monitor]),
    {ok, #state{db_name=Db_Name,
                changes_pid=Pid,
                changes_ref=Ref}}.

watch_config_changes() ->
    Server = self(),
    couch_config:register(
      fun ("couch_httpd_auth", "authentication_db", _Value, _Persist) ->
              gen_server:cast(Server, stop);
          (_Section, _Key, _Value, _Persist) ->
              ok
      end).

admin_ctx() ->
    {user_ctx, #user_ctx{roles=[<<"_admin">>]}}.

init_changes(Parent, Db_Name) ->
    {ok, Db} = couch_db:open_int(Db_Name, [admin_ctx(), sys_db]),
    FunAcc = {fun ?MODULE:change_filter/3, #filter{server=Parent}},
    (couch_changes:handle_changes(
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
                    {ok, Db} = ensure_user_db(User),
                    try
                        ensure_security(User, Db)
                    after
                        couch_db:close(Db)
                    end,
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
    User_Db = user_db_name(User),
    case couch_db:open_int(User_Db, [admin_ctx(), nologifmissing]) of
        Ok={ok, _Db} ->
            Ok;
        _Err ->
            couch_db:create(User_Db, [admin_ctx()])
    end.

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

ensure_security(User, Db) ->
    {SecProps} = couch_db:get_security(Db),
    case lists:foldl(
           fun (Prop, SAcc) -> add_user(User, Prop, SAcc) end,
           {false, SecProps},
           [<<"admins">>, <<"members">>]) of
        {false, _} ->
            ok;
        {true, SecProps1} ->
            couch_db:set_security(Db, {SecProps1})
    end.

user_db_name(User) ->
    <<"userdb-", (iolist_to_binary(mochihex:to_hex(User)))/binary>>.

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
