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

-module(couch_password_hasher).

-behaviour(gen_server).

-include_lib("couch/include/couch_db.hrl").

-export([start_link/0]).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-export([maybe_upgrade_password_hash/6, hash_admin_passwords/1]).

-export([worker_loop/1]).

-define(IN_PROGRESS_ETS, couch_password_hasher_in_progress).

-record(state, {
    worker_pid
}).

%%%===================================================================
%%% Public functions
%%%===================================================================

maybe_upgrade_password_hash(Req, UserName, Password, UserProps, AuthModule, AuthCtx) ->
    UpgradeEnabled = config:get_boolean("chttpd_auth", "upgrade_hash_on_auth", true),
    IsDoc = is_doc(UserProps),
    NeedsUpgrade = needs_upgrade(UserProps),
    InProgress = in_progress(AuthModule, UserName),
    if
        UpgradeEnabled andalso IsDoc andalso NeedsUpgrade andalso not InProgress ->
            gen_server:cast(
                ?MODULE,
                {upgrade_password_hash, Req, UserName, Password, UserProps, AuthModule, AuthCtx}
            );
        true ->
            ok
    end.

-spec hash_admin_passwords(Persist :: boolean()) -> Reply :: term().
hash_admin_passwords(Persist) ->
    gen_server:cast(?MODULE, {hash_admin_passwords, Persist}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    hash_admin_passwords(true),
    ?IN_PROGRESS_ETS = ets:new(?IN_PROGRESS_ETS, [named_table, {read_concurrency, true}]),
    spawn_link(?MODULE, worker_loop, [self()]),
    {ok, start_worker_loop(#state{})}.

handle_call(Msg, _From, #state{} = State) ->
    {stop, {invalid_call, Msg}, {invalid_call, Msg}, State}.

handle_cast(
    {upgrade_password_hash, Req, UserName, Password, UserProps, AuthModule, AuthCtx}, State
) ->
    case ets:insert_new(?IN_PROGRESS_ETS, {{AuthModule, UserName}}) of
        true ->
            State#state.worker_pid !
                {upgrade_password_hash, Req, UserName, Password, UserProps, AuthModule, AuthCtx};
        false ->
            ok
    end,
    {noreply, State};
handle_cast({hash_admin_passwords, Persist}, State) ->
    hash_admin_passwords_int(Persist),
    {noreply, State};
handle_cast(Msg, State) ->
    {stop, {invalid_cast, Msg}, State}.

handle_info({done, AuthModule, UserName}, State) ->
    ets:delete(?IN_PROGRESS_ETS, {AuthModule, UserName}),
    {noreply, State};
handle_info(Msg, State) ->
    {stop, {invalid_info, Msg}, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

hash_admin_passwords_int(Persist) ->
    lists:foreach(
        fun({User, ClearPassword}) ->
            HashedPassword = couch_passwords:hash_admin_password(ClearPassword),
            config:set("admins", User, ?b2l(HashedPassword), Persist)
        end,
        couch_passwords:get_unhashed_admins()
    ).

is_doc(UserProps) ->
    couch_util:get_value(<<"_rev">>, UserProps) /= undefined.

needs_upgrade(UserProps) ->
    CurrentScheme = couch_util:get_value(<<"password_scheme">>, UserProps),
    TargetScheme = ?l2b(chttpd_util:get_chttpd_auth_config("password_scheme", "pbkdf2")),
    CurrentPRF = couch_util:get_value(<<"pbkdf2_prf">>, UserProps),
    TargetPRF = ?l2b(chttpd_util:get_chttpd_auth_config("pbkdf2_prf", "sha256")),
    CurrentIterations = couch_util:get_value(<<"iterations">>, UserProps),
    TargetIterations = chttpd_util:get_chttpd_auth_config_integer(
        "iterations", 600000
    ),
    case {TargetScheme, TargetIterations, TargetPRF} of
        {CurrentScheme, CurrentIterations, _} when CurrentScheme == <<"simple">> ->
            false;
        {CurrentScheme, CurrentIterations, CurrentPRF} when CurrentScheme == <<"pbkdf2">> ->
            false;
        {_, _, _} ->
            true
    end.

in_progress(AuthModule, UserName) ->
    ets:member(?IN_PROGRESS_ETS, {AuthModule, UserName}).

start_worker_loop(State) ->
    WorkerPid = spawn_link(?MODULE, worker_loop, [self()]),
    State#state{worker_pid = WorkerPid}.

worker_loop(Parent) ->
    receive
        {upgrade_password_hash, Req, UserName, Password, UserProps, AuthModule, AuthCtx} ->
            couch_log:notice("upgrading stored password hash for '~s' (~p)", [UserName, AuthCtx]),
            upgrade_password_hash(Req, UserName, Password, UserProps, AuthModule, AuthCtx),
            erlang:send_after(5000, Parent, {done, AuthModule, UserName});
        _Msg ->
            ignore
    end,
    worker_loop(Parent).

upgrade_password_hash(Req, _UserName, Password, UserProps0, AuthModule, AuthCtx) ->
    UserProps1 = [{<<"password">>, Password}, {<<"preserve_salt">>, true} | UserProps0],
    NewUserDoc = couch_doc:from_json_obj({UserProps1}),
    catch AuthModule:update_user_creds(Req, NewUserDoc, AuthCtx).
