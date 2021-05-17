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

-module(couch_server).
-behaviour(gen_server).
-behaviour(config_listener).
-vsn(3).

-export([get_version/0, get_version/1, get_git_sha/0, get_uuid/0]).
-export([init/1, handle_call/3, sup_start_link/0]).
-export([handle_cast/2, code_change/3, handle_info/2, terminate/2]).
-export([is_admin/2, has_admins/0]).

% config_listener api
-export([handle_config_change/5, handle_config_terminate/3]).

-include_lib("couch/include/couch_db.hrl").

-define(RELISTEN_DELAY, 5000).

get_version() ->
    %% Defined in rebar.config.script
    ?COUCHDB_VERSION.
get_version(short) ->
    %% strip git hash from version string
    [Version | _Rest] = string:tokens(get_version(), "+"),
    Version.

get_git_sha() -> ?COUCHDB_GIT_SHA.

get_uuid() ->
    case config:get("couchdb", "uuid", undefined) of
        undefined ->
            UUID = couch_uuids:random(),
            config:set("couchdb", "uuid", ?b2l(UUID)),
            UUID;
        UUID ->
            ?l2b(UUID)
    end.

sup_start_link() ->
    gen_server:start_link({local, couch_server}, couch_server, [], []).

is_admin(User, ClearPwd) ->
    case config:get("admins", User) of
        "-hashed-" ++ HashedPwdAndSalt ->
            [HashedPwd, Salt] = string:tokens(HashedPwdAndSalt, ","),
            couch_util:to_hex(crypto:hash(sha, ClearPwd ++ Salt)) == HashedPwd;
        _Else ->
            false
    end.

has_admins() ->
    config:get("admins") /= [].

hash_admin_passwords() ->
    hash_admin_passwords(true).

hash_admin_passwords(Persist) ->
    lists:foreach(
        fun({User, ClearPassword}) ->
            HashedPassword = couch_passwords:hash_admin_password(ClearPassword),
            config:set("admins", User, ?b2l(HashedPassword), Persist)
        end,
        couch_passwords:get_unhashed_admins()
    ).

init([]) ->
    % Mark being able to receive documents with an _access property as a supported feature
    config:enable_feature('access-ready'),
    % Mark if fips is enabled
    case
        erlang:function_exported(crypto, info_fips, 0) andalso
            crypto:info_fips() == enabled
    of
        true ->
            config:enable_feature('fips');
        false ->
            ok
    end,
    ok = config:listen_for_changes(?MODULE, nil),
    hash_admin_passwords(),
    {ok, nil}.

handle_call(Msg, _From, Srv) ->
    {stop, {bad_call, Msg}, Srv}.

handle_cast(Msg, Srv) ->
    {stop, {bad_cast, Msg}, Srv}.

handle_info(Msg, Srv) ->
    {stop, {unknown_message, Msg}, Srv}.

code_change(_OldVsn, Srv, _Extra) ->
    {ok, Srv}.

terminate(_Reason, _Srv) ->
    ok.

handle_config_change("admins", _, _, Persist, _) ->
    % spawn here so couch event manager doesn't deadlock
    {ok, spawn(fun() -> hash_admin_passwords(Persist) end)};
handle_config_change(_, _, _, _, _) ->
    {ok, nil}.

handle_config_terminate(_, stop, _) ->
    ok;
handle_config_terminate(_Server, _Reason, _State) ->
    erlang:send_after(?RELISTEN_DELAY, whereis(?MODULE), restart_config_listener).
