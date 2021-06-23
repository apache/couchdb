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

-module(couch_replicator_auth).

-export([
    initialize/1,
    update_headers/2,
    handle_response/3,
    cleanup/1
]).

-include_lib("couch_replicator/include/couch_replicator_api_wrap.hrl").

-type headers() :: [{string(), string()}].
-type code() :: non_neg_integer().

-define(DEFAULT_PLUGINS, "couch_replicator_auth_session,couch_replicator_auth_noop").

% Behavior API

% Note for plugin developers: consider using the "auth" field in the source and
% target objects to store credentials. In that case non-owner and non-admin
% users will have those credentials stripped when they read the replication
% document, which mimicks the behavior for "headers" and user and pass fields
% in endpoint URLs".

-callback initialize(#httpdb{}) ->
    {ok, #httpdb{}, term()} | {error, term()} | ignore.

-callback update_headers(term(), headers()) -> {headers(), term()}.

-callback handle_response(term(), code(), headers()) ->
    {continue | retry, term()}.

-callback cleanup(term()) -> ok.

% Main API

-spec initialize(#httpdb{}) -> {ok, #httpdb{}} | {error, term()}.
initialize(#httpdb{auth_context = nil} = HttpDb) ->
    case try_initialize(get_plugin_modules(), HttpDb) of
        {ok, Mod, HttpDb1, Context} ->
            {ok, HttpDb1#httpdb{auth_context = {Mod, Context}}};
        {error, Error} ->
            {error, Error}
    end.

-spec update_headers(#httpdb{}, headers()) -> {headers(), #httpdb{}}.
update_headers(#httpdb{auth_context = {Mod, Context}} = HttpDb, Headers) ->
    {Headers1, Context1} = Mod:update_headers(Context, Headers),
    {Headers1, HttpDb#httpdb{auth_context = {Mod, Context1}}}.

-spec handle_response(#httpdb{}, code(), headers()) ->
    {continue | retry, term()}.
handle_response(#httpdb{} = HttpDb, Code, Headers) ->
    {Mod, Context} = HttpDb#httpdb.auth_context,
    {Res, Context1} = Mod:handle_response(Context, Code, Headers),
    {Res, HttpDb#httpdb{auth_context = {Mod, Context1}}}.

-spec cleanup(#httpdb{}) -> #httpdb{}.
cleanup(#httpdb{auth_context = {Module, Context}} = HttpDb) ->
    ok = Module:cleanup(Context),
    HttpDb#httpdb{auth_context = nil}.

% Private helper functions

-spec get_plugin_modules() -> [atom()].
get_plugin_modules() ->
    Plugins1 = config:get("replicator", "auth_plugins", ?DEFAULT_PLUGINS),
    [list_to_atom(Plugin) || Plugin <- string:tokens(Plugins1, ",")].

try_initialize([], _HttpDb) ->
    {error, no_more_auth_plugins_left_to_try};
try_initialize([Mod | Modules], HttpDb) ->
    try Mod:initialize(HttpDb) of
        {ok, HttpDb1, Context} ->
            {ok, Mod, HttpDb1, Context};
        ignore ->
            try_initialize(Modules, HttpDb);
        {error, Error} ->
            {error, Error}
    catch
        error:undef ->
            {error, {could_not_load_plugin_module, Mod}}
    end.
