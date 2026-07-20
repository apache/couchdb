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

-behaviour(gen_server).
-behaviour(config_listener).

-export([
    initialize/1,
    update_headers/2,
    handle_response/3,
    cleanup/1
]).

-export([
    start_link/0,
    reconcile/0,
    context/0
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

-export([
    handle_config_change/5,
    handle_config_terminate/3
]).

-include_lib("couch_replicator/include/couch_replicator_api_wrap.hrl").

-type headers() :: [{string(), string()}].
-type code() :: non_neg_integer().

-define(DEFAULT_PLUGINS, "couch_replicator_auth_session,couch_replicator_auth_noop").
-define(RELISTEN_DELAY, 5000).

% Plugin module map to whatever that plugin's sup_initialize/0 returned.
-record(st, {ctx = #{}}).

% Behavior API

% Note for plugin developers: consider using the "auth" field in the source and
% target objects to store credentials. In that case non-owner and non-admin
% users will have those credentials stripped when they read the replication
% document, which mimics the behavior for "headers" and user and pass fields
% in endpoint URLs".

-callback initialize(#httpdb{}) ->
    {ok, #httpdb{}, term()} | {error, term()} | ignore.

-callback update_headers(term(), headers()) -> {headers(), term()}.

-callback handle_response(term(), code(), headers()) ->
    {continue | retry, term()}.

-callback cleanup(term()) -> ok.

% Optional. Node-wide setup and teardown, A plugin that needs state shared by
% every replication job (say, a token ets cache) starts it in sup_initialize/0
% and returns an opaque context. That context is handed back to sup_cleanup/1
% when the plugin is removed from the `[replicator] auth_plugins` config or the
% replicator shuts down.
%
% Anything linked here is linked to the calling process, which is
% couch_replicator_auth gen_server.
%
-callback sup_initialize() -> {ok, term()} | ignore | {error, term()}.

-callback sup_cleanup(term()) -> ok.

-optional_callbacks([sup_initialize/0, sup_cleanup/1]).

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

% Plugin manager to manage the optional sub_initialize/0 contexts.
%

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec reconcile() -> ok.
reconcile() ->
    gen_server:call(?MODULE, reconcile, infinity).

% The plugin modules currently set up.
-spec context() -> [atom()].
context() ->
    gen_server:call(?MODULE, context, infinity).

init([]) ->
    process_flag(trap_exit, true),
    ok = config:listen_for_changes(?MODULE, nil),
    {ok, #st{ctx = plugins_reconcile(#{})}}.

handle_call(reconcile, _From, #st{ctx = Ctx} = St) ->
    {reply, ok, St#st{ctx = plugins_reconcile(Ctx)}};
handle_call(context, _From, #st{ctx = Ctx} = St) ->
    {reply, lists:sort(maps:keys(Ctx)), St};
handle_call(Msg, _From, #st{} = St) ->
    {reply, {error, {invalid_call, Msg}}, St}.

handle_cast(reconcile, #st{ctx = Ctx} = St) ->
    {noreply, St#st{ctx = plugins_reconcile(Ctx)}};
handle_cast(_Msg, #st{} = St) ->
    {noreply, St}.

handle_info(restart_config_listener, #st{} = St) ->
    ok = config:listen_for_changes(?MODULE, nil),
    {noreply, St};
handle_info({'EXIT', Pid, Reason}, #st{} = St) ->
    couch_log:warning("~p: linked process ~p exited: ~p", [?MODULE, Pid, Reason]),
    {noreply, St};
handle_info(_Msg, #st{} = St) ->
    {noreply, St}.

terminate(_Reason, #st{ctx = Ctx}) ->
    _ = lists:foldl(fun plugin_cleanup/2, Ctx, maps:keys(Ctx)),
    ok.

handle_config_change("replicator", "auth_plugins", _V, _Persist, S) ->
    ok = gen_server:cast(?MODULE, reconcile),
    {ok, S};
handle_config_change(_, _, _, _, S) ->
    {ok, S}.

handle_config_terminate(_, stop, _) ->
    ok;
handle_config_terminate(_, _, _) ->
    Pid = whereis(?MODULE),
    erlang:send_after(?RELISTEN_DELAY, Pid, restart_config_listener).

% Private plugin supervisor helper functions

% Start/stop contexts based on `[replicator] auth_plugins` settings.
plugins_reconcile(Ctx) when is_map(Ctx) ->
    Wanted = [Mod || Mod <- get_plugin_modules(), exports_sup_initialize(Mod)],
    Running = maps:keys(Ctx),
    Ctx1 = lists:foldl(fun plugin_cleanup/2, Ctx, Running -- Wanted),
    lists:foldl(fun plugin_initialize/2, Ctx1, Wanted -- Running).

plugin_initialize(Mod, Ctx) ->
    try Mod:sup_initialize() of
        {ok, PluginCtx} ->
            couch_log:info("~p: initialized auth plugin ~p", [?MODULE, Mod]),
            Ctx#{Mod => PluginCtx};
        ignore ->
            Ctx;
        {error, Error} ->
            LogMsg = "~p: auth plugin ~p sup_initialize failed: ~p",
            couch_log:error(LogMsg, [?MODULE, Mod, Error]),
            Ctx
    catch
        Tag:Error:Stack ->
            LogMsg = "~p: auth plugin ~p sup_initialize crashed: ~p:~p ~p",
            couch_log:error(LogMsg, [?MODULE, Mod, Tag, Error, Stack]),
            Ctx
    end.

plugin_cleanup(Mod, Ctx) ->
    case maps:take(Mod, Ctx) of
        {PluginCtx, Ctx1} ->
            % try...catch so if it throw it won't leave a stale entry
            % A stale entry would block subsequent re-initialization
            try Mod:sup_cleanup(PluginCtx) of
                _ -> ok
            catch
                Tag:Error:Stack ->
                    LogMsg = "~p: auth plugin ~p sup_cleanup crashed: ~p:~p ~p",
                    couch_log:error(LogMsg, [?MODULE, Mod, Tag, Error, Stack])
            end,
            couch_log:info("~p: cleaned up auth plugin ~p", [?MODULE, Mod]),
            Ctx1;
        error ->
            Ctx
    end.

exports_sup_initialize(Mod) ->
    % function_exported/3 is false for a module that has not been loaded yet,
    % and a plugin named in config may not exist at all.
    case code:ensure_loaded(Mod) of
        {module, Mod} ->
            erlang:function_exported(Mod, sup_initialize, 0);
        {error, Reason} ->
            LogMsg = "~p: could not load auth plugin module ~p: ~p",
            couch_log:error(LogMsg, [?MODULE, Mod, Reason]),
            false
    end.

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
