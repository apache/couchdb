% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License.  You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
% License for the specific language governing permissions and limitations under
% the License.

-module(couch_secrets).

-behaviour(gen_server).
-behaviour(config_listener).

-include_lib("couch/include/couch_db.hrl").

%% public api
-export([sign/1, sign/2, verify/2, verify/3, secret_is_set/0]).

%% gen_server functions
-export([
    start_link/0,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_continue/2,
    handle_info/2
]).

%% config_listener functions
-export([
    handle_config_change/5,
    handle_config_terminate/3
]).

sign(Message) ->
    sign(Message, <<>>).

sign(Message, ExtraSecret) ->
    [HashAlgorithm | _] = couch_util:get_config_hash_algorithms(),
    case current_secret() of
        undefined ->
            throw({internal_server_error, <<"cookie auth secret is not set">>});
        CurrentSecret ->
            FullSecret = <<CurrentSecret/binary, ExtraSecret/binary>>,
            couch_util:hmac(HashAlgorithm, FullSecret, Message)
    end.

verify(Message, ExpectedMAC) ->
    verify(Message, <<>>, ExpectedMAC).

verify(Message, ExtraSecret, ExpectedMAC) ->
    FullSecrets = [<<Secret/binary, ExtraSecret/binary>> || Secret <- all_secrets()],
    AllAlgorithms = couch_util:get_config_hash_algorithms(),
    verify(Message, AllAlgorithms, FullSecrets, ExpectedMAC).

verify(Message, AllAlgorithms, FullSecrets, ExpectedMAC) ->
    Algorithms = lists:filter(
        fun(Algorithm) ->
            #{size := Size} = crypto:hash_info(Algorithm),
            Size == byte_size(ExpectedMAC)
        end,
        AllAlgorithms
    ),
    VerifyFun = fun({Secret, Algorithm}) ->
        ActualMAC = couch_util:hmac(Algorithm, Secret, Message),
        crypto:hash_equals(ExpectedMAC, ActualMAC)
    end,
    lists:any(VerifyFun, [{S, A} || S <- FullSecrets, A <- Algorithms]).

secret_is_set() ->
    current_secret_from_ets() /= undefined.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, nil, []).

init(nil) ->
    ets:new(?MODULE, [named_table, {read_concurrency, true}]),
    true = ets:insert(?MODULE, {{node(), current}, current_secret_from_config()}),
    update_all_secrets(),
    erlang:send_after(5000, self(), cache_cleanup),
    ok = config:listen_for_changes(?MODULE, undefined),
    {ok, nil, {continue, get_secrets}}.

handle_call({insert, {Node, current}, Secret}, _From, State) ->
    case current_secret_from_ets(Node) of
        undefined ->
            ets:insert(?MODULE, [{{Node, current}, Secret}]);
        OldSecret ->
            TimeoutSecs = chttpd_util:get_chttpd_auth_config_integer("timeout", 600),
            ExpiresAt = erlang:system_time(second) + TimeoutSecs,
            ets:insert(?MODULE, [{{Node, current}, Secret}, {{Node, ExpiresAt}, OldSecret}])
    end,
    update_all_secrets(),
    {reply, ok, State};
handle_call({insert, Key, Secret}, _From, State) ->
    ets:insert(?MODULE, {Key, Secret}),
    update_all_secrets(),
    {reply, ok, State};
handle_call(get_secrets, _From, State) ->
    Secrets = ets:match_object(?MODULE, {{node(), '_'}, '_'}),
    {reply, Secrets, State};
handle_call(flush_cache, _From, State) ->
    %% used from tests to prevent spurious failures due to timing
    MatchSpec = [{{{'_', '$1'}, '_'}, [{is_integer, '$1'}], [true]}],
    NumDeleted = ets:select_delete(?MODULE, MatchSpec),
    if
        NumDeleted > 0 -> update_all_secrets();
        true -> ok
    end,
    {reply, NumDeleted, State};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_continue(get_secrets, State) ->
    {Replies, _BadNodes} = gen_server:multi_call(nodes(), ?MODULE, get_secrets),
    {_Nodes, Secrets} = lists:unzip(Replies),
    true = ets:insert(?MODULE, lists:flatten(Secrets)),
    update_all_secrets(),
    {noreply, State}.

handle_info(restart_config_listener, State) ->
    ok = config:listen_for_changes(?MODULE, nil),
    update_current_secret(),
    {noreply, State};
handle_info(cache_cleanup, State) ->
    erlang:send_after(5000, self(), cache_cleanup),
    Now = os:system_time(second),
    MatchSpec = [{{{'_', '$1'}, '_'}, [{is_integer, '$1'}, {'<', '$1', Now}], [true]}],
    NumDeleted = ets:select_delete(?MODULE, MatchSpec),
    if
        NumDeleted > 0 -> update_all_secrets();
        true -> ok
    end,
    {noreply, State};
handle_info(_Msg, State) ->
    {noreply, State}.

handle_config_change("chttpd_auth", "secret", _, _, _) ->
    update_current_secret(),
    {ok, undefined};
handle_config_change("couch_httpd_auth", "secret", _, _, _) ->
    update_current_secret(),
    {ok, undefined};
handle_config_change(_, _, _, _, _) ->
    {ok, undefined}.

handle_config_terminate(_, stop, _) ->
    ok;
handle_config_terminate(_Server, _Reason, _State) ->
    erlang:send_after(3000, whereis(?MODULE), restart_config_listener).

%% private functions

update_current_secret() ->
    NewSecret = current_secret_from_config(),
    spawn(fun() ->
        gen_server:multi_call(nodes(), ?MODULE, {insert, {node(), current}, NewSecret}),
        gen_server:call(?MODULE, {insert, {node(), current}, NewSecret})
    end).

update_all_secrets() ->
    AllSecrets = ets:match_object(?MODULE, {{'_', '_'}, '_'}),
    ets:insert(?MODULE, {all_secrets, lists:usort([V || {_K, V} <- AllSecrets, is_binary(V)])}).

current_secret_from_config() ->
    case chttpd_util:get_chttpd_auth_config("secret") of
        undefined ->
            undefined;
        Secret ->
            ?l2b(Secret)
    end.

current_secret() ->
    case current_secret_from_ets() of
        undefined ->
            current_secret_from_config();
        CurrentSecret ->
            CurrentSecret
    end.

current_secret_from_ets() ->
    current_secret_from_ets(node()).

current_secret_from_ets(Node) ->
    secret_from_ets({Node, current}).

all_secrets() ->
    case all_secrets_from_ets() of
        [] ->
            CurrentSecret = current_secret_from_config(),
            if
                CurrentSecret == undefined -> [];
                true -> [CurrentSecret]
            end;
        AllSecrets ->
            AllSecrets
    end.

all_secrets_from_ets() ->
    secret_from_ets(all_secrets).

secret_from_ets(Key) ->
    case ets:lookup(?MODULE, Key) of
        [{Key, Value}] -> Value;
        [] -> undefined
    end.
