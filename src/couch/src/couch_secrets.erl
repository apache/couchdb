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
-export([sign/1, sign/2, verify/2, verify/3]).

%% gen_server functions
-export([
    start_link/0,
    init/1,
    handle_call/3,
    handle_cast/2,
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
    case current_secret_from_ets() of
        undefined ->
            throw({internal_server_error, <<"cookie auth secret is not set">>});
        CurrentSecret ->
            FullSecret = <<CurrentSecret/binary, ExtraSecret/binary>>,
            couch_util:hmac(HashAlgorithm, FullSecret, Message)
    end.

verify(Message, ExpectedMAC) ->
    verify(Message, <<>>, ExpectedMAC).

verify(Message, ExtraSecret, ExpectedMAC) ->
    FullSecrets = [<<Secret/binary, ExtraSecret/binary>> || Secret <- all_secrets_from_ets()],
    AllAlgorithms = couch_util:get_config_hash_algorithms(),
    VerifyFun = fun({Secret, Algorithm}) ->
        ActualMAC = couch_util:hmac(Algorithm, Secret, Message),
        case crypto:hash_info(Algorithm) of
            #{size := Size} when Size == byte_size(ExpectedMAC) ->
                crypto:hash_equals(ExpectedMAC, ActualMAC);
            _ ->
                false
        end
    end,
    lists:any(VerifyFun, [{S, A} || S <- FullSecrets, A <- AllAlgorithms]).

start_link() ->
    {Replies, _BadNodes} = gen_server:multi_call(nodes(), ?MODULE, get_secrets),
    gen_server:start_link({local, ?MODULE}, ?MODULE, Replies, []).

init(Secrets) ->
    ets:new(?MODULE, [public, named_table]),
    true = ets:insert(?MODULE, Secrets),
    true = ets:insert(?MODULE, {{node(), current}, current_secret_from_config()}),
    update_all_secrets(),
    erlang:send_after(5000, self(), cache_cleanup),
    ok = config:listen_for_changes(?MODULE, undefined),
    {ok, nil}.

handle_call({insert, Key, Secret}, _From, State) ->
    ets:insert(?MODULE, {Key, Secret}),
    update_all_secrets(),
    {reply, ok, State};
handle_call(get_secrets, _From, State) ->
    Secrets = ets:match_object(?MODULE, {{node(), '_'}, '_'}),
    {reply, Secrets, State};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(restart_config_listener, State) ->
    ok = config:listen_for_changes(?MODULE, nil),
    update_current_secret(),
    {noreply, State};
handle_info(cache_cleanup, State) ->
    erlang:send_after(5000, self(), cache_cleanup),
    Now = os:system_time(second),
    MatchSpec = [{{{'_', '$1'}, '_'}, [{is_integer, '$1'}, {'<', '$1', Now}], [true]}],
    ets:select_delete(?MODULE, MatchSpec),
    update_all_secrets(),
    {noreply, State};
handle_info(_Msg, State) ->
    {noreply, State}.

handle_config_change("chttpd_auth", "secret", _, _, _) ->
    cache_old_secret(),
    update_current_secret(),
    {ok, undefined};
handle_config_change("couch_httpd_auth", "secret", _, _, _) ->
    cache_old_secret(),
    update_current_secret(),
    {ok, undefined};
handle_config_change(_, _, _, _, _) ->
    {ok, undefined}.

handle_config_terminate(_, stop, _) ->
    ok;
handle_config_terminate(_Server, _Reason, _State) ->
    erlang:send_after(3000, whereis(?MODULE), restart_config_listener).

%% private functions

cache_old_secret() ->
    OldSecret = current_secret_from_ets(),
    ExpiresAt =
        erlang:system_time(second) + chttpd_util:get_chttpd_auth_config_integer("timeout", 600),
    true = ets:insert(?MODULE, {{node(), ExpiresAt}, OldSecret}).

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

current_secret_from_ets() ->
    secret_from_ets({node(), current}).

all_secrets_from_ets() ->
    secret_from_ets(all_secrets).

secret_from_ets(Key) ->
    [{Key, Value}] = ets:lookup(?MODULE, Key),
    Value.
