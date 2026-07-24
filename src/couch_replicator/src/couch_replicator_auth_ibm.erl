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

-module(couch_replicator_auth_ibm).

-behaviour(couch_replicator_auth).
-behaviour(gen_server).

-export([
    sup_initialize/0,
    sup_cleanup/1,
    initialize/1,
    update_headers/2,
    handle_response/3,
    cleanup/1
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-include_lib("couch_replicator/include/couch_replicator_api_wrap.hrl").

-define(EARLY_REFRESH_MS, (5 * 60 * 1000)).

-record(entry, {
    api_key_hash,
    ref_count = 0,
    refresh_ref,
    token
}).

%% callbacks

sup_initialize() ->
    {ok, _} = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

sup_cleanup(_) ->
    ok = gen_server:stop(?MODULE).

initialize(#httpdb{} = HttpDb) ->
    case extract_api_key(HttpDb) of
        {ok, APIKey} ->
            case gen_server:call(?MODULE, {register, APIKey}) of
                ok ->
                    {ok, HttpDb, APIKey};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, _} ->
            ignore
    end.

update_headers(APIKey, Headers) when is_list(Headers) ->
    case get_token(APIKey) of
        {ok, Token} ->
            {[{"Authorization", "Bearer " ++ Token} | Headers], APIKey};
        {error, _Reason} ->
            {Headers, APIKey}
    end.

handle_response(APIKey, _StatusCode, _Headers) ->
    {continue, APIKey}.

cleanup(APIKey) ->
    ok = gen_server:cast(?MODULE, {unregister, APIKey}).

get_token(APIKey) ->
    case ets:lookup(?MODULE, hash(APIKey)) of
        [#entry{} = Entry] ->
            {ok, Entry#entry.token};
        [] ->
            {error, no_token}
    end.

%% gen_server callbacks.

init(_) ->
    ?MODULE = ets:new(?MODULE, [protected, {keypos, #entry.api_key_hash}, named_table]),
    {ok, undefined}.

handle_call({register, APIKey}, _From, State) ->
    APIKeyHash = hash(APIKey),
    case ets:lookup(?MODULE, APIKeyHash) of
        [#entry{ref_count = RefCount} = Entry] ->
            ets:insert(?MODULE, Entry#entry{ref_count = RefCount + 1}),
            {reply, ok, State};
        [] ->
            case acquire_token(APIKey) of
                {ok, Token, ExpiresInMs} ->
                    RefreshRef = erlang:send_after(
                        max(0, ExpiresInMs - ?EARLY_REFRESH_MS),
                        self(),
                        {refresh_token, APIKey}
                    ),
                    true = ets:insert_new(?MODULE, #entry{
                        api_key_hash = APIKeyHash,
                        refresh_ref = RefreshRef,
                        token = Token,
                        ref_count = 1
                    }),
                    {reply, ok, State};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end
    end;
handle_call(_Msg, _From, State) ->
    {reply, {error, unexpected_msg}, State}.

handle_cast({unregister, APIKey}, State) ->
    APIKeyHash = hash(APIKey),
    case ets:lookup(?MODULE, APIKeyHash) of
        [] ->
            ok;
        [#entry{ref_count = RefCount} = Entry] when RefCount > 1 ->
            ets:insert(?MODULE, Entry#entry{ref_count = RefCount - 1});
        [#entry{ref_count = 1} = Entry] ->
            erlang:cancel_timer(Entry#entry.refresh_ref),
            ets:delete(?MODULE, APIKeyHash)
    end,
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({refresh_token, APIKey}, State) ->
    APIKeyHash = hash(APIKey),
    case ets:lookup(?MODULE, APIKeyHash) of
        [] ->
            ok;
        [#entry{} = Entry] ->
            case acquire_token(APIKey) of
                {ok, Token, ExpiresInMs} ->
                    RefreshRef = erlang:send_after(
                        max(0, ExpiresInMs - ?EARLY_REFRESH_MS),
                        self(),
                        {refresh_token, APIKey}
                    ),
                    erlang:cancel_timer(Entry#entry.refresh_ref),
                    ets:insert(?MODULE, Entry#entry{
                        refresh_ref = RefreshRef, token = Token
                    });
                {error, Reason} ->
                    ets:delete(?MODULE, APIKeyHash),
                    couch_log:warning("~p: token acquisition failed with ~p", [?MODULE, Reason])
            end
    end,
    {noreply, State};
handle_info(_Msg, State) ->
    {noreply, State}.

%% private functions.

extract_api_key(#httpdb{auth_props = AuthProps}) ->
    case proplists:get_value(<<"ibm">>, AuthProps) of
        {IBMProps} when is_list(IBMProps) ->
            case proplists:get_value(<<"api_key">>, IBMProps) of
                APIKey when is_binary(APIKey), byte_size(APIKey) > 0 ->
                    {ok, APIKey};
                _ ->
                    {error, missing_api_key}
            end;
        _ ->
            {error, missing_api_key}
    end.

acquire_token(APIKey) ->
    Headers = [
        {"User-Agent", ?COUCH_REPLICATOR_USER_AGENT},
        {"content-type", "application/x-www-form-urlencoded"}
    ],
    Body = mochiweb_util:urlencode([
        {"grant_type", "urn:ibm:params:oauth:grant-type:apikey"},
        {"response_type", "cloud_iam"},
        {"apikey", APIKey}
    ]),
    Timeout = token_timeout(),
    Options = [
        {inactivity_timeout, Timeout},
        {is_ssl, true},
        {response_format, binary},
        {ssl_options, [{cacerts, couch_replicator_utils:cacert_get()}]}
    ],
    case ibrowse:send_req(token_url(), Headers, post, Body, Options, Timeout) of
        {ok, "200", _ResponseHeaders, ResponseBody} ->
            decode_iam_response(ResponseBody);
        {ok, StatusCode, _ResponseHeaders, ResponseBody} ->
            case extract_error_message(ResponseBody) of
                {ok, IAMCode, IAMMessage} ->
                    {error, {StatusCode, IAMCode, IAMMessage}};
                {error, Reason} ->
                    {error, {StatusCode, Reason}}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

decode_iam_response(ResponseBody) ->
    try jiffy:decode(ResponseBody, [return_maps]) of
        Decoded when is_map(Decoded) ->
            Token = maps:get(<<"access_token">>, Decoded, undefined),
            ExpiresInSecs = maps:get(<<"expires_in">>, Decoded, undefined),
            case {Token, ExpiresInSecs} of
                {Token, ExpiresInSecs} when is_binary(Token), is_integer(ExpiresInSecs) ->
                    {ok, binary_to_list(Token), ExpiresInSecs * 1000};
                _ ->
                    {error, malformed_iam_response}
            end;
        _ ->
            {error, malformed_iam_response}
    catch
        _:_ ->
            {error, malformed_iam_response}
    end.

extract_error_message(ResponseBody) ->
    try jiffy:decode(ResponseBody, [return_maps]) of
        Decoded when is_map(Decoded) ->
            Code = maps:get(<<"errorCode">>, Decoded, undefined),
            Message = maps:get(<<"errorMessage">>, Decoded, undefined),
            {ok, Code, Message};
        _ ->
            {error, malformed_iam_response}
    catch
        _:_ ->
            {error, malformed_iam_response}
    end.

token_url() ->
    config:get("ibm", "token_url", "https://iam.cloud.ibm.com/identity/token").

token_timeout() ->
    config:get_integer("ibm", "token_timeout", 30000).

hash(Bin) when is_binary(Bin) ->
    crypto:hash(sha256, Bin).
