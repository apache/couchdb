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

% @doc
% This module parses JSON Web Key Sets (JWKS) and caches them for
% performance reasons. To use the module, include it in your
% supervision tree.

-module(jwks).
-behaviour(gen_server).

-export([
    start_link/1,
    get_key/2
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3,
    terminate/2
]).

start_link(JWKSUrl) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, JWKSUrl, []).


get_key(Pid, Kid) ->
    case lookup(Kid) of
        {ok, Key} ->
            couch_stats:increment_counter([jkws, hit]),
            {ok, Key};
        {error, not_found} ->
            couch_stats:increment_counter([jkws, miss]),
            Url = gen_server:call(Pid, get_url),
            case get_keyset(Url) of
                {ok, KeySet} ->
                    ok = gen_server:call(Pid, {replace_keyset, KeySet}),
                    lookup(Kid);
                {error, Reason} ->
                    {error, Reason}
            end
    end.


lookup(Kid) ->
    case ets:lookup(?MODULE, Kid) of
        [{Kid, Key}] ->
            {ok, Key};
        [] ->
            {error, not_found}
    end.



%% gen_server functions

init(Url) ->
    ?MODULE = ets:new(?MODULE, [protected, named_table, {read_concurrency, true}]),
    KeySet = get_keyset(Url),
    set_keyset(KeySet),
    {ok, Url}.


handle_call({replace_keyset, KeySet}, _From, State) ->
    set_keyset(KeySet),
    {reply, ok, State};

handle_call(get_url, _From, State) ->
    {reply, State, State};

handle_call(_Msg, _From, State) ->
    {noreply, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(_Msg, State) ->
    {noreply, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


terminate(_Reason, _State) ->
    ok.

%% private functions

get_keyset(Url) ->
    ReqHeaders = [],
    T0 = os:timestamp(),
    case ibrowse:send_req(Url, ReqHeaders, get) of
        {ok, "200", _RespHeaders, RespBody} ->
            Latency = timer:now_diff(os:timestamp(), T0) / 1000,
            couch_stats:update_histogram([jkws, latency], Latency),
            {ok, parse_keyset(RespBody)};
        _Else ->
            {error, get_keyset_failed}
    end.


set_keyset(KeySet) ->
    true = ets:delete_all_objects(?MODULE),
    true = ets:insert(?MODULE, KeySet).


parse_keyset(Body) ->
    {Props} = jiffy:decode(Body),
    Keys = proplists:get_value(<<"keys">>, Props),
    [parse_key(Key) || Key <- Keys].


parse_key({Props}) ->
    <<"RS256">> = proplists:get_value(<<"alg">>, Props),
    <<"RSA">> = proplists:get_value(<<"kty">>, Props),
    Kid = proplists:get_value(<<"kid">>, Props),
    E = proplists:get_value(<<"e">>, Props),
    N = proplists:get_value(<<"n">>, Props),
    {Kid, {'RSAPublicKey', decode_number(N), decode_number(E)}}.


decode_number(Base64) ->
    crypto:bytes_to_integer(b64url:decode(Base64)).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

jwks_test() ->
    application:start(ibrowse),
    jwks:start_link("https://iam.eu-gb.bluemix.net/oidc/keys"),
    ?assertMatch({ok, _}, jwks:get_key(?MODULE, <<"20170402-00:00:00">>)).

-endif.
