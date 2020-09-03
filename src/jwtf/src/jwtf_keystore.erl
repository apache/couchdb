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

-module(jwtf_keystore).
-behaviour(gen_server).
-behaviour(config_listener).

-include_lib("public_key/include/public_key.hrl").

% public api.
-export([
    get/2,
    start_link/0
]).

% gen_server api.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    code_change/3, terminate/2]).

% config_listener api
-export([handle_config_change/5, handle_config_terminate/3]).

% public functions

get(Alg, undefined) when is_binary(Alg) ->
    get(Alg, <<"_default">>);

get(Alg, KID0) when is_binary(Alg), is_binary(KID0) ->
    Kty = kty(Alg),
    KID = binary_to_list(KID0),
    case ets:lookup(?MODULE, {Kty, KID}) of
        [] ->
            Key = get_from_config(Kty, KID),
            ok = gen_server:call(?MODULE, {set, Kty, KID, Key}),
            Key;
        [{{Kty, KID}, Key}] ->
             Key
    end.


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% gen_server functions

init(_) ->
    ok = config:listen_for_changes(?MODULE, nil),
    ets:new(?MODULE, [public, named_table]),
    {ok, nil}.


handle_call({set, Kty, KID, Key}, _From, State) ->
    true = ets:insert(?MODULE, {{Kty, KID}, Key}),
    {reply, ok, State}.


handle_cast({delete, Kty, KID}, State) ->
    true = ets:delete(?MODULE, {Kty, KID}),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(restart_config_listener, State) ->
    ok = config:listen_for_changes(?MODULE, nil),
    {noreply, State};

handle_info(_Msg, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


% config listener callback

handle_config_change("jwt_keys", ConfigKey, _ConfigValue, _, _) ->
    case string:split(ConfigKey, ":") of
        [Kty, KID] ->
            gen_server:cast(?MODULE, {delete, Kty, KID});
        _ ->
            ignored
    end,
    {ok, nil};

handle_config_change(_, _, _, _, _) ->
    {ok, nil}.

handle_config_terminate(_Server, stop, _State) ->
    ok;

handle_config_terminate(_Server, _Reason, _State) ->
    erlang:send_after(100, whereis(?MODULE), restart_config_listener).

% private functions

get_from_config(Kty, KID) ->
    case config:get("jwt_keys", string:join([Kty, KID], ":")) of
        undefined ->
            throw({bad_request, <<"Unknown kid">>});
        Encoded ->
            case Kty of
                "hmac" ->
                    try
                        base64:decode(Encoded)
                    catch
                        error:_ ->
                            throw({bad_request, <<"Not a valid key">>})
                    end;
                "rsa" ->
                    case pem_decode(Encoded) of
                        #'RSAPublicKey'{} = Key ->
                            Key;
                        _ ->
                            throw({bad_request, <<"not an RSA public key">>})
                    end;
                "ec" ->
                    case pem_decode(Encoded) of
                        {#'ECPoint'{}, _} = Key ->
                            Key;
                        _ ->
                            throw({bad_request, <<"not an EC public key">>})
                    end
            end
    end.

pem_decode(PEM) ->
    BinPEM = re:replace(PEM, "\\\\n", "\n", [global, {return, binary}]),
    try
        case public_key:pem_decode(BinPEM) of
            [PEMEntry] ->
                public_key:pem_entry_decode(PEMEntry);
            _ ->
                throw({bad_request, <<"Not a valid key">>})
        end
   catch
       error:_ ->
           throw({bad_request, <<"Not a valid key">>})
   end.

kty(<<"HS", _/binary>>) ->
    "hmac";

kty(<<"RS", _/binary>>) ->
    "rsa";

kty(<<"ES", _/binary>>) ->
    "ec";

kty(_) ->
    throw({bad_request, <<"Unknown kty">>}).
