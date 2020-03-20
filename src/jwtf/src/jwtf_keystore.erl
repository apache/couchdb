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

get(Alg, undefined) ->
    get(Alg, "_default");

get(Alg, KID) when is_binary(KID) ->
    get(Alg, binary_to_list(KID));

get(Alg, KID) ->
    case ets:lookup(?MODULE, KID) of
        [] ->
            Key = get_from_config(Alg, KID),
            ok = gen_server:call(?MODULE, {set, KID, Key}),
            Key;
        [{KID, Key}] ->
             Key
    end.


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% gen_server functions

init(_) ->
    ok = config:listen_for_changes(?MODULE, nil),
    ets:new(?MODULE, [public, named_table]),
    {ok, nil}.


handle_call({set, KID, Key}, _From, State) ->
    true = ets:insert(?MODULE, {KID, Key}),
    {reply, ok, State}.


handle_cast({delete, KID}, State) ->
    true = ets:delete(?MODULE, KID),
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

handle_config_change("jwt_keys", KID, _Value, _, _) ->
    {ok, gen_server:cast(?MODULE, {delete, KID})};

handle_config_change(_, _, _, _, _) ->
    {ok, nil}.

handle_config_terminate(_Server, stop, _State) ->
    ok;

handle_config_terminate(_Server, _Reason, _State) ->
    erlang:send_after(100, whereis(?MODULE), restart_config_listener).

% private functions

get_from_config(Alg, KID) ->
    case config:get("jwt_keys", KID) of
        undefined ->
            throw({bad_request, <<"Unknown kid">>});
        Key ->
            case jwtf:verification_algorithm(Alg) of
                {hmac, _} ->
                    list_to_binary(Key);
                {public_key, _} ->
                    BinKey = iolist_to_binary(string:replace(Key, "\\n", "\n", all)),
                    [PEMEntry] = public_key:pem_decode(BinKey),
                    public_key:pem_entry_decode(PEMEntry)
            end
    end.
