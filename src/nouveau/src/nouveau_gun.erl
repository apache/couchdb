%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-

%% index manager ensures only one process is updating a nouveau index at a time.
%% calling update_index will block until at least one attempt has been made to
%% make the index as current as the database at the time update_index was called.

-module(nouveau_gun).
-behaviour(gen_server).
-behaviour(config_listener).

-export([start_link/0]).
-export([host_header/0]).

%%% gen_server callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([handle_continue/2]).

% config_listener callbacks
-export([handle_config_change/5]).
-export([handle_config_terminate/3]).

-define(NOUVEAU_HOST_HEADER, nouveau_host_header).

-record(state, {
    enabled,
    url
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

host_header() ->
    persistent_term:get(?NOUVEAU_HOST_HEADER).

init(_) ->
    ok = config:listen_for_changes(?MODULE, nil),
    State = #state{enabled = false, url = nouveau_util:nouveau_url()},
    {ok, State, {continue, reconfigure}}.

handle_call(_Msg, _From, State) ->
    {reply, unexpected_msg, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(restart_config_listener, State) ->
    ok = config:listen_for_changes(?MODULE, nil),
    {noreply, State};
handle_info(reconfigure, State) ->
    reconfigure(new_state(), State);
handle_info(Msg, State) ->
    couch_log:warning("~p received unexpected message: ~p", [?MODULE, Msg]),
    {noreply, State}.

handle_continue(reconfigure, State) ->
    reconfigure(new_state(), State).

handle_config_change("nouveau", "enable", _Value, _Persist, nil) ->
    whereis(?MODULE) ! reconfigure,
    {ok, nil};
handle_config_change("nouveau", "url", _Value, _Persist, nil) ->
    whereis(?MODULE) ! reconfigure,
    {ok, nil};
handle_config_change(_Section, _Key, _Value, _Persist, nil) ->
    {ok, nil}.

handle_config_terminate(_Server, stop, nil) ->
    ok;
handle_config_terminate(_Server, _Reason, nil) ->
    erlang:send_after(
        500,
        whereis(?MODULE),
        restart_config_listener
    ).

%% private functions

new_state() ->
    #state{enabled = nouveau:enabled(), url = nouveau_util:nouveau_url()}.

reconfigure(#state{} = State, #state{} = State) ->
    %% no change
    {noreply, State};
reconfigure(#state{enabled = false} = NewState, #state{enabled = true} = CurrState) ->
    %% turning off
    stop_gun(CurrState#state.url),
    {noreply, NewState};
reconfigure(#state{enabled = true} = NewState, #state{enabled = false}) ->
    %% turning on
    case start_gun(NewState#state.url) of
        {ok, _PoolPid} ->
            {noreply, NewState};
        {error, Reason} ->
            {stop, Reason}
    end;
reconfigure(#state{enabled = true} = NewState, #state{enabled = true} = CurrState) when
    NewState#state.url /= CurrState#state.url
->
    %% changing url while on
    stop_gun(CurrState#state.url),
    reconfigure(NewState, CurrState#state{enabled = false});
reconfigure(#state{enabled = false} = NewState, #state{enabled = false} = CurrState) when
    NewState#state.url /= CurrState#state.url
->
    %% changing url while off
    {noreply, NewState}.

start_gun(URL) ->
    #{host := Host, port := Port, scheme := Scheme} = uri_string:parse(URL),
    persistent_term:put(?NOUVEAU_HOST_HEADER, {<<"host">>, [Host, $:, integer_to_binary(Port)]}),
    PoolSize = config:get_integer("nouveau", "pool_size", 10),
    CACertFile = config:get("nouveau", "ssl_cacert_file"),
    KeyFile = config:get("nouveau", "ssl_key_file"),
    CertFile = config:get("nouveau", "ssl_cert_file"),
    Password = config:get("nouveau", "ssl_password"),
    Transport = scheme_to_transport(Scheme),
    BaseConnOptions = #{transport => Transport, protocols => [http2]},
    ConnOptions =
        if
            Transport == tls andalso KeyFile /= undefined andalso CertFile /= undefined ->
                CertKeyConf0 = #{
                    certfile => CertFile,
                    keyfile => KeyFile,
                    password => Password,
                    cacertfile => CACertFile
                },
                CertKeyConf1 = maps:filter(fun remove_undefined/2, CertKeyConf0),
                BaseConnOptions#{
                    tls_opts => [{certs_keys, [CertKeyConf1]}]
                };
            true ->
                BaseConnOptions
        end,
    gun_pool:start_pool(Host, Port, #{size => PoolSize, conn_opts => ConnOptions}).

stop_gun(URL) ->
    #{host := Host, port := Port, scheme := Scheme} = uri_string:parse(URL),
    gun_pool:stop_pool(Host, Port, #{transport => scheme_to_transport(Scheme)}).

remove_undefined(_Key, Value) ->
    Value /= undefined.

scheme_to_transport("http") ->
    tcp;
scheme_to_transport("https") ->
    tls.
