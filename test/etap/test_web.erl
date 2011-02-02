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

-module(test_web).
-behaviour(gen_server).

-export([start_link/0, loop/1, get_port/0, set_assert/1, check_last/0]).
-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

-define(SERVER, test_web_server).
-define(HANDLER, test_web_handler).

start_link() ->
    gen_server:start({local, ?HANDLER}, ?MODULE, [], []),
    mochiweb_http:start([
        {name, ?SERVER},
        {loop, {?MODULE, loop}},
        {port, 0}
    ]).

loop(Req) ->
    %etap:diag("Handling request: ~p", [Req]),
    case gen_server:call(?HANDLER, {check_request, Req}) of
        {ok, RespInfo} ->
            {ok, Req:respond(RespInfo)};
        {raw, {Status, Headers, BodyChunks}} ->
            Resp = Req:start_response({Status, Headers}),
            lists:foreach(fun(C) -> Resp:send(C) end, BodyChunks),
            erlang:put(mochiweb_request_force_close, true),
            {ok, Resp};
        {chunked, {Status, Headers, BodyChunks}} ->
            Resp = Req:respond({Status, Headers, chunked}),
            timer:sleep(500),
            lists:foreach(fun(C) -> Resp:write_chunk(C) end, BodyChunks),
            Resp:write_chunk([]),
            {ok, Resp};
        {error, Reason} ->
            etap:diag("Error: ~p", [Reason]),
            Body = lists:flatten(io_lib:format("Error: ~p", [Reason])),
            {ok, Req:respond({200, [], Body})}
    end.

get_port() ->
    mochiweb_socket_server:get(?SERVER, port).

set_assert(Fun) ->
    ok = gen_server:call(?HANDLER, {set_assert, Fun}).

check_last() ->
    gen_server:call(?HANDLER, last_status).

init(_) ->
    {ok, nil}.

terminate(_Reason, _State) ->
    ok.

handle_call({check_request, Req}, _From, State) when is_function(State, 1) ->
    Resp2 = case (catch State(Req)) of
        {ok, Resp} -> {reply, {ok, Resp}, was_ok};
        {raw, Resp} -> {reply, {raw, Resp}, was_ok};
        {chunked, Resp} -> {reply, {chunked, Resp}, was_ok};
        Error -> {reply, {error, Error}, not_ok}
    end,
    Req:cleanup(),
    Resp2;
handle_call({check_request, _Req}, _From, _State) ->
    {reply, {error, no_assert_function}, not_ok};
handle_call(last_status, _From, State) when is_atom(State) ->
    {reply, State, nil};
handle_call(last_status, _From, State) ->
    {reply, {error, not_checked}, State};
handle_call({set_assert, Fun}, _From, nil) ->
    {reply, ok, Fun};
handle_call({set_assert, _}, _From, State) ->
    {reply, {error, assert_function_set}, State};
handle_call(Msg, _From, State) ->
    {reply, {ignored, Msg}, State}.

handle_cast(Msg, State) ->
    etap:diag("Ignoring cast message: ~p", [Msg]),
    {noreply, State}.

handle_info(Msg, State) ->
    etap:diag("Ignoring info message: ~p", [Msg]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
