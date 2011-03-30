% Copyright 2010 Cloudant
% 
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

-module(rexi).
-export([start/0, stop/0, restart/0]).
-export([cast/2, cast/3, kill/2]).
-export([reply/1, sync_reply/1, sync_reply/2]).
-export([async_server_call/2, async_server_call/3]).
-export([get_errors/0, get_last_error/0, set_error_limit/1]).

-include("rexi.hrl").

-define(SERVER, rexi_server).

start() ->
    application:start(rexi).

stop() ->
    application:stop(rexi).

restart() ->
    stop(), start().

-spec get_errors() -> {ok, [#error{}]}.
get_errors() ->
    gen_server:call(?SERVER, get_errors).

-spec get_last_error() -> {ok, #error{}} | {error, empty}.
get_last_error() ->
    gen_server:call(?SERVER, get_last_error).

-spec set_error_limit(pos_integer()) -> ok.
set_error_limit(N) when is_integer(N), N > 0 ->
    gen_server:call(?SERVER, {set_error_limit, N}).

%% @equiv cast(Node, self(), MFA)
-spec cast(node(), {atom(), atom(), list()}) -> reference().
cast(Node, MFA) ->
    cast(Node, self(), MFA).

%% @doc Executes apply(M, F, A) on Node.
%% You might want to use this instead of rpc:cast/4 for two reasons.  First,
%% the Caller pid and the returned reference are inserted into the remote
%% process' dictionary as `rexi_from', so it has a way to communicate with you.
%% Second, the remote process is monitored. If it exits with a Reason other
%% than normal, Caller will receive a message of the form
%% `{Ref, {rexi_EXIT, Reason}}' where Ref is the returned reference.
-spec cast(node(), pid(), {atom(), atom(), list()}) -> reference().
cast(Node, Caller, MFA) ->
    Ref = make_ref(),
    do_send({?SERVER, Node}, cast_msg({doit, {Caller, Ref}, get(nonce), MFA})),
    Ref.

%% @doc Sends an async kill signal to the remote process associated with Ref.
%% No rexi_EXIT message will be sent.
-spec kill(node(), reference()) -> ok.
kill(Node, Ref) ->
    do_send({?SERVER, Node}, cast_msg({kill, Ref})),
    ok.

%% @equiv async_server_call(Server, self(), Request)
-spec async_server_call(pid() | {atom(),node()}, any()) -> reference().
async_server_call(Server, Request) ->
    async_server_call(Server, self(), Request).

%% @doc Sends a properly formatted gen_server:call Request to the Server and
%% returns the reference which the Server will include in its reply.  The
%% function acts more like cast() than call() in that the server process
%% is not monitored.  Clients who want to know if the server is alive should
%% monitor it themselves before calling this function.
-spec async_server_call(pid() | {atom(),node()}, pid(), any()) -> reference().
async_server_call(Server, Caller, Request) ->
    Ref = make_ref(),
    do_send(Server, {'$gen_call', {Caller,Ref}, Request}),
    Ref.

%% @doc convenience function to reply to the original rexi Caller.
-spec reply(any()) -> any().
reply(Reply) ->
    {Caller, Ref} = get(rexi_from),
    erlang:send(Caller, {Ref,Reply}).

%% @equiv sync_reply(Reply, 300000)
sync_reply(Reply) ->
    sync_reply(Reply, 300000).

%% @doc convenience function to reply to caller and wait for response.  Message
%% is of the form {OriginalRef, {self(),reference()}, Reply}, which enables the
%% original caller to respond back.
-spec sync_reply(any(), pos_integer() | infinity) -> any().
sync_reply(Reply, Timeout) ->
    {Caller, Ref} = get(rexi_from),
    Tag = make_ref(),
    erlang:send(Caller, {Ref, {self(),Tag}, Reply}),
    receive {Tag, Response} ->
        Response
    after Timeout ->
        timeout
    end.

%% internal functions %%

cast_msg(Msg) -> {'$gen_cast', Msg}.

% send a message as quickly as possible
do_send(Dest, Msg) ->
    case erlang:send(Dest, Msg, [noconnect, nosuspend]) of
    noconnect ->
        spawn(erlang, send, [Dest, Msg]);
    nosuspend ->
        spawn(erlang, send, [Dest, Msg]);
    ok ->
        ok
    end.
