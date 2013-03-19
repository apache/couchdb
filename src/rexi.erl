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
-export([cast/2, cast/3, cast/4, kill/2]).
-export([reply/1, sync_reply/1, sync_reply/2]).
-export([async_server_call/2, async_server_call/3]).
-export([stream/1, stream/2, stream/3, stream_ack/1, stream_ack/2]).

-include_lib("rexi/include/rexi.hrl").

-define(SERVER, rexi_server).

start() ->
    application:start(rexi).

stop() ->
    application:stop(rexi).

restart() ->
    stop(), start().


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
    Msg = cast_msg({doit, {Caller, Ref}, get(nonce), MFA}),
    rexi_utils:send({?SERVER, Node}, Msg),
    Ref.

%% @doc Executes apply(M, F, A) on Node.
%% This version accepts a sync option which uses the erlang:send/2 call
%% directly in process instead of deferring to a spawned process if
%% erlang:send/2 were to block. If the sync option is omitted this call
%% is identical to cast/3.
-spec cast(node(), pid(), {atom(), atom(), list()}, [atom()]) -> reference().
cast(Node, Caller, MFA, Options) ->
    case lists:member(sync, Options) of
        true ->
            Ref = make_ref(),
            Msg = cast_msg({doit, {Caller, Ref}, get(nonce), MFA}),
            erlang:send({?SERVER, Node}, Msg),
            Ref;
        false ->
            cast(Node, Caller, MFA)
    end.

%% @doc Sends an async kill signal to the remote process associated with Ref.
%% No rexi_EXIT message will be sent.
-spec kill(node(), reference()) -> ok.
kill(Node, Ref) ->
    rexi_utils:send({?SERVER, Node}, cast_msg({kill, Ref})),
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
    rexi_utils:send(Server, {'$gen_call', {Caller,Ref}, Request}),
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

%% @equiv stream(Msg, 100, 300000)
stream(Msg) ->
    stream(Msg, 10, 300000).

%% @equiv stream(Msg, Limit, 300000)
stream(Msg, Limit) ->
    stream(Msg, Limit, 300000).

%% @doc convenience function to stream messages to caller while blocking when
%% a specific number of messages are outstanding. Message is of the form
%% {OriginalRef, self(), Reply}, which enables the original caller to ack.
-spec stream(any(), integer(), pos_integer() | infinity) -> any().
stream(Msg, Limit, Timeout) ->
    try maybe_wait(Limit, Timeout) of
        {ok, Count} ->
            put(rexi_unacked, Count+1),
            {Caller, Ref} = get(rexi_from),
            erlang:send(Caller, {Ref, self(), Msg}),
            ok
    catch throw:timeout ->
        timeout
    end.

%% @equiv stream_ack(Client, 1)
stream_ack(Client) ->
    erlang:send(Client, {rexi_ack, 1}).

%% @doc Ack streamed messages
stream_ack(Client, N) ->
    erlang:send(Client, {rexi_ack, N}).

%% internal functions %%

cast_msg(Msg) -> {'$gen_cast', Msg}.

maybe_wait(Limit, Timeout) ->
    case get(rexi_unacked) of
        undefined ->
            {ok, 0};
        Count when Count >= Limit ->
            wait_for_ack(Count, Timeout);
        Count ->
            drain_acks(Count)
    end.

wait_for_ack(Count, Timeout) ->
    receive
        {rexi_ack, N} -> drain_acks(Count-N)
    after Timeout ->
        throw(timeout)
    end.

drain_acks(Count) when Count < 0 ->
    erlang:error(mismatched_rexi_ack);
drain_acks(Count) ->
    receive
        {rexi_ack, N} -> drain_acks(Count-N)
    after 0 ->
        {ok, Count}
    end.
