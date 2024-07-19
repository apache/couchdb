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

-export([cast/2, cast/3, cast/4, kill_all/1]).
-export([reply/1]).
-export([stream_start/1, stream_cancel/1]).
-export([stream_ack/1]).
-export([stream2/1, stream_last/1, stream_last/2]).
-export([ping/0]).

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
    rexi_utils:send(rexi_utils:server_pid(Node), Msg),
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
            erlang:send(rexi_utils:server_pid(Node), Msg),
            Ref;
        false ->
            cast(Node, Caller, MFA)
    end.

%% @doc Sends an async kill signal to the remote processes associated with Refs.
%% No rexi_EXIT message will be sent.
-spec kill_all([{node(), reference()}]) -> ok.
kill_all(NodeRefs) when is_list(NodeRefs) ->
    % #{N1 => [Ref1, Ref2], N2 => [Ref3], ...}
    KeyFun = fun({Node, _Ref}) -> Node end,
    ValFun = fun({_Node, Ref}) -> Ref end,
    PerNodeMap = maps:groups_from_list(KeyFun, ValFun, NodeRefs),
    maps:foreach(
        fun(Node, Refs) ->
            ServerPid = rexi_utils:server_pid(Node),
            rexi_utils:send(ServerPid, cast_msg({kill_all, Refs}))
        end,
        PerNodeMap
    ).

%% @doc convenience function to reply to the original rexi Caller.
-spec reply(any()) -> any().
reply(Reply) ->
    {Caller, Ref} = get(rexi_from),
    erlang:send(Caller, {Ref, Reply}).

%% @doc convenience function to reply to caller and wait for response.  Message
%% is of the form {OriginalRef, {self(),reference()}, Reply}, which enables the
%% original caller to respond back.
-spec sync_reply(any(), pos_integer() | infinity) -> any().
sync_reply(Reply, Timeout) ->
    {Caller, Ref} = get(rexi_from),
    Tag = make_ref(),
    erlang:send(Caller, {Ref, {self(), Tag}, Reply}),
    receive
        {Tag, Response} ->
            Response
    after Timeout ->
        timeout
    end.

%% @doc Start a worker stream
%%
%% If a coordinator wants to continue using a streaming worker it
%% should use this function to inform the worker to continue
%% sending messages. The `From` should be the value provided by
%% the worker in the rexi_STREAM_INIT message.
-spec stream_start({pid(), any()}) -> ok.
stream_start({Pid, _Tag} = From) when is_pid(Pid) ->
    gen_server:reply(From, rexi_STREAM_START).

%% @doc Cancel a worker stream
%%
%% If a coordinator decideds that a worker is not going to be part
%% of the response it should use this function to cancel the worker.
%% The `From` should be the value provided by the worker in the
%% rexi_STREAM_INIT message.
-spec stream_cancel({pid(), any()}) -> ok.
stream_cancel({Pid, _Tag} = From) when is_pid(Pid) ->
    gen_server:reply(From, rexi_STREAM_CANCEL).

%% @equiv stream2(Msg, 5, 300000)
stream2(Msg) ->
    Limit = config:get_integer("rexi", "stream_limit", 5),
    stream2(Msg, Limit, 300000).

%% @doc Stream a message back to the coordinator. It limits the
%% number of unacked messsages to Limit and throws a timeout error
%% if it doesn't receive an ack in Timeout milliseconds. This
%% is a combination of the old stream_start and stream functions
%% which automatically does the stream initialization logic.
-spec stream2(any(), pos_integer(), pos_integer() | inifinity) -> any().
stream2(Msg, Limit, Timeout) ->
    maybe_init_stream(Timeout),
    try maybe_wait(Limit, Timeout) of
        {ok, Count} ->
            put(rexi_unacked, Count + 1),
            {Caller, Ref} = get(rexi_from),
            erlang:send(Caller, {Ref, self(), Msg}),
            ok
    catch
        throw:timeout ->
            couch_stats:increment_counter([rexi, streams, timeout, stream]),
            exit(timeout)
    end.

%% @equiv stream_last(Msg, 300000)
stream_last(Msg) ->
    stream_last(Msg, 300000).

%% @doc Send the last message in a stream. This difference between
%% this and stream is that it uses rexi:reply/1 which doesn't include
%% the worker pid and doesn't wait for a response from the controller.
stream_last(Msg, Timeout) ->
    maybe_init_stream(Timeout),
    rexi:reply(Msg),
    ok.

%% @doc Ack streamed messages
stream_ack(Client) ->
    erlang:send(Client, {rexi_ack, 1}).

%% Sends a ping message to the coordinator. This is for long running
%% operations on a node that could exceed the rexi timeout
ping() ->
    {Caller, _} = get(rexi_from),
    erlang:send(Caller, {rexi, '$rexi_ping'}).

%% internal functions %%

cast_msg(Msg) -> {'$gen_cast', Msg}.

maybe_init_stream(Timeout) ->
    case get(rexi_STREAM_INITED) of
        true ->
            ok;
        _ ->
            init_stream(Timeout)
    end.

init_stream(Timeout) ->
    case sync_reply(rexi_STREAM_INIT, Timeout) of
        rexi_STREAM_START ->
            put(rexi_STREAM_INITED, true),
            ok;
        rexi_STREAM_CANCEL ->
            exit(normal);
        timeout ->
            couch_stats:increment_counter([rexi, streams, timeout, init_stream]),
            exit(timeout);
        Else ->
            exit({invalid_stream_message, Else})
    end.

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
        {rexi_ack, N} -> drain_acks(Count - N)
    after Timeout ->
        couch_stats:increment_counter([rexi, streams, timeout, wait_for_ack]),
        throw(timeout)
    end.

drain_acks(Count) when Count < 0 ->
    erlang:error(mismatched_rexi_ack);
drain_acks(Count) ->
    receive
        {rexi_ack, N} -> drain_acks(Count - N)
    after 0 ->
        {ok, Count}
    end.
