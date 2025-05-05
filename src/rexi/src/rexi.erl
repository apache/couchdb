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

-export([cast/2, cast/3, cast/4]).
-export([cast_ref/3, cast_ref/4, cast_ref/5]).
-export([kill_all/1]).
-export([reply/1]).
-export([stream_start/1, stream_cancel/1]).
-export([stream_ack/1]).
-export([stream2/1, stream_last/1, stream_last/2]).
-export([ping/0]).
-export([aggregate_server_queue_len/0, aggregate_buffer_queue_len/0]).

%% Like cast(Node, self(), MFA)
-spec cast(node(), {atom(), atom(), list()}) -> reference().
cast(Node, MFA) ->
    cast_ref(make_ref(), Node, self(), MFA).

%% Executes apply(M, F, A) on Node.
%% Works like cast_ref/4 but creates its own ref.
-spec cast(node(), pid(), {atom(), atom(), list()}) -> reference().
cast(Node, Caller, MFA) ->
    cast_ref(make_ref(), Node, Caller, MFA).

%% Executes apply(M, F, A) on Node.
%% Works like cast_ref/5 but creates its own ref.
-spec cast(node(), pid(), {atom(), atom(), list()}, [atom()]) -> reference().
cast(Node, Caller, MFA, Options) ->
    cast_ref(make_ref(), Node, Caller, MFA, Options).

%% Like cast_ref(Ref, Node, self(), MFA)
-spec cast_ref(reference(), node(), {atom(), atom(), list()}) -> reference().
cast_ref(Ref, Node, MFA) when is_reference(Ref) ->
    cast_ref(Ref, Node, self(), MFA).

%% Execute apply(M, F, A) on Node. Remote rexi_server spawns and monitors a
%% worker process to execute this function. Its process dictionary will contain
%% the `rexi_from = {CallerPid, Ref}` tuple so it can communicate with the
%% caller process. If the worker crashes, the remote rexi_server sends the
%% caller a `{Ref, {rexi_EXIT, Reason}}` message.
%%
%% The first argument is a reference. `cast/2,3,4` versions will automatically
%% create it. The reason to create it manually is to hand it over to cleanup
%% process beforehand, to ensure if the caller dies the cleaner process will be
%% able to kill the remote workers. It may be useful when unused workers may
%% live longer consume resources (keep db handles, acces disk, etc).
%%
%%
-spec cast_ref(reference(), node(), pid(), {atom(), atom(), list()}) -> reference().
cast_ref(Ref, Node, Caller, MFA) when is_reference(Ref) ->
    Msg = cast_msg({doit, {Caller, Ref}, get(nonce), MFA}),
    rexi_utils:send(rexi_utils:server_pid(Node), Msg),
    Ref.

%% Executes apply(M, F, A) on Node.
%% This version accepts a sync option which uses the erlang:send/2 call
%% directly in process instead of deferring to a spawned process if
%% erlang:send/2 were to block. If the sync option is omitted this call
%% is identical to cast_ref/4.
%%
-spec cast_ref(reference(), node(), pid(), {atom(), atom(), list()}, [atom()]) -> reference().
cast_ref(Ref, Node, Caller, MFA, Options) when is_reference(Ref) ->
    case lists:member(sync, Options) of
        true ->
            Msg = cast_msg({doit, {Caller, Ref}, get(nonce), MFA}),
            erlang:send(rexi_utils:server_pid(Node), Msg),
            Ref;
        false ->
            cast_ref(Ref, Node, Caller, MFA)
    end.

%% Send async kill signals to the remote processes associated with Refs. It's
%% more efficient to send a larger group of {Node, Ref} tuples as this function
%% will batch refs per node for effiency. Workers killed this way will not send
%% rexi_EXIT messages.
%%
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

%% Convenience function to reply to the original rexi Caller.
%%
-spec reply(any()) -> any().
reply(Reply) ->
    {Caller, Ref} = get(rexi_from),
    erlang:send(Caller, {Ref, Reply}).

%% Private function used by stream2 to initialize the stream. Message is of the
%% form {OriginalRef, {self(),reference()}, Reply}, which enables the
%% coordinator to respond back. The function uses a gen_server From tuple for
%% rexi_from so that the caller may respond using gen_server:reply(From, Msg).
%% (see stream_start/1 and stream_cancel/1 functions).
%%
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

%% Start a worker stream. Coordinator sends this message to the worker to tell
%% it to start streaming, after the worker sent a rexi_STREAM_INIT message.
%%
%% The `From` should be the value provided by the worker in the
%% rexi_STREAM_INIT message.
%%
-spec stream_start({pid(), any()}) -> ok.
stream_start({Pid, _Tag} = From) when is_pid(Pid) ->
    gen_server:reply(From, rexi_STREAM_START).

%% Cancel a worker stream
%%
%% If a coordinator decides that a worker is not going to be part of the
%% response, it should use this function to cancel the worker. The `From`
%% should be the value provided by the worker in the rexi_STREAM_INIT message.
%%
-spec stream_cancel({pid(), any()}) -> ok.
stream_cancel({Pid, _Tag} = From) when is_pid(Pid) ->
    gen_server:reply(From, rexi_STREAM_CANCEL).

%% Like stream2(Msg, 5, 300000)
stream2(Msg) ->
    Limit = config:get_integer("rexi", "stream_limit", 5),
    stream2(Msg, Limit, 300000).

%% Stream messages back to the coordinator. Initializes on first use. Limit
%% the number of unacked messsages to Limit, and throw a timeout error if it
%% doesn't receive an ack in Timeout milliseconds.
%%
%% The general protocol looks like this:
%%
%%  Coordinator                          Worker (one of Q*N usually)
%%  ----------                           --------------------------
%%  cast/2,3,4     -> {doit, ...}     -> rexi_server:
%%                                         spawn_monitor worker process.
%%                                         First time stream2/1 is called it
%%                                         runs init_stream/1.
%%
%%                                        init_stream/1:
%%                 <- rexi_STREAM_INIT <-  sync send, wait for reply
%%
%%  Some workers are told to
%%  continue with rexi_STREAM_START.
%%  Others are told to stop with
%%  rexi_STREAM_CANCEL
%%
%%                 -> rexi_STREAM_START ->
%%                                        Keep calling rexi:stream2/3
%%                                        to stream data to coordinator...
%%
%%              <- Caller ! {Ref, self(), Msg} <-
%%                                        ...
%% Coordinator must acknowledge.
%%                 ->  {rexi_ack, 1}    ->
%%                                        Send last message. No need for ack.
%%                 <-   Caller ! Msg    <-
%%
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

%% Like stream_last(Msg, 300000)
%%
stream_last(Msg) ->
    stream_last(Msg, 300000).

%% Send the last message in a stream. This difference between
%% this and stream is that it uses rexi:reply/1 which doesn't include
%% the worker pid and doesn't wait for a response from the controller.
%%
stream_last(Msg, Timeout) ->
    maybe_init_stream(Timeout),
    rexi:reply(Msg),
    ok.

%% Ack streamed messages. Coordinator must ack stream messages, except
%% the last one sent with stream_last/1,2. Up to Limit message can be in
%% flight un-acked before the workers will stop and wait for an ack.
%%
stream_ack(Client) ->
    erlang:send(Client, {rexi_ack, 1}).

%% Sends a ping message to the coordinator. This is for long running
%% operations on a node that could exceed the rexi timeout
%%
ping() ->
    {Caller, _} = get(rexi_from),
    erlang:send(Caller, {rexi, '$rexi_ping'}).

aggregate_server_queue_len() ->
    rexi_server_mon:aggregate_queue_len(rexi_server).

aggregate_buffer_queue_len() ->
    rexi_server_mon:aggregate_queue_len(rexi_buffer).

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
    error(mismatched_rexi_ack);
drain_acks(Count) ->
    receive
        {rexi_ack, N} -> drain_acks(Count - N)
    after 0 ->
        {ok, Count}
    end.
