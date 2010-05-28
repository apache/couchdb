-module(fabric_util).

-export([submit_jobs/3, recv/4, receive_loop/4, receive_loop/6]).

-include("../../dynomite/include/membership.hrl").
-include_lib("eunit/include/eunit.hrl").

submit_jobs(Shards, EndPoint, ExtraArgs) ->
    lists:map(fun(#shard{node=Node, name=ShardName} = Shard) ->
        io:format("submitting ~p ~p~n", [Node, {fabric_rpc, EndPoint, [ShardName | ExtraArgs]}]),
        Ref = rexi:cast(Node, {fabric_rpc, EndPoint, [ShardName | ExtraArgs]}),
        Shard#shard{ref = Ref}
    end, Shards).

recv(Workers, Keypos, Fun, Acc0) ->
    receive_loop(Workers, Keypos, Fun, Acc0).

receive_loop(Workers, Keypos, Fun, Acc0) ->
    case couch_config:get("fabric", "request_timeout", "10000") of
    "infinity" ->
        Timeout = infinity;
    N ->
        Timeout = list_to_integer(N)
    end,
    receive_loop(Workers, Keypos, Fun, Acc0, Timeout, infinity).

%% @doc set up the receive loop with an overall timeout
-spec receive_loop([any()], integer(), function(), any(), timeout(), timeout()) ->
    {ok, any()}.
receive_loop(RefPartMap, Keypos, Fun, Acc0, GlobalTimeout, PerMsgTO) ->
    TimeoutRef = erlang:make_ref(),
    {ok, TRef} = timer:send_after(GlobalTimeout, {timeout, TimeoutRef}),
    try
        process_mailbox(RefPartMap, Keypos, Fun, Acc0, TimeoutRef, PerMsgTO)
    after
        timer:cancel(TRef)
    end.

process_mailbox(RefList, Keypos, Fun, Acc0, TimeoutRef, PerMsgTO) ->
    case process_message(RefList, Keypos, Fun, Acc0, TimeoutRef, PerMsgTO) of
    {ok, Acc} ->
        process_mailbox(RefList, Keypos, Fun, Acc, TimeoutRef, PerMsgTO);
    {stop, Acc} ->
        {ok, Acc};
    Error ->
        Error
    end.

process_message(RefList, Keypos, Fun, Acc0, TimeoutRef, PerMsgTO) ->
    receive
    {timeout, TimeoutRef} ->
        timeout;
    {Ref, Msg} ->
        io:format("process_message ~p ~p~n", [Ref, Msg]),
        case lists:keyfind(Ref, Keypos, RefList) of
        false ->
            % this was some non-matching message which we will ignore
            {ok, Acc0};
        RefPart ->
            % call the Fun that understands the message
            %?debugFmt("~nAcc0: ~p~n", [Acc0]),
            Fun(RefPart, Msg, Acc0)
        end;
    {rexi_DOWN, _RexiMonPid, ServerPid, Reason} = Msg ->
        showroom_log:message(alert, "rexi_DOWN ~p ~p", [ServerPid, Reason]),
        Fun(nil, Msg, Acc0)
    after PerMsgTO ->
        timeout
    end.
