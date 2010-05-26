-module(fabric_util).

-export([receive_loop/6]).

-include("../../dynomite/include/membership.hrl").


%% @doc set up the receive loop with an overall timeout
-spec receive_loop([ref_part_map()], integer(), function(), any(),
                   integer(), integer()) ->
    {ok, beg_acc()}.
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
        case lists:keyfind(Ref, Keypos, RefList) of
        false ->
            % this was some non-matching message which we will ignore
            {ok, Acc0};
        RefPart ->
            % call the Fun that understands the message
            Fun(RefPart, Msg, Acc0)
        end;
    {rexi_DOWN, _RexiMonPid, ServerPid, Reason} = Msg ->
        showroom_log:message(alert, "rexi_DOWN ~p ~p", [ServerPid, Reason]),
        Fun(nil, Msg, Acc0)
    after PerMsgTO ->
        timeout
    end.
