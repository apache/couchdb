-module(rexi_utils).

-export([recv/6]).

%% @doc set up the receive loop with an overall timeout
-spec recv([any()], integer(), function(), any(), timeout(), timeout()) ->
    {ok, any()} | {timeout, any()} | {error, atom()} | {error, atom(), any()}.
recv(Refs, Keypos, Fun, Acc0, infinity, PerMsgTO) ->
    process_mailbox(Refs, Keypos, Fun, Acc0, nil, PerMsgTO);
recv(Refs, Keypos, Fun, Acc0, GlobalTimeout, PerMsgTO) ->
    TimeoutRef = erlang:make_ref(),
    TRef = erlang:send_after(GlobalTimeout, self(), {timeout, TimeoutRef}),
    try
        process_mailbox(Refs, Keypos, Fun, Acc0, TimeoutRef, PerMsgTO)
    after
        erlang:cancel_timer(TRef)
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
        {timeout, Acc0};
    {Ref, Msg} ->
        case lists:keyfind(Ref, Keypos, RefList) of
        false ->
            % this was some non-matching message which we will ignore
            {ok, Acc0};
        Worker ->
            Fun(Msg, Worker, Acc0)
        end;
    {Ref, From, Msg} ->
        case lists:keyfind(Ref, Keypos, RefList) of
        false ->
            {ok, Acc0};
        Worker ->
            Fun(Msg, {Worker, From}, Acc0)
        end;
    {rexi_DOWN, _, _, _} = Msg ->
        Fun(Msg, nil, Acc0)
    after PerMsgTO ->
        {timeout, Acc0}
    end.
