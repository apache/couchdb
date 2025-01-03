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

-module(rexi_utils).

-export([server_pid/1, send/2, recv/6]).
-export([add_delta/2, extract_delta/1, get_delta/0]).
-export([maybe_add_delta/1, maybe_add_delta/2]).

%% @doc Return a rexi_server id for the given node.
server_id(Node) ->
    list_to_atom("rexi_server_" ++ atom_to_list(Node)).

%% @doc Return a {server_id(node()), Node} Pid name for the given Node.
server_pid(Node) ->
    {server_id(node()), Node}.

%% @doc send a message as quickly as possible
send(Dest, Msg) ->
    case erlang:send(Dest, Msg, [noconnect, nosuspend]) of
        ok ->
            ok;
        _ ->
            % treat nosuspend and noconnect the same
            rexi_buffer:send(Dest, Msg)
    end.

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
        {new_refs, NewRefList, Acc} ->
            process_mailbox(NewRefList, Keypos, Fun, Acc, TimeoutRef, PerMsgTO);
        {stop, Acc} ->
            {ok, Acc};
        Error ->
            Error
    end.

process_message(RefList, Keypos, Fun, Acc0, TimeoutRef, PerMsgTO) ->
    receive
        Msg ->
            process_raw_message(Msg, RefList, Keypos, Fun, Acc0, TimeoutRef)
    after PerMsgTO ->
        {timeout, Acc0}
    end.

process_raw_message(Payload0, RefList, Keypos, Fun, Acc0, TimeoutRef) ->
    {Payload, Delta} = extract_delta(Payload0),
    couch_stats_resource_tracker:accumulate_delta(Delta),
    case Payload of
        {timeout, TimeoutRef} ->
            {timeout, Acc0};
        {rexi, Ref, Msg} ->
            case lists:keyfind(Ref, Keypos, RefList) of
                false ->
                    {ok, Acc0};
                Worker ->
                    Fun(Msg, Worker, Acc0)
            end;
        {rexi, Ref, From, Msg} ->
            case lists:keyfind(Ref, Keypos, RefList) of
                false ->
                    {ok, Acc0};
                Worker ->
                    Fun(Msg, {Worker, From}, Acc0)
            end;
        {rexi, '$rexi_ping'} ->
            {ok, Acc0};
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
    end.

add_delta({A}, Delta) -> {A, Delta};
add_delta({A, B}, Delta) -> {A, B, Delta};
add_delta({A, B, C}, Delta) -> {A, B, C, Delta};
add_delta({A, B, C, D}, Delta) -> {A, B, C, D, Delta};
add_delta({A, B, C, D, E}, Delta) -> {A, B, C, D, E, Delta};
add_delta({A, B, C, D, E, F}, Delta) -> {A, B, C, D, E, F, Delta};
add_delta({A, B, C, D, E, F, G}, Delta) -> {A, B, C, D, E, F, G, Delta};
add_delta(T, _Delta) -> T.

extract_delta({A, {delta, Delta}}) -> {{A}, Delta};
extract_delta({A, B, {delta, Delta}}) -> {{A, B}, Delta};
extract_delta({A, B, C, {delta, Delta}}) -> {{A, B, C}, Delta};
extract_delta({A, B, C, D, {delta, Delta}}) -> {{A, B, C, D}, Delta};
extract_delta({A, B, C, D, E, {delta, Delta}}) -> {{A, B, C, D, E}, Delta};
extract_delta({A, B, C, D, E, F, {delta, Delta}}) -> {{A, B, C, D, E, F}, Delta};
extract_delta({A, B, C, D, E, F, G, {delta, Delta}}) -> {{A, B, C, D, E, F, G}, Delta};
extract_delta(T) -> {T, undefined}.

get_delta() ->
    {delta, couch_stats_resource_tracker:make_delta()}.

maybe_add_delta(T) ->
    case couch_stats_resource_tracker:is_enabled() of
        false ->
            T;
        true ->
            %% Call add_elta/2 directly instead of maybe_add_delta/2 to avoid
            %% redundant is_enabled check, or the pre-emptive get_delta/0
            add_delta(T, rexi_utils:get_delta())
    end.

%% Allow for externally provided Delta in error handling scenarios
%% eg in cases like rexi_server:notify_caller
maybe_add_delta(T, undefined) ->
    T;
maybe_add_delta(T, Delta) when is_map(Delta) ->
    maybe_add_delta(T, {delta, Delta});
maybe_add_delta(T, {delta, _} = Delta) ->
    case couch_stats_resource_tracker:is_enabled() of
        false ->
            T;
        true ->
            add_delta(T, Delta)
    end.
