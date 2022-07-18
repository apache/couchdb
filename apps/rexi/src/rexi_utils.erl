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

-export([server_id/1, server_pid/1, send/2, recv/6]).

%% @doc Return a rexi_server id for the given node.
server_id(Node) ->
    case config:get_boolean("rexi", "server_per_node", true) of
        true ->
            list_to_atom("rexi_server_" ++ atom_to_list(Node));
        _ ->
            rexi_server
    end.

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
    after PerMsgTO ->
        {timeout, Acc0}
    end.
