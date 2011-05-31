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

-module(couch_work_queue).
-behaviour(gen_server).

-include("couch_db.hrl").

% public API
-export([new/1, queue/2, dequeue/1, dequeue/2, close/1]).

% gen_server callbacks
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, code_change/3, handle_info/2]).

-record(q, {
    queue = queue:new(),
    blocked = [],
    max_size,
    max_items,
    items = 0,
    size = 0,
    work_waiters = [],
    close_on_dequeue = false,
    multi_workers = false
}).


new(Options) ->
    gen_server:start_link(couch_work_queue, Options, []).


queue(Wq, Item) when is_binary(Item) ->
    gen_server:call(Wq, {queue, Item, byte_size(Item)}, infinity);
queue(Wq, Item) ->
    gen_server:call(Wq, {queue, Item, ?term_size(Item)}, infinity).


dequeue(Wq) ->
    dequeue(Wq, all).

    
dequeue(Wq, MaxItems) ->
    try
        gen_server:call(Wq, {dequeue, MaxItems}, infinity)
    catch
        _:_ -> closed
    end.


close(Wq) ->
    gen_server:cast(Wq, close).
    

init(Options) ->
    Q = #q{
        max_size = couch_util:get_value(max_size, Options, nil),
        max_items = couch_util:get_value(max_items, Options, nil),
        multi_workers = couch_util:get_value(multi_workers, Options, false)
    },
    {ok, Q}.


terminate(_Reason, #q{work_waiters=Workers}) ->
    lists:foreach(fun({W, _}) -> gen_server:reply(W, closed) end, Workers).

    
handle_call({queue, Item, Size}, From, #q{work_waiters = []} = Q0) ->
    Q = Q0#q{size = Q0#q.size + Size, % increment_queue_size(Q0, Item),
                items = Q0#q.items + 1,
                queue = queue:in({Item, Size}, Q0#q.queue)},
    case (Q#q.size >= Q#q.max_size) orelse
            (Q#q.items >= Q#q.max_items) of
    true ->
        {noreply, Q#q{blocked = [From | Q#q.blocked]}};
    false ->
        {reply, ok, Q}
    end;

handle_call({queue, Item, _}, _From, #q{work_waiters = [{W, _Max} | Rest]} = Q) ->
    gen_server:reply(W, {ok, [Item]}),
    {reply, ok, Q#q{work_waiters = Rest}};

handle_call({dequeue, Max}, From, Q) ->
    #q{work_waiters = Workers, multi_workers = Multi, items = Count} = Q,
    case {Workers, Multi} of
    {[_ | _], false} ->
        exit("Only one caller allowed to wait for this work at a time");
    {[_ | _], true} ->
        {noreply, Q#q{work_waiters=Workers ++ [{From, Max}]}};
    _ ->
        case Count of
        0 ->
            {noreply, Q#q{work_waiters=Workers ++ [{From, Max}]}};
        C when C > 0 ->
            deliver_queue_items(Max, Q)
        end
    end.


deliver_queue_items(Max, Q) ->
    #q{
        queue = Queue,
        items = Count,
        size = Size,
        close_on_dequeue = Close,
        blocked = Blocked
    } = Q,
    case (Max =:= all) orelse (Max >= Count) of
    false ->
        {Items, Size2, Queue2, Blocked2} = dequeue_items(
            Max, Size, Queue, Blocked, []),
        Q2 = Q#q{
            items = Count - Max, size = Size2, blocked = Blocked2, queue = Queue2
        },
        {reply, {ok, Items}, Q2};
    true ->
        lists:foreach(fun(F) -> gen_server:reply(F, ok) end, Blocked),
        Q2 = Q#q{items = 0, size = 0, blocked = [], queue = queue:new()},
        Items = [Item || {Item, _} <- queue:to_list(Queue)],
        case Close of
        false ->
            {reply, {ok, Items}, Q2};
        true ->
            {stop, normal, {ok, Items}, Q2}
        end
    end.


dequeue_items(0, Size, Queue, Blocked, DequeuedAcc) ->
    {lists:reverse(DequeuedAcc), Size, Queue, Blocked};

dequeue_items(NumItems, Size, Queue, Blocked, DequeuedAcc) ->
    {{value, {Item, ItemSize}}, Queue2} = queue:out(Queue),
    case Blocked of
    [] ->
        Blocked2 = Blocked;
    [From | Blocked2] ->
        gen_server:reply(From, ok)
    end,
    dequeue_items(
        NumItems - 1, Size - ItemSize, Queue2, Blocked2, [Item | DequeuedAcc]).
    

handle_cast(close, #q{items = 0} = Q) ->
    {stop, normal, Q};

handle_cast(close, Q) ->
    {noreply, Q#q{close_on_dequeue = true}}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(X, Q) ->
    {stop, X, Q}.
