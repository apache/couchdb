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

-export([new/2,queue/2,dequeue/1,dequeue/2,close/1]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, code_change/3, handle_info/2]).

-record(q, {
    queue=queue:new(),
    blocked=[],
    max_size,
    max_items,
    items=0,
    size=0,
    work_waiter=nil,
    close_on_dequeue=false
}).

new(MaxSize, MaxItems) ->
    gen_server:start_link(couch_work_queue, {MaxSize, MaxItems}, []).

queue(Wq, Item) ->
    gen_server:call(Wq, {queue, Item}, infinity).

dequeue(Wq) ->
    dequeue(Wq, all).
    
dequeue(Wq, MaxItems) ->
    try gen_server:call(Wq, {dequeue, MaxItems}, infinity)
    catch
        _:_ -> closed
    end.

close(Wq) ->
    gen_server:cast(Wq, close).
    

init({MaxSize,MaxItems}) ->
    {ok, #q{max_size=MaxSize, max_items=MaxItems}}.

terminate(_Reason, #q{work_waiter=nil}) ->
    ok;
terminate(_Reason, #q{work_waiter={WWFrom, _}}) ->
    gen_server:reply(WWFrom, closed).
    
handle_call({queue, Item}, From, #q{work_waiter=nil}=Q0) ->
    Q = Q0#q{size=Q0#q.size + byte_size(term_to_binary(Item)),
                items=Q0#q.items + 1,
                queue=queue:in(Item, Q0#q.queue)},
    case (Q#q.size >= Q#q.max_size) orelse
            (Q#q.items >= Q#q.max_items) of
    true ->
        {noreply, Q#q{blocked=[From | Q#q.blocked]}};
    false ->
        {reply, ok, Q}
    end;
handle_call({queue, Item}, _From, #q{work_waiter={WWFrom, _Max}}=Q) ->
    gen_server:reply(WWFrom, {ok, [Item]}),
    {reply, ok, Q#q{work_waiter=nil}};
handle_call({dequeue, _Max}, _From, #q{work_waiter=WW}) when WW /= nil ->
    exit("Only one caller allowed to wait for work at a time");
handle_call({dequeue, Max}, From, #q{items=0}=Q) ->
    {noreply, Q#q{work_waiter={From, Max}}};
handle_call({dequeue, Max}, _From, #q{queue=Queue, max_size=MaxSize,
        max_items=MaxItems, items=Items,close_on_dequeue=Close}=Q) ->
    if Max >= Items orelse Max == all ->
        [gen_server:reply(From, ok) || From <- Q#q.blocked],
        Q2 = #q{max_size=MaxSize, max_items=MaxItems},
        if Close ->
            {stop, normal, {ok, queue:to_list(Queue)}, Q2};
        true ->
            {reply, {ok, queue:to_list(Queue)}, Q2}
        end;
    true ->
        {DequeuedItems, Queue2, Blocked2} =
                dequeue_items(Max, Queue, Q#q.blocked, []),
        {reply, {ok, DequeuedItems},
                Q#q{items=Items-Max,blocked=Blocked2,queue=Queue2}}
    end.

dequeue_items(0, Queue, Blocked, DequeuedAcc) ->
    {lists:reverse(DequeuedAcc), Queue, Blocked};
dequeue_items(NumItems, Queue, Blocked, DequeuedAcc) ->
    {{value, Item}, Queue2} = queue:out(Queue),
    case Blocked of
    [] ->
        Blocked2 = Blocked;
    [From|Blocked2] ->
        gen_server:reply(From, ok)
    end,
    dequeue_items(NumItems-1, Queue2, Blocked2, [Item | DequeuedAcc]).
    

handle_cast(close, #q{items=0}=Q) ->
    {stop, normal, Q};
handle_cast(close, Q) ->
    {noreply, Q#q{close_on_dequeue=true}}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(X, Q) ->
    {stop, X, Q}.
