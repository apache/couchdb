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

-export([new/2,queue/2,dequeue/1,close/1]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, code_change/3, handle_info/2]).

-record(q, {
    buffer=[],
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
    try gen_server:call(Wq, dequeue, infinity)
    catch
        _:_ -> closed
    end.

close(Wq) ->
    gen_server:cast(Wq, close).
    

init({MaxSize,MaxItems}) ->
    {ok, #q{max_size=MaxSize, max_items=MaxItems}}.

terminate(_Reason, #q{work_waiter=nil}) ->
    ok;
terminate(_Reason, #q{work_waiter=WW}) ->
    gen_server:reply(WW, closed).
    
handle_call({queue, Item}, From, #q{work_waiter=nil}=Q0) ->
    Q = Q0#q{size=Q0#q.size + byte_size(term_to_binary(Item)),
                items=Q0#q.items + 1,
                buffer=[Item | Q0#q.buffer]},
    case (Q#q.size >= Q#q.max_size) orelse
            (Q#q.items >= Q#q.max_items) of
    true ->
        {noreply, Q#q{blocked=[From | Q#q.blocked]}};
    false ->
        {reply, ok, Q}
    end;
handle_call({queue, Item}, _From, #q{work_waiter=WW}=Q) ->
    gen_server:reply(WW, {ok, [Item]}),
    {reply, ok, Q#q{work_waiter=nil}};
handle_call(dequeue, _From, #q{work_waiter=WW}) when WW /= nil ->
    exit("Only one caller allowed to wait for work at a time");
handle_call(dequeue, From, #q{items=0}=Q) ->
    {noreply, Q#q{work_waiter=From}};
handle_call(dequeue, _From, #q{buffer=Buff, max_size=MaxSize,
        max_items=MaxItems, close_on_dequeue=Close}=Q) ->
    [gen_server:reply(From, ok) || From <- Q#q.blocked],
    Q2 = #q{max_size=MaxSize, max_items=MaxItems},
    if Close ->
        {stop, normal, {ok, Buff}, Q2};
    true ->
        {reply, {ok, Buff}, #q{max_size=MaxSize, max_items=MaxItems}}
    end.

handle_cast(close, #q{buffer=[]}=Q) ->
    {stop, normal, Q};
handle_cast(close, Q) ->
    {noreply, Q#q{close_on_dequeue=true}}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(X, Q) ->
    {stop, X, Q}.
