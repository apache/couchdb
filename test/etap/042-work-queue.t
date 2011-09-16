#!/usr/bin/env escript
%% -*- erlang -*-
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

main(_) ->
    test_util:init_code_path(),

    etap:plan(155),
    case (catch test()) of
        ok ->
            etap:end_tests();
        Other ->
            etap:diag(io_lib:format("Test died abnormally: ~p", [Other])),
            etap:bail(Other)
    end,
    ok.


test() ->
    ok = crypto:start(),
    test_single_consumer_max_item_count(),
    test_single_consumer_max_size(),
    test_single_consumer_max_item_count_and_size(),
    test_multiple_consumers(),
    ok.


test_single_consumer_max_item_count() ->
    etap:diag("Spawning a queue with 3 max items, 1 producer and 1 consumer"),

    {ok, Q} = couch_work_queue:new([{max_items, 3}]),
    Producer = spawn_producer(Q),
    Consumer = spawn_consumer(Q),

    etap:is(couch_work_queue:item_count(Q), 0, "Queue item count is 0"),

    consume(Consumer, 1),
    etap:is(ping(Consumer), timeout,
        "Consumer blocked when attempting to dequeue 1 item from empty queue"),

    Item1 = produce(Producer, 10),
    etap:is(ping(Producer), ok, "Producer not blocked"),

    etap:is(ping(Consumer), ok, "Consumer unblocked"),
    etap:is(last_consumer_items(Consumer), {ok, [Item1]},
        "Consumer received the right item"),

    Item2 = produce(Producer, 20),
    etap:is(ping(Producer), ok, "Producer not blocked with non full queue"),
    etap:is(couch_work_queue:item_count(Q), 1, "Queue item count is 1"),

    Item3 = produce(Producer, 15),
    etap:is(ping(Producer), ok, "Producer not blocked with non full queue"),
    etap:is(couch_work_queue:item_count(Q), 2, "Queue item count is 2"),

    Item4 = produce(Producer, 3),
    etap:is(couch_work_queue:item_count(Q), 3, "Queue item count is 3"),
    etap:is(ping(Producer), timeout, "Producer blocked with full queue"),

    consume(Consumer, 2),
    etap:is(ping(Consumer), ok,
        "Consumer not blocked when attempting to dequeue 2 items from queue"),
    etap:is(last_consumer_items(Consumer), {ok, [Item2, Item3]},
        "Consumer received the right items"),
    etap:is(couch_work_queue:item_count(Q), 1, "Queue item count is 1"),

    consume(Consumer, 2),
    etap:is(ping(Consumer), ok,
        "Consumer not blocked when attempting to dequeue 2 items from queue"),
    etap:is(last_consumer_items(Consumer), {ok, [Item4]},
        "Consumer received the right item"),
    etap:is(couch_work_queue:item_count(Q), 0, "Queue item count is 0"),

    consume(Consumer, 100),
    etap:is(ping(Consumer), timeout,
        "Consumer blocked when attempting to dequeue 100 items from empty queue"),
    etap:is(couch_work_queue:item_count(Q), 0, "Queue item count is 0"),

    Item5 = produce(Producer, 11),
    etap:is(ping(Producer), ok, "Producer not blocked with empty queue"),
    etap:is(couch_work_queue:item_count(Q), 0, "Queue item count is 0"),

    Item6 = produce(Producer, 19),
    etap:is(ping(Producer), ok, "Producer not blocked with non full queue"),
    etap:is(couch_work_queue:item_count(Q), 1, "Queue item count is 1"),

    Item7 = produce(Producer, 2),
    etap:is(ping(Producer), ok, "Producer not blocked with non full queue"),
    etap:is(couch_work_queue:item_count(Q), 2, "Queue item count is 2"),

    Item8 = produce(Producer, 33),
    etap:is(ping(Producer), timeout, "Producer blocked with full queue"),
    etap:is(couch_work_queue:item_count(Q), 3, "Queue item count is 3"),

    etap:is(ping(Consumer), ok, "Consumer unblocked"),
    etap:is(last_consumer_items(Consumer), {ok, [Item5]},
        "Consumer received the first queued item"),
    etap:is(couch_work_queue:item_count(Q), 3, "Queue item count is 3"),

    consume(Consumer, all),
    etap:is(ping(Consumer), ok,
        "Consumer not blocked when attempting to dequeue all items from queue"),
    etap:is(last_consumer_items(Consumer), {ok, [Item6, Item7, Item8]},
        "Consumer received all queued items"),

    etap:is(couch_work_queue:item_count(Q), 0, "Queue item count is 0"),

    etap:is(close_queue(Q), ok, "Closed queue"),
    consume(Consumer, 1),
    etap:is(last_consumer_items(Consumer), closed, "Consumer got closed queue"),
    etap:is(couch_work_queue:item_count(Q), closed, "Queue closed"),
    etap:is(couch_work_queue:size(Q), closed, "Queue closed"),

    stop(Producer, "producer"),
    stop(Consumer, "consumer").



test_single_consumer_max_size() ->
    etap:diag("Spawning a queue with max size of 160 bytes, "
        "1 producer and 1 consumer"),

    {ok, Q} = couch_work_queue:new([{max_size, 160}]),
    Producer = spawn_producer(Q),
    Consumer = spawn_consumer(Q),

    etap:is(couch_work_queue:item_count(Q), 0, "Queue item count is 0"),
    etap:is(couch_work_queue:size(Q), 0, "Queue size is 0 bytes"),

    consume(Consumer, 1),
    etap:is(ping(Consumer), timeout,
        "Consumer blocked when attempting to dequeue 1 item from empty queue"),

    Item1 = produce(Producer, 50),
    etap:is(ping(Producer), ok, "Producer not blocked"),

    etap:is(ping(Consumer), ok, "Consumer unblocked"),
    etap:is(last_consumer_items(Consumer), {ok, [Item1]},
        "Consumer received the right item"),

    etap:is(couch_work_queue:item_count(Q), 0, "Queue item count is 0"),
    etap:is(couch_work_queue:size(Q), 0, "Queue size is 0 bytes"),

    Item2 = produce(Producer, 50),
    etap:is(ping(Producer), ok, "Producer not blocked"),
    etap:is(couch_work_queue:item_count(Q), 1, "Queue item count is 1"),
    etap:is(couch_work_queue:size(Q), 50, "Queue size is 50 bytes"),

    Item3 = produce(Producer, 50),
    etap:is(ping(Producer), ok, "Producer not blocked"),
    etap:is(couch_work_queue:item_count(Q), 2, "Queue item count is 2"),
    etap:is(couch_work_queue:size(Q), 100, "Queue size is 100 bytes"),

    Item4 = produce(Producer, 61),
    etap:is(ping(Producer), timeout, "Producer blocked"),
    etap:is(couch_work_queue:item_count(Q), 3, "Queue item count is 3"),
    etap:is(couch_work_queue:size(Q), 161, "Queue size is 161 bytes"),

    consume(Consumer, 1),
    etap:is(ping(Consumer), ok,
        "Consumer not blocked when attempting to dequeue 1 item from full queue"),
    etap:is(last_consumer_items(Consumer), {ok, [Item2]},
        "Consumer received the right item"),
    etap:is(couch_work_queue:item_count(Q), 2, "Queue item count is 2"),
    etap:is(couch_work_queue:size(Q), 111, "Queue size is 111 bytes"),

    Item5 = produce(Producer, 20),
    etap:is(ping(Producer), ok, "Producer not blocked"),
    etap:is(couch_work_queue:item_count(Q), 3, "Queue item count is 3"),
    etap:is(couch_work_queue:size(Q), 131, "Queue size is 131 bytes"),

    Item6 = produce(Producer, 40),
    etap:is(ping(Producer), timeout, "Producer blocked"),
    etap:is(couch_work_queue:item_count(Q), 4, "Queue item count is 4"),
    etap:is(couch_work_queue:size(Q), 171, "Queue size is 171 bytes"),

    etap:is(close_queue(Q), timeout,
        "Timeout when trying to close non-empty queue"),

    consume(Consumer, 2),
    etap:is(ping(Consumer), ok,
        "Consumer not blocked when attempting to dequeue 2 items from full queue"),
    etap:is(last_consumer_items(Consumer), {ok, [Item3, Item4]},
        "Consumer received the right items"),
    etap:is(couch_work_queue:item_count(Q), 2, "Queue item count is 2"),
    etap:is(couch_work_queue:size(Q), 60, "Queue size is 60 bytes"),

    etap:is(close_queue(Q), timeout,
        "Timeout when trying to close non-empty queue"),

    consume(Consumer, all),
    etap:is(ping(Consumer), ok,
        "Consumer not blocked when attempting to dequeue all items from queue"),
    etap:is(last_consumer_items(Consumer), {ok, [Item5, Item6]},
        "Consumer received the right items"),

    etap:is(couch_work_queue:item_count(Q), closed, "Queue closed"),
    etap:is(couch_work_queue:size(Q), closed, "Queue closed"),

    consume(Consumer, all),
    etap:is(last_consumer_items(Consumer), closed, "Consumer got closed queue"),

    stop(Producer, "producer"),
    stop(Consumer, "consumer").


test_single_consumer_max_item_count_and_size() ->
    etap:diag("Spawning a queue with 3 max items, max size of 200 bytes, "
        "1 producer and 1 consumer"),

    {ok, Q} = couch_work_queue:new([{max_items, 3}, {max_size, 200}]),
    Producer = spawn_producer(Q),
    Consumer = spawn_consumer(Q),

    etap:is(couch_work_queue:item_count(Q), 0, "Queue item count is 0"),
    etap:is(couch_work_queue:size(Q), 0, "Queue size is 0 bytes"),

    Item1 = produce(Producer, 100),
    etap:is(ping(Producer), ok, "Producer not blocked"),
    etap:is(couch_work_queue:item_count(Q), 1, "Queue item count is 1"),
    etap:is(couch_work_queue:size(Q), 100, "Queue size is 100 bytes"),

    Item2 = produce(Producer, 110),
    etap:is(ping(Producer), timeout,
        "Producer blocked when queue size >= max_size"),
    etap:is(couch_work_queue:item_count(Q), 2, "Queue item count is 2"),
    etap:is(couch_work_queue:size(Q), 210, "Queue size is 210 bytes"),

    consume(Consumer, all),
    etap:is(ping(Consumer), ok,
        "Consumer not blocked when attempting to dequeue all items from queue"),
    etap:is(last_consumer_items(Consumer), {ok, [Item1, Item2]},
        "Consumer received the right items"),
    etap:is(couch_work_queue:item_count(Q), 0, "Queue item count is 0"),
    etap:is(couch_work_queue:size(Q), 0, "Queue size is 0 bytes"),

    etap:is(ping(Producer), ok, "Producer not blocked anymore"),

    Item3 = produce(Producer, 10),
    etap:is(ping(Producer), ok, "Producer not blocked"),
    etap:is(couch_work_queue:item_count(Q), 1, "Queue item count is 1"),
    etap:is(couch_work_queue:size(Q), 10, "Queue size is 10 bytes"),

    Item4 = produce(Producer, 4),
    etap:is(ping(Producer), ok, "Producer not blocked"),
    etap:is(couch_work_queue:item_count(Q), 2, "Queue item count is 2"),
    etap:is(couch_work_queue:size(Q), 14, "Queue size is 14 bytes"),

    Item5 = produce(Producer, 2),
    etap:is(ping(Producer), timeout,
        "Producer blocked when queue item count = max_items"),
    etap:is(couch_work_queue:item_count(Q), 3, "Queue item count is 3"),
    etap:is(couch_work_queue:size(Q), 16, "Queue size is 16 bytes"),

    consume(Consumer, 1),
    etap:is(ping(Consumer), ok,
        "Consumer not blocked when attempting to dequeue 1 item from queue"),
    etap:is(last_consumer_items(Consumer), {ok, [Item3]},
       "Consumer received 1 item"),
    etap:is(couch_work_queue:item_count(Q), 2, "Queue item count is 2"),
    etap:is(couch_work_queue:size(Q), 6, "Queue size is 6 bytes"),

    etap:is(close_queue(Q), timeout,
        "Timeout when trying to close non-empty queue"),

    consume(Consumer, 1),
    etap:is(ping(Consumer), ok,
        "Consumer not blocked when attempting to dequeue 1 item from queue"),
    etap:is(last_consumer_items(Consumer), {ok, [Item4]},
       "Consumer received 1 item"),
    etap:is(couch_work_queue:item_count(Q), 1, "Queue item count is 1"),
    etap:is(couch_work_queue:size(Q), 2, "Queue size is 2 bytes"),

    Item6 = produce(Producer, 50),
    etap:is(ping(Producer), ok,
        "Producer not blocked when queue is not full and already received"
        " a close request"),
    etap:is(couch_work_queue:item_count(Q), 2, "Queue item count is 2"),
    etap:is(couch_work_queue:size(Q), 52, "Queue size is 52 bytes"),

    consume(Consumer, all),
    etap:is(ping(Consumer), ok,
        "Consumer not blocked when attempting to dequeue all items from queue"),
    etap:is(last_consumer_items(Consumer), {ok, [Item5, Item6]},
       "Consumer received all queued items"),

    etap:is(couch_work_queue:item_count(Q), closed, "Queue closed"),
    etap:is(couch_work_queue:size(Q), closed, "Queue closed"),

    consume(Consumer, 1),
    etap:is(last_consumer_items(Consumer), closed, "Consumer got closed queue"),

    stop(Producer, "producer"),
    stop(Consumer, "consumer").


test_multiple_consumers() ->
    etap:diag("Spawning a queue with 3 max items, max size of 200 bytes, "
        "1 producer and 3 consumers"),

    {ok, Q} = couch_work_queue:new(
        [{max_items, 3}, {max_size, 200}, {multi_workers, true}]),
    Producer = spawn_producer(Q),
    Consumer1 = spawn_consumer(Q),
    Consumer2 = spawn_consumer(Q),
    Consumer3 = spawn_consumer(Q),

    etap:is(couch_work_queue:item_count(Q), 0, "Queue item count is 0"),
    etap:is(couch_work_queue:size(Q), 0, "Queue size is 0 bytes"),

    consume(Consumer1, 1),
    etap:is(ping(Consumer1), timeout,
        "Consumer 1 blocked when attempting to dequeue 1 item from empty queue"),
    consume(Consumer2, 2),
    etap:is(ping(Consumer2), timeout,
        "Consumer 2 blocked when attempting to dequeue 2 items from empty queue"),
    consume(Consumer3, 1),
    etap:is(ping(Consumer3), timeout,
        "Consumer 3 blocked when attempting to dequeue 1 item from empty queue"),

    Item1 = produce(Producer, 50),
    etap:is(ping(Producer), ok, "Producer not blocked"),
    etap:is(couch_work_queue:item_count(Q), 0, "Queue item count is 0"),
    etap:is(couch_work_queue:size(Q), 0, "Queue size is 0 bytes"),

    Item2 = produce(Producer, 50),
    etap:is(ping(Producer), ok, "Producer not blocked"),
    etap:is(couch_work_queue:item_count(Q), 0, "Queue item count is 0"),
    etap:is(couch_work_queue:size(Q), 0, "Queue size is 0 bytes"),

    Item3 = produce(Producer, 50),
    etap:is(ping(Producer), ok, "Producer not blocked"),
    etap:is(couch_work_queue:item_count(Q), 0, "Queue item count is 0"),
    etap:is(couch_work_queue:size(Q), 0, "Queue size is 0 bytes"),

    etap:is(ping(Consumer1), ok, "Consumer 1 unblocked"),
    etap:is(last_consumer_items(Consumer1), {ok, [Item1]},
       "Consumer 1 received 1 item"),
    etap:is(couch_work_queue:item_count(Q), 0, "Queue item count is 0"),
    etap:is(couch_work_queue:size(Q), 0, "Queue size is 0 bytes"),

    etap:is(ping(Consumer2), ok, "Consumer 2 unblocked"),
    etap:is(last_consumer_items(Consumer2), {ok, [Item2]},
       "Consumer 2 received 1 item"),
    etap:is(couch_work_queue:item_count(Q), 0, "Queue item count is 0"),
    etap:is(couch_work_queue:size(Q), 0, "Queue size is 0 bytes"),

    etap:is(ping(Consumer3), ok, "Consumer 3 unblocked"),
    etap:is(last_consumer_items(Consumer3), {ok, [Item3]},
       "Consumer 3 received 1 item"),
    etap:is(couch_work_queue:item_count(Q), 0, "Queue item count is 0"),
    etap:is(couch_work_queue:size(Q), 0, "Queue size is 0 bytes"),

    consume(Consumer1, 1),
    etap:is(ping(Consumer1), timeout,
        "Consumer 1 blocked when attempting to dequeue 1 item from empty queue"),
    consume(Consumer2, 2),
    etap:is(ping(Consumer2), timeout,
        "Consumer 2 blocked when attempting to dequeue 1 item from empty queue"),
    consume(Consumer3, 1),
    etap:is(ping(Consumer3), timeout,
        "Consumer 3 blocked when attempting to dequeue 1 item from empty queue"),

    Item4 = produce(Producer, 50),
    etap:is(ping(Producer), ok, "Producer not blocked"),
    etap:is(couch_work_queue:item_count(Q), 0, "Queue item count is 0"),
    etap:is(couch_work_queue:size(Q), 0, "Queue size is 0 bytes"),

    etap:is(close_queue(Q), ok, "Closed queue"),

    etap:is(ping(Consumer1), ok, "Consumer 1 unblocked"),
    etap:is(last_consumer_items(Consumer1), {ok, [Item4]},
       "Consumer 1 received 1 item"),

    etap:is(couch_work_queue:item_count(Q), closed, "Queue closed"),
    etap:is(couch_work_queue:size(Q), closed, "Queue closed"),

    etap:is(ping(Consumer2), ok, "Consumer 2 unblocked"),
    etap:is(last_consumer_items(Consumer2), closed,
        "Consumer 2 received 'closed' atom"),

    etap:is(ping(Consumer3), ok, "Consumer 3 unblocked"),
    etap:is(last_consumer_items(Consumer3), closed,
        "Consumer 3 received 'closed' atom"),

    stop(Producer, "producer"),
    stop(Consumer1, "consumer 1"),
    stop(Consumer2, "consumer 2"),
    stop(Consumer3, "consumer 3").


close_queue(Q) ->
    ok = couch_work_queue:close(Q),
    MonRef = erlang:monitor(process, Q),
    receive
    {'DOWN', MonRef, process, Q, _Reason} ->
         etap:diag("Queue closed")
    after 3000 ->
         erlang:demonitor(MonRef),
         timeout
    end.


spawn_consumer(Q) ->
    Parent = self(),
    spawn(fun() -> consumer_loop(Parent, Q, nil) end).


consumer_loop(Parent, Q, PrevItem) ->
    receive
    {stop, Ref} ->
        Parent ! {ok, Ref};
    {ping, Ref} ->
        Parent ! {pong, Ref},
        consumer_loop(Parent, Q, PrevItem);
    {last_item, Ref} ->
        Parent ! {item, Ref, PrevItem},
        consumer_loop(Parent, Q, PrevItem);
    {consume, N} ->
        Result = couch_work_queue:dequeue(Q, N),
        consumer_loop(Parent, Q, Result)
    end.


spawn_producer(Q) ->
    Parent = self(),
    spawn(fun() -> producer_loop(Parent, Q) end).


producer_loop(Parent, Q) ->
    receive
    {stop, Ref} ->
        Parent ! {ok, Ref};
    {ping, Ref} ->
        Parent ! {pong, Ref},
        producer_loop(Parent, Q);
    {produce, Ref, Size} ->
        Item = crypto:rand_bytes(Size),
        Parent ! {item, Ref, Item},
        ok = couch_work_queue:queue(Q, Item),
        producer_loop(Parent, Q)
    end.


consume(Consumer, N) ->
    Consumer ! {consume, N}.


last_consumer_items(Consumer) ->
    Ref = make_ref(),
    Consumer ! {last_item, Ref},
    receive
    {item, Ref, Items} ->
        Items
    after 3000 ->
        timeout
    end.


produce(Producer, Size) ->
    Ref = make_ref(),
    Producer ! {produce, Ref, Size},
    receive
    {item, Ref, Item} ->
        Item
    after 3000 ->
        etap:bail("Timeout asking producer to produce an item")
    end.


ping(Pid) ->
    Ref = make_ref(),
    Pid ! {ping, Ref},
    receive
    {pong, Ref} ->
        ok
    after 3000 ->
        timeout
    end.


stop(Pid, Name) ->
    Ref = make_ref(),
    Pid ! {stop, Ref},
    receive
    {ok, Ref} ->
        etap:diag("Stopped " ++ Name)
    after 3000 ->
        etap:bail("Timeout stopping " ++ Name)
    end.
