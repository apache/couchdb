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

-module(couch_work_queue_tests).

-include("couch_eunit.hrl").

-define(TIMEOUT, 100).


setup(Opts) ->
    {ok, Q} = couch_work_queue:new(Opts),
    Producer = spawn_producer(Q),
    Consumer = spawn_consumer(Q),
    {Q, Producer, Consumer}.

setup_max_items() ->
    setup([{max_items, 3}]).

setup_max_size() ->
    setup([{max_size, 160}]).

setup_max_items_and_size() ->
    setup([{max_size, 160}, {max_items, 3}]).

setup_multi_workers() ->
    {Q, Producer, Consumer1} = setup([{max_size, 160},
                                      {max_items, 3},
                                      {multi_workers, true}]),
    Consumer2 = spawn_consumer(Q),
    Consumer3 = spawn_consumer(Q),
    {Q, Producer, [Consumer1, Consumer2, Consumer3]}.

teardown({Q, Producer, Consumers}) when is_list(Consumers) ->
    % consume all to unblock and let producer/consumer stop without timeout
    [consume(Consumer, all) || Consumer <- Consumers],

    ok = close_queue(Q),
    ok = stop(Producer, "producer"),
    R = [stop(Consumer, "consumer") || Consumer <- Consumers],
    R = [ok || _ <- Consumers],
    ok;
teardown({Q, Producer, Consumer}) ->
    teardown({Q, Producer, [Consumer]}).


single_consumer_test_() ->
    {
        "Single producer and consumer",
        [
            {
                "Queue with 3 max items",
                {
                    foreach,
                    fun setup_max_items/0, fun teardown/1,
                    single_consumer_max_item_count() ++ common_cases()
                }
            },
            {
                "Queue with max size of 160 bytes",
                {
                    foreach,
                    fun setup_max_size/0, fun teardown/1,
                    single_consumer_max_size() ++ common_cases()
                }
            },
            {
                "Queue with max size of 160 bytes and 3 max items",
                {
                    foreach,
                    fun setup_max_items_and_size/0, fun teardown/1,
                    single_consumer_max_items_and_size() ++ common_cases()
                }
            }
        ]
    }.

multiple_consumers_test_() ->
    {
        "Single producer and multiple consumers",
        [
            {
                "Queue with max size of 160 bytes and 3 max items",
                {
                    foreach,
                    fun setup_multi_workers/0, fun teardown/1,
                    common_cases() ++ multiple_consumers()
                }

            }
        ]
    }.

common_cases()->
    [
        fun should_block_consumer_on_dequeue_from_empty_queue/1,
        fun should_consume_right_item/1,
        fun should_timeout_on_close_non_empty_queue/1,
        fun should_not_block_producer_for_non_empty_queue_after_close/1,
        fun should_be_closed/1
    ].

single_consumer_max_item_count()->
    [
        fun should_have_no_items_for_new_queue/1,
        fun should_block_producer_on_full_queue_count/1,
        fun should_receive_first_queued_item/1,
        fun should_consume_multiple_items/1,
        fun should_consume_all/1
    ].

single_consumer_max_size()->
    [
        fun should_have_zero_size_for_new_queue/1,
        fun should_block_producer_on_full_queue_size/1,
        fun should_increase_queue_size_on_produce/1,
        fun should_receive_first_queued_item/1,
        fun should_consume_multiple_items/1,
        fun should_consume_all/1
    ].

single_consumer_max_items_and_size() ->
    single_consumer_max_item_count() ++ single_consumer_max_size().

multiple_consumers() ->
    [
        fun should_have_zero_size_for_new_queue/1,
        fun should_have_no_items_for_new_queue/1,
        fun should_increase_queue_size_on_produce/1
    ].


should_have_no_items_for_new_queue({Q, _, _}) ->
    ?_assertEqual(0, couch_work_queue:item_count(Q)).

should_have_zero_size_for_new_queue({Q, _, _}) ->
    ?_assertEqual(0, couch_work_queue:size(Q)).

should_block_consumer_on_dequeue_from_empty_queue({_, _, Consumers}) when is_list(Consumers) ->
    [consume(C, 2) || C <- Consumers],
    Pongs = [ping(C) || C <- Consumers],
    ?_assertEqual([timeout, timeout, timeout], Pongs);
should_block_consumer_on_dequeue_from_empty_queue({_, _, Consumer}) ->
    consume(Consumer, 1),
    Pong = ping(Consumer),
    ?_assertEqual(timeout, Pong).

should_consume_right_item({Q, Producer, Consumers}) when is_list(Consumers) ->
    [consume(C, 3) || C <- Consumers],

    Item1 = produce(Producer, 10),
    ok = ping(Producer),
    ?assertEqual(0, couch_work_queue:item_count(Q)),
    ?assertEqual(0, couch_work_queue:size(Q)),

    Item2 = produce(Producer, 10),
    ok = ping(Producer),
    ?assertEqual(0, couch_work_queue:item_count(Q)),
    ?assertEqual(0, couch_work_queue:size(Q)),

    Item3 = produce(Producer, 10),
    ok = ping(Producer),
    ?assertEqual(0, couch_work_queue:item_count(Q)),
    ?assertEqual(0, couch_work_queue:size(Q)),

    R = [{ping(C), Item}
         || {C, Item} <- lists:zip(Consumers, [Item1, Item2, Item3])],

    ?_assertEqual([{ok, Item1}, {ok, Item2}, {ok, Item3}], R);
should_consume_right_item({_, Producer, Consumer}) ->
    consume(Consumer, 1),
    Item = produce(Producer, 10),
    produce(Producer, 20),
    ok = ping(Producer),
    ok = ping(Consumer),
    {ok, Items} = last_consumer_items(Consumer),
    ?_assertEqual([Item], Items).

should_increase_queue_size_on_produce({Q, Producer, _}) ->
    produce(Producer, 50),
    ok = ping(Producer),
    Count1 = couch_work_queue:item_count(Q),
    Size1 = couch_work_queue:size(Q),

    produce(Producer, 10),
    Count2 = couch_work_queue:item_count(Q),
    Size2 = couch_work_queue:size(Q),

    ?_assertEqual([{Count1, Size1}, {Count2, Size2}], [{1, 50}, {2, 60}]).

should_block_producer_on_full_queue_count({Q, Producer, _}) ->
    produce(Producer, 10),
    ?assertEqual(1, couch_work_queue:item_count(Q)),
    ok = ping(Producer),

    produce(Producer, 15),
    ?assertEqual(2, couch_work_queue:item_count(Q)),
    ok = ping(Producer),

    produce(Producer, 20),
    ?assertEqual(3, couch_work_queue:item_count(Q)),
    Pong = ping(Producer),

    ?_assertEqual(timeout, Pong).

should_block_producer_on_full_queue_size({Q, Producer, _}) ->
    produce(Producer, 100),
    ok = ping(Producer),
    ?assertEqual(1, couch_work_queue:item_count(Q)),
    ?assertEqual(100, couch_work_queue:size(Q)),

    produce(Producer, 110),
    Pong = ping(Producer),
    ?assertEqual(2, couch_work_queue:item_count(Q)),
    ?assertEqual(210, couch_work_queue:size(Q)),

    ?_assertEqual(timeout, Pong).

should_consume_multiple_items({_, Producer, Consumer}) ->
    Item1 = produce(Producer, 10),
    ok = ping(Producer),

    Item2 = produce(Producer, 15),
    ok = ping(Producer),

    consume(Consumer, 2),

    {ok, Items} = last_consumer_items(Consumer),
    ?_assertEqual([Item1, Item2], Items).

should_receive_first_queued_item({Q, Producer, Consumer}) ->
    consume(Consumer, 100),
    timeout = ping(Consumer),

    Item = produce(Producer, 11),
    ok = ping(Producer),

    ok = ping(Consumer),
    ?assertEqual(0, couch_work_queue:item_count(Q)),

    {ok, Items} = last_consumer_items(Consumer),
    ?_assertEqual([Item], Items).

should_consume_all({_, Producer, Consumer}) ->
    Item1 = produce(Producer, 10),
    Item2 = produce(Producer, 15),
    Item3 = produce(Producer, 20),

    consume(Consumer, all),

    {ok, Items} = last_consumer_items(Consumer),
    ?_assertEqual([Item1, Item2, Item3], Items).

should_timeout_on_close_non_empty_queue({Q, Producer, _}) ->
    produce(Producer, 1),
    Status = close_queue(Q),

    ?_assertEqual(timeout, Status).

should_not_block_producer_for_non_empty_queue_after_close({Q, Producer, _}) ->
    produce(Producer, 1),
    close_queue(Q),
    Pong = ping(Producer),
    Size = couch_work_queue:size(Q),
    Count = couch_work_queue:item_count(Q),

    ?_assertEqual({ok, 1, 1}, {Pong, Size, Count}).

should_be_closed({Q, _, Consumers}) when is_list(Consumers) ->
    ok = close_queue(Q),

    [consume(C, 1) || C <- Consumers],

    LastConsumerItems = [last_consumer_items(C) || C <- Consumers],
    ItemsCount = couch_work_queue:item_count(Q),
    Size = couch_work_queue:size(Q),

    ?_assertEqual({[closed, closed, closed], closed, closed},
                  {LastConsumerItems, ItemsCount, Size});
should_be_closed({Q, _, Consumer}) ->
    ok = close_queue(Q),

    consume(Consumer, 1),

    LastConsumerItems = last_consumer_items(Consumer),
    ItemsCount = couch_work_queue:item_count(Q),
    Size = couch_work_queue:size(Q),

    ?_assertEqual({closed, closed, closed},
                  {LastConsumerItems, ItemsCount, Size}).


close_queue(Q) ->
    ok = couch_work_queue:close(Q),
    MonRef = erlang:monitor(process, Q),
    receive
        {'DOWN', MonRef, process, Q, _Reason} -> ok
    after ?TIMEOUT ->
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
    after ?TIMEOUT ->
        timeout
    end.

produce(Producer, Size) ->
    Ref = make_ref(),
    Producer ! {produce, Ref, Size},
    receive
        {item, Ref, Item} ->
            Item
    after ?TIMEOUT ->
        erlang:error({assertion_failed,
                      [{module, ?MODULE},
                       {line, ?LINE},
                       {reason, "Timeout asking producer to produce an item"}]})
    end.

ping(Pid) ->
    Ref = make_ref(),
    Pid ! {ping, Ref},
    receive
        {pong, Ref} ->
            ok
    after ?TIMEOUT ->
        timeout
    end.

stop(Pid, Name) ->
    Ref = make_ref(),
    Pid ! {stop, Ref},
    receive
        {ok, Ref} -> ok
    after ?TIMEOUT ->
        ?debugMsg("Timeout stopping " ++ Name),
        timeout
    end.
