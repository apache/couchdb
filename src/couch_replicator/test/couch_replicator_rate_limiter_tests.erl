-module(couch_replicator_rate_limiter_tests).

-include_lib("couch/include/couch_eunit.hrl").


rate_limiter_test_() ->
    {
        foreach,
        fun setup/0,
        fun teardown/1,
        [
            t_new_key(),
            t_1_failure(),
            t_2_failures_back_to_back(),
            t_2_failures(),
            t_success_threshold(),
            t_1_failure_2_successes()
        ]
    }.


t_new_key() ->
    ?_test(begin
        ?assertEqual(0, couch_replicator_rate_limiter:interval({"foo", get}))
    end).


t_1_failure() ->
    ?_test(begin
        ?assertEqual(24, couch_replicator_rate_limiter:failure({"foo", get}))
    end).


t_2_failures() ->
    ?_test(begin
        couch_replicator_rate_limiter:failure({"foo", get}),
        low_pass_filter_delay(),
        Interval = couch_replicator_rate_limiter:failure({"foo", get}),
        ?assertEqual(29, Interval)
    end).


t_2_failures_back_to_back() ->
    ?_test(begin
        couch_replicator_rate_limiter:failure({"foo", get}),
        Interval = couch_replicator_rate_limiter:failure({"foo", get}),
        ?assertEqual(24, Interval)
    end).


t_success_threshold() ->
    ?_test(begin
        Interval = couch_replicator_rate_limiter:success({"foo", get}),
        ?assertEqual(0, Interval),
        Interval = couch_replicator_rate_limiter:success({"foo", get}),
        ?assertEqual(0, Interval)
    end).


t_1_failure_2_successes() ->
    ?_test(begin
        couch_replicator_rate_limiter:failure({"foo", get}),
        low_pass_filter_delay(),
        Succ1 = couch_replicator_rate_limiter:success({"foo", get}),
        ?assertEqual(20, Succ1),
        low_pass_filter_delay(),
        Succ2 = couch_replicator_rate_limiter:success({"foo", get}),
        ?assertEqual(0, Succ2)
    end).


low_pass_filter_delay() ->
    timer:sleep(100).


setup() ->
    {ok, Pid} = couch_replicator_rate_limiter:start_link(),
    Pid.


teardown(Pid) ->
    Ref = erlang:monitor(process, Pid),
    unlink(Pid),
    exit(Pid, kill),
    receive
        {'DOWN', Ref, process, Pid, _} ->
            ok
    end,
    ok.
