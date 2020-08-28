-module(couch_replicator_rate_limiter_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("fabric/test/fabric2_test.hrl").


rate_limiter_test_() ->
    {
        foreach,
        fun setup/0,
        fun teardown/1,
        [
            ?TDEF_FE(t_new_key),
            ?TDEF_FE(t_1_failure),
            ?TDEF_FE(t_2_failures_back_to_back),
            ?TDEF_FE(t_2_failures),
            ?TDEF_FE(t_success_threshold),
            ?TDEF_FE(t_1_failure_2_successes)
        ]
    }.


t_new_key(_) ->
    ?assertEqual(0, couch_replicator_rate_limiter:interval({"foo", get})).


t_1_failure(_) ->
    ?assertEqual(24, couch_replicator_rate_limiter:failure({"foo", get})).


t_2_failures(_) ->
    couch_replicator_rate_limiter:failure({"foo", get}),
    low_pass_filter_delay(),
    Interval = couch_replicator_rate_limiter:failure({"foo", get}),
    ?assertEqual(29, Interval).


t_2_failures_back_to_back(_) ->
    couch_replicator_rate_limiter:failure({"foo", get}),
    Interval = couch_replicator_rate_limiter:failure({"foo", get}),
    ?assertEqual(24, Interval).


t_success_threshold(_) ->
    Interval = couch_replicator_rate_limiter:success({"foo", get}),
    ?assertEqual(0, Interval),
    Interval = couch_replicator_rate_limiter:success({"foo", get}),
    ?assertEqual(0, Interval).


t_1_failure_2_successes(_) ->
    couch_replicator_rate_limiter:failure({"foo", get}),
    low_pass_filter_delay(),
    Succ1 = couch_replicator_rate_limiter:success({"foo", get}),
    ?assertEqual(20, Succ1),
    low_pass_filter_delay(),
    Succ2 = couch_replicator_rate_limiter:success({"foo", get}),
    ?assertEqual(0, Succ2).


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
