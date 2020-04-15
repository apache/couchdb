-module(chttpd_stats_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").


start() ->
    ok = application:start(config),
    ok = application:start(couch_log).


stop(_) ->
    ok = application:stop(config),
    ok = application:stop(couch_log).


setup() ->
    ok = meck:new(chttpd_stats, [passthrough]).


teardown(_) ->
    meck:unload(),
    ok.



chttpd_stats_test_() ->
    {
        "chttpd_stats tests",
        {
            setup,
            fun start/0,
            fun stop/1,
            {
                foreach,
                fun setup/0, fun teardown/1,
                [
                    fun test_reset/1,
                    fun test_no_reset/1
                ]
            }
        }
    }.


test_reset(_) ->
    ?_test(begin
        chttpd_stats:init(undefined),
        chttpd_stats:incr_rows(3),
        chttpd_stats:incr_rows(),
        chttpd_stats:incr_writes(5),
        chttpd_stats:incr_writes(),
        chttpd_stats:incr_reads(),
        chttpd_stats:incr_reads(2),
        State1 = get(chttpd_stats),
        ?assertMatch({st, 3, 6, 4, _, _, _, _}, State1),

        ok = meck:expect(chttpd_stats, report, fun(_) -> true end),
        % force a reset with 0 interval
        chttpd_stats:update_interval(0),
        % after this is called, the report should happen and rows should
        % reset to 0
        chttpd_stats:incr_rows(),
        ResetState = get(chttpd_stats),
        ?assertMatch({st, 0, 0, 0, _, _, _, _}, ResetState)
    end).


test_no_reset(_) ->
    ?_test(begin
        ok = meck:expect(chttpd_stats, report, fun(_) -> false end),
        chttpd_stats:init(undefined),
        chttpd_stats:update_interval(0),
        chttpd_stats:incr_rows(),
        State = get(chttpd_stats),
        ?assertMatch({st, 0, 0, 1, _, _, _, _}, State)
    end).
