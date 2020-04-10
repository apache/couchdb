-module(chttpd_stats_tests).

-define(NODEBUG, ok).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").


setup() ->
    ok = meck:new(chttpd_stats, [unstick, passthrough]),
    ok = meck:expect(chttpd_stats, report, fun(_) -> ok end).

teardown(_) ->
    meck:unload(),
    ok.


chttpd_stats_test_() ->
    {
        "chttpd_stats tests",
        {
            setup,
            fun chttpd_test_util:start_couch/0,
            fun chttpd_test_util:stop_couch/1,
            {
                foreach,
                fun setup/0, fun teardown/1,
                [
                    fun test_reset/1
                ]
            }
        }
    }.


test_reset(_) ->
   config:set_integer("chttpd", "stats_reset_interval", 60),
   chttpd_stats:init(undefined),
   chttpd_stats:incr_rows(),
   State = get(chttpd_stats),
   ?assertEqual(1, element(4, State)),
   % force a reset with 0 interval
   chttpd_stats:update_interval(0),
   % after this is called, the report should happen and rows should
   % reset to 0
   chttpd_stats:incr_rows(),
   ResetState = get(chttpd_stats),
   ?_assertEqual(0, element(4, ResetState)).
