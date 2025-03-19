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

-module(csrt_logger_tests).

-include_lib("couch/include/couch_db.hrl").
-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").


csrt_logger_works_test_() ->
    {
        foreach,
        fun setup/0,
        fun teardown/1,
        [
            ?TDEF_FE(t_do_report),
            ?TDEF_FE(t_do_lifetime_report),
            ?TDEF_FE(t_do_status_report)
        ]
    }.

make_docs(Count) ->
    lists:map(
        fun(I) ->
            #doc{
                id = ?l2b("foo_" ++ integer_to_list(I)),
                body={[{<<"value">>, I}]}
            }
        end,
        lists:seq(1, Count)).

setup() ->
    Ctx = test_util:start_couch([fabric, couch_stats]),
    DbName = ?tempdb(),
    ok = fabric:create_db(DbName, [{q, 8}, {n, 1}]),
    Docs = make_docs(100),
    Opts = [],
    {ok, _} = fabric:update_docs(DbName, Docs, Opts),
    Method = 'GET',
    Path = "/" ++ ?b2l(DbName) ++ "/_all_docs",
    Nonce = couch_util:to_hex(crypto:strong_rand_bytes(5)),
    Req = #httpd{method=Method, nonce=Nonce},
    {_, _} = PidRef = csrt:create_coordinator_context(Req, Path),
    MArgs = #mrargs{include_docs = false},
    _Res = fabric:all_docs(DbName, [?ADMIN_CTX], fun view_cb/2, [], MArgs),
    Rctx = load_rctx(PidRef),
    #{ctx => Ctx, dbname => DbName, rctx => Rctx}.

teardown(#{ctx := Ctx, dbname := DbName}) ->
    ok = fabric:delete_db(DbName, [?ADMIN_CTX]),
    test_util:stop_couch(Ctx).

t_do_report(#{rctx := RctxA}) ->
    LRctxA = lists:sort(maps:to_list(csrt_util:to_json(RctxA))),
    ReportNameA = "foo",
    ok = meck:new(couch_log),
    AssertDoReport = fun(ReportNameB, RctxB) ->
        LRctxB = lists:sort(maps:to_list(RctxB)),
        ?assertEqual(ReportNameA, ReportNameB, "Report Name"),
        ?assertEqual(LRctxA, LRctxB, "Rctx Report Body"),
        true
    end,
    ok = meck:expect(couch_log, report, AssertDoReport),
    ?assert(csrt_logger:do_report(ReportNameA, RctxA)),
    ?assert(meck:validate(couch_log), "CSRT do_report"),
    ok = meck:unload(couch_log).

t_do_lifetime_report(#{rctx := RctxA}) ->
    LRctxA = lists:sort(maps:to_list(csrt_util:to_json(RctxA))),
    ReportNameA = "csrt-pid-usage-lifetime",
    ok = meck:new(couch_log),
    AssertDoReport = fun(ReportNameB, RctxB) ->
        LRctxB = lists:sort(maps:to_list(RctxB)),
        ?assertEqual(ReportNameA, ReportNameB, "Report Name"),
        ?assertEqual(LRctxA, LRctxB, "Rctx Report Body"),
        true
    end,
    ok = meck:expect(couch_log, report, AssertDoReport),
    ?assert(csrt_logger:do_lifetime_report(RctxA)),
    ?assert(meck:validate(couch_log), "CSRT do_report"),
    ok = meck:unload(couch_log).

t_do_status_report(#{rctx := RctxA}) ->
    LRctxA = lists:sort(maps:to_list(csrt_util:to_json(RctxA))),
    ReportNameA = "csrt-pid-usage-status",
    ok = meck:new(couch_log),
    AssertDoReport = fun(ReportNameB, RctxB) ->
        LRctxB = lists:sort(maps:to_list(RctxB)),
        ?assertEqual(ReportNameA, ReportNameB, "Report Name"),
        ?assertEqual(LRctxA, LRctxB, "Rctx Report Body"),
        true
    end,
    ok = meck:expect(couch_log, report, AssertDoReport),
    ?assert(csrt_logger:do_status_report(RctxA)),
    ?assert(meck:validate(couch_log), "CSRT do_report"),
    ok = meck:unload(couch_log).

load_rctx(PidRef) ->
    timer:sleep(50), %% Add slight delay to accumulate RPC response deltas
    csrt:get_resource(PidRef).

view_cb({row, Row}, Acc) ->
    {ok, [Row | Acc]};
view_cb(_Msg, Acc) ->
    {ok, Acc}.
