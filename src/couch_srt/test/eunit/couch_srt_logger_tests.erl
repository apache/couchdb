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

-module(couch_srt_logger_tests).

-include_lib("couch/include/couch_db.hrl").
-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").
-include("../../src/couch_srt.hrl").

%% Use different values than default configs to ensure they're picked up
-define(THRESHOLD_DBNAME_IO, 91).
-define(THRESHOLD_DOCS_READ, 123).
-define(THRESHOLD_DOCS_WRITTEN, 12).
-define(THRESHOLD_IOQ_CALLS, 439).
-define(THRESHOLD_ROWS_READ, 143).
-define(THRESHOLD_CHANGES, 79).
-define(THRESHOLD_LONG_REQS, 432).

csrt_logger_reporting_works_test_() ->
    {
        foreach,
        fun setup_reporting/0,
        fun teardown_reporting/1,
        [
            ?TDEF_FE(t_enablement),
            ?TDEF_FE(t_do_report),
            ?TDEF_FE(t_do_lifetime_report),
            ?TDEF_FE(t_do_status_report)
        ]
    }.

csrt_logger_matchers_test_() ->
    {
        foreach,
        fun setup/0,
        fun teardown/1,
        [
            ?TDEF_FE(t_enablement),
            ?TDEF_FE(t_matcher_on_dbnames_io),
            ?TDEF_FE(t_matcher_on_docs_read),
            ?TDEF_FE(t_matcher_on_docs_written),
            ?TDEF_FE(t_matcher_on_rows_read),
            ?TDEF_FE(t_matcher_on_changes_processed),
            ?TDEF_FE(t_matcher_on_long_reqs),
            ?TDEF_FE(t_matcher_on_ioq_calls),
            ?TDEF_FE(t_matcher_on_nonce),
            ?TDEF_FE(t_matcher_on_all_coordinators),
            ?TDEF_FE(t_matcher_on_all_rpc_workers),
            ?TDEF_FE(t_matcher_register_deregister)
        ]
    }.

make_docs(Count) ->
    lists:map(
        fun(I) ->
            #doc{
                id = ?l2b("foo_" ++ integer_to_list(I)),
                body = {[{<<"value">>, I}]}
            }
        end,
        lists:seq(1, Count)
    ).

set_matcher_threshold(_Key, undefined) ->
    ok;
set_matcher_threshold(Key, Val) when is_integer(Val) ->
    config:set(?CSRT_MATCHERS_THRESHOLD, Key, integer_to_list(Val), false).

set_dbnames_io_threshold(Key, Val) when is_integer(Val) ->
    config:set(?CSRT_MATCHERS_DBNAMES, Key, integer_to_list(Val), false).

setup() ->
    Ctx = test_util:start_couch([fabric, couch_stats, couch_srt]),
    couch_srt_test_helper:enable_default_logger_matchers(),
    config:set_boolean(?CSRT, "randomize_testing", false, false),
    config:set_boolean(?CSRT, "enable_reporting", true, false),
    config:set_boolean(?CSRT, "enable_rpc_reporting", true, false),
    ok = meck:new(ioq, [passthrough]),
    ok = meck:expect(ioq, bypass, fun(_, _) -> false end),
    DbName = ?tempdb(),
    ok = fabric:create_db(DbName, [{q, 8}, {n, 1}]),
    Docs = make_docs(100),
    Opts = [],
    {ok, _} = fabric:update_docs(DbName, Docs, Opts),
    Method = 'GET',
    Path = "/" ++ ?b2l(DbName) ++ "/_all_docs",
    Nonce = couch_util:to_hex(crypto:strong_rand_bytes(5)),
    Req = #httpd{method = Method, nonce = Nonce},
    {_, _} = PidRef = couch_srt:create_coordinator_context(Req, Path),
    MArgs = #mrargs{include_docs = false},
    _Res = fabric:all_docs(DbName, [?ADMIN_CTX], fun view_cb/2, [], MArgs),
    Rctx = load_rctx(PidRef),

    DefaultMatcherThresholds = [
        {"all_coordinators", undefined},
        {"all_rpc_workers", undefined},
        {"docs_read", ?THRESHOLD_DOCS_READ},
        {"docs_written", ?THRESHOLD_DOCS_WRITTEN},
        {"ioq_calls", ?THRESHOLD_IOQ_CALLS},
        {"rows_read", ?THRESHOLD_ROWS_READ},
        {"changes_processed", ?THRESHOLD_CHANGES},
        {"long_reqs", ?THRESHOLD_LONG_REQS}
    ],
    [set_matcher_threshold(Key, Val) || {Key, Val} <- DefaultMatcherThresholds],

    DbnameIOMatcherThresholds = [
        {"foo", ?THRESHOLD_DBNAME_IO},
        {"bar", ?THRESHOLD_DBNAME_IO},
        {"foo/bar", ?THRESHOLD_DBNAME_IO}
    ],
    [set_dbnames_io_threshold(Key, Val) || {Key, Val} <- DbnameIOMatcherThresholds],

    couch_srt_logger:reload_matchers(),
    #{ctx => Ctx, dbname => DbName, rctx => Rctx, rctxs => couch_srt_test_helper:rctxs()}.

teardown(#{ctx := Ctx, dbname := DbName}) ->
    ok = fabric:delete_db(DbName, [?ADMIN_CTX]),
    ok = meck:unload(ioq),
    test_util:stop_couch(Ctx).

setup_reporting() ->
    Ctx = setup(),
    ok = meck:new(couch_log, [passthrough]),
    ok = meck:expect(couch_log, report, fun(_, _) -> true end),
    Ctx.

teardown_reporting(Ctx) ->
    ok = meck:unload(couch_log),
    teardown(Ctx).

t_enablement(#{}) ->
    %% Set an invalid match spec to ensure couch_srt_logger is resilient
    config:set(?CSRT_MATCHERS_DBNAMES, "foobar", "lkajsdfkjkkadfjkajkf", false),
    ?assertEqual(ok, couch_srt_logger:reload_matchers(), "reloads even with bad matcher specs set"),
    ?assert(couch_srt_util:is_enabled(), "CSRT is enabled"),
    ?assert(couch_srt_util:is_enabled_reporting(), "CSRT reporting is enabled"),
    ?assert(couch_srt_util:is_enabled_rpc_reporting(), "CSRT RPC reporting is enabled").

t_do_report(#{rctx := Rctx}) ->
    JRctx = couch_srt_test_helper:jrctx(Rctx),
    ReportName = "foo",
    ?assert(couch_srt_logger:do_report(ReportName, Rctx), "CSRT _logger:do_report " ++ ReportName),
    ?assert(meck:validate(couch_log), "CSRT do_report"),
    ?assert(meck:validate(couch_log), "CSRT validate couch_log"),
    ?assert(
        meck:called(couch_log, report, [ReportName, JRctx]),
        "CSRT couch_log:report"
    ).

t_do_lifetime_report(#{rctx := Rctx}) ->
    JRctx = couch_srt_test_helper:jrctx(Rctx),
    ReportName = "csrt-pid-usage-lifetime",
    ?assert(
        couch_srt_logger:do_lifetime_report(Rctx),
        "CSRT _logger:do_report " ++ ReportName
    ),
    ?assert(meck:validate(couch_log), "CSRT validate couch_log"),
    ?assert(
        meck:called(couch_log, report, [ReportName, JRctx]),
        "CSRT couch_log:report"
    ).

t_do_status_report(#{rctx := Rctx}) ->
    JRctx = couch_srt_test_helper:jrctx(Rctx),
    ReportName = "csrt-pid-usage-status",
    ?assert(couch_srt_logger:do_status_report(Rctx), "couch_srt_logger:do_ " ++ ReportName),
    ?assert(meck:validate(couch_log), "CSRT validate couch_log"),
    ?assert(
        meck:called(couch_log, report, [ReportName, JRctx]),
        "CSRT couch_log:report"
    ).

t_matcher_on_docs_read(#{rctxs := Rctxs0}) ->
    Threshold = ?THRESHOLD_DOCS_READ,
    %% Make sure we have at least one match
    Rctxs = [couch_srt_test_helper:rctx_gen(#{docs_read => Threshold + 10}) | Rctxs0],
    ?assertEqual(
        lists:sort(lists:filter(matcher_gte(docs_read, Threshold), Rctxs)),
        lists:sort(lists:filter(matcher_for_csrt("docs_read"), Rctxs)),
        "Docs read matcher"
    ).

t_matcher_on_docs_written(#{rctxs := Rctxs0}) ->
    Threshold = ?THRESHOLD_DOCS_WRITTEN,
    %% Make sure we have at least one match
    Rctxs = [couch_srt_test_helper:rctx_gen(#{docs_written => Threshold + 10}) | Rctxs0],
    ?assertEqual(
        lists:sort(lists:filter(matcher_gte(docs_written, Threshold), Rctxs)),
        lists:sort(lists:filter(matcher_for_csrt("docs_written"), Rctxs)),
        "Docs written matcher"
    ).

t_matcher_on_rows_read(#{rctxs := Rctxs0}) ->
    Threshold = ?THRESHOLD_ROWS_READ,
    %% Make sure we have at least one match
    Rctxs = [couch_srt_test_helper:rctx_gen(#{rows_read => Threshold + 10}) | Rctxs0],
    ?assertEqual(
        lists:sort(lists:filter(matcher_gte(rows_read, Threshold), Rctxs)),
        lists:sort(lists:filter(matcher_for_csrt("rows_read"), Rctxs)),
        "Rows read matcher"
    ).

t_matcher_on_changes_processed(#{rctxs := Rctxs0}) ->
    Threshold = ?THRESHOLD_CHANGES,
    %% Make sure we have at least one match
    Rctx0 = couch_srt_test_helper:rctx_gen(#{
        mod => chttpd_db, func => handle_changes_req, rows_read => Threshold + 10
    }),
    Rctxs = [Rctx0 | Rctxs0],
    ChangesFilter =
        fun
            %% Matcher on changes only works for coordinators at the moment due
            %% to overloading over rows_read for all aggregate operations
            (#rctx{type = #coordinator{mod = chttpd_db, func = handle_changes_req}} = R) ->
                Ret = couch_srt_entry:value(changes_returned, R),
                Proc = couch_srt_entry:value(rows_read, R),
                (Proc - Ret) >= Threshold;
            (_) ->
                false
        end,
    ?assertEqual(
        lists:sort(lists:filter(ChangesFilter, Rctxs)),
        lists:sort(lists:filter(matcher_for_csrt("changes_processed"), Rctxs)),
        "Changes processed matcher"
    ).

t_matcher_on_long_reqs(#{rctxs := Rctxs0}) ->
    %% Threshold is in milliseconds, convert to native time format
    Threshold = ?THRESHOLD_LONG_REQS,
    NativeThreshold = erlang:convert_time_unit(Threshold, millisecond, native),
    %% Native is a small timescale, make sure we have enough for a millisecond
    %% measureable time delta
    %% Make sure we have at least one match
    Now = couch_srt_util:tnow(),
    UpdatedAt = Now - round(NativeThreshold * 1.23),
    Rctxs = [
        couch_srt_test_helper:rctx_gen(#{started_at => Now, updated_at => UpdatedAt}) | Rctxs0
    ],
    DurationFilter = fun(R) ->
        Started = couch_srt_entry:value(started_at, R),
        Updated = couch_srt_entry:value(updated_at, R),
        Updated - Started >= NativeThreshold
    end,
    ?assertEqual(
        lists:sort(lists:filter(DurationFilter, Rctxs)),
        lists:sort(lists:filter(matcher_for_csrt("long_reqs"), Rctxs)),
        "Long requests matcher"
    ).

t_matcher_on_ioq_calls(#{rctxs := Rctxs0}) ->
    Threshold = ?THRESHOLD_IOQ_CALLS,
    %% Make sure we have at least one match
    Rctxs = [couch_srt_test_helper:rctx_gen(#{ioq_calls => Threshold + 10}) | Rctxs0],
    ?assertEqual(
        lists:sort(lists:filter(matcher_gte(ioq_calls, Threshold), Rctxs)),
        lists:sort(lists:filter(matcher_for_csrt("ioq_calls"), Rctxs)),
        "IOQ calls matcher"
    ).

t_matcher_on_nonce(#{rctxs := Rctxs0}) ->
    Nonce = "foobar7799",
    %% Make sure we have at least one match
    Rctxs = [couch_srt_test_helper:rctx_gen(#{nonce => Nonce}) | Rctxs0],
    %% Nonce requires dynamic matcher as it's a static match
    %% TODO: add pattern based nonce matching
    MSpec = couch_srt_logger:matcher_on_nonce(Nonce),
    CompMSpec = ets:match_spec_compile(MSpec),
    Matchers = #{"nonce" => {MSpec, CompMSpec}},
    IsMatch = fun(ARctx) -> couch_srt_logger:is_match(ARctx, Matchers) end,
    ?assertEqual(
        lists:sort(lists:filter(matcher_on(nonce, Nonce), Rctxs)),
        lists:sort(lists:filter(IsMatch, Rctxs)),
        "Rows read matcher"
    ).

t_matcher_on_dbnames_io(#{rctxs := Rctxs0}) ->
    Threshold = ?THRESHOLD_DBNAME_IO,
    SThreshold = integer_to_list(Threshold),
    DbFoo = "foo",
    DbBar = "bar",
    MatcherFoo = matcher_for_csrt("dbnames_io__" ++ DbFoo ++ "__" ++ SThreshold),
    MatcherBar = matcher_for_csrt("dbnames_io__" ++ DbBar ++ "__" ++ SThreshold),
    MatcherFooBar = matcher_for_csrt("dbnames_io__foo/bar__" ++ SThreshold),
    %% Add an extra Rctx with dbname foo/bar to ensure correct naming matches
    ExtraRctx = couch_srt_test_helper:rctx_gen(#{
        dbname => <<"foo/bar">>, get_kp_node => Threshold + 10
    }),
    %% Make sure we have at least one match
    Rctxs = [ExtraRctx, couch_srt_test_helper:rctx_gen(#{ioq_calls => Threshold + 10}) | Rctxs0],
    ?assertEqual(
        lists:sort(lists:filter(matcher_for_dbnames_io(DbFoo, Threshold), Rctxs)),
        lists:sort(lists:filter(MatcherFoo, Rctxs)),
        "dbnames_io foo matcher"
    ),
    ?assertEqual(
        lists:sort(lists:filter(matcher_for_dbnames_io(DbBar, Threshold), Rctxs)),
        lists:sort(lists:filter(MatcherBar, Rctxs)),
        "dbnames_io bar matcher"
    ),
    ?assertEqual(
        [ExtraRctx],
        lists:sort(lists:filter(MatcherFooBar, Rctxs)),
        "dbnames_io foo/bar matcher"
    ).

t_matcher_register_deregister(#{rctxs := Rctxs0}) ->
    CrazyDbName = <<"asdf123@?!&#fdsa">>,
    MName = "Crazy-Matcher",
    MSpec = couch_srt_logger:matcher_on_dbname(CrazyDbName),
    %% Add an extra Rctx with CrazyDbName to create a specific match
    ExtraRctx = couch_srt_test_helper:rctx_gen(#{dbname => CrazyDbName}),
    %% Make sure we have at least one match
    Rctxs = [ExtraRctx | Rctxs0],

    ?assertEqual(#{}, couch_srt_logger:get_registered_matchers(), "no current registered matchers"),
    ?assertEqual(
        {error, {invalid_ms, "bad_spec", "fdsa"}},
        couch_srt_logger:register_matcher("bad_spec", "fdsa"),
        "register bad matcher fails"
    ),
    ?assertEqual(ok, couch_srt_logger:register_matcher(MName, MSpec), "register matcher"),
    CompMSpec = test_util:wait(
        fun() ->
            case couch_srt_logger:get_matcher(MName) of
                undefined ->
                    wait;
                {MSpec, _Ref} = CompMSpec0 ->
                    CompMSpec0
            end
        end
    ),
    Matchers = #{MName => CompMSpec},
    ?assert(CompMSpec =/= timeout, "newly registered matcher was initialized"),
    ?assertEqual(
        [MName],
        maps:keys(couch_srt_logger:get_registered_matchers()),
        "correct current registered matchers"
    ),
    ?assert(
        couch_srt_logger:is_match(ExtraRctx, Matchers), "our registered matcher matches expectedly"
    ),
    ?assert(
        couch_srt_logger:is_match(ExtraRctx),
        "our registered matcher is picked up and matches expectedly"
    ),
    ?assertEqual(
        Matchers,
        couch_srt_logger:find_matches(Rctxs, Matchers),
        "we find our matcher and no extra matchers"
    ),
    ?assert(
        maps:is_key(
            MName,
            couch_srt_logger:find_matches(Rctxs, couch_srt_logger:get_matchers())
        ),
        "find our CrazyDbName matcher in matches against all registered matchers"
    ),
    ?assertEqual(
        #{MName => [ExtraRctx]},
        couch_srt_logger:find_all_matches(Rctxs, Matchers),
        "find our CrazyDb ExtraRctx with our Matcher, and nothing else"
    ),
    ?assertEqual(ok, couch_srt_logger:reload_matchers(), "we can reload matchers"),
    ?assertEqual(
        [MName],
        maps:keys(couch_srt_logger:get_registered_matchers()),
        "correct current registered matchers after a global reload"
    ),
    ?assert(
        maps:is_key(
            MName,
            couch_srt_logger:find_matches(Rctxs, couch_srt_logger:get_matchers())
        ),
        "our matcher still behaves expectedly after a global matcher reload"
    ),
    ?assertEqual(ok, couch_srt_logger:deregister_matcher(MName), "deregister_matcher returns ok"),
    Matcher2 = test_util:wait(
        fun() ->
            case couch_srt_logger:get_matcher(MName) of
                undefined ->
                    undefined;
                _ ->
                    wait
            end
        end
    ),
    ?assertEqual(undefined, Matcher2, "matcher was deregistered successfully"),
    ?assertEqual(
        #{}, couch_srt_logger:get_registered_matchers(), "no leftover registered matchers"
    ).

t_matcher_on_all_coordinators(#{rctxs := Rctxs0}) ->
    %% Make sure we have at least one match
    Rctxs = [couch_srt_test_helper:rctx_gen(#{type => #coordinator{}}) | Rctxs0],
    ?assertEqual(
        lists:sort(lists:filter(matcher_on_coordinators(), Rctxs)),
        lists:sort(lists:filter(matcher_for_csrt("all_coordinators"), Rctxs)),
        "All Coordinators matcher"
    ).

t_matcher_on_all_rpc_workers(#{rctxs := Rctxs0}) ->
    %% Make sure we have at least one match
    Rctxs = [couch_srt_test_helper:rctx_gen(#{type => #rpc_worker{}}) | Rctxs0],
    ?assertEqual(
        lists:sort(lists:filter(matcher_on_rpc_workers(), Rctxs)),
        lists:sort(lists:filter(matcher_for_csrt("all_rpc_workers"), Rctxs)),
        "All RPC Workers matcher"
    ).

load_rctx(PidRef) ->
    %% Add slight delay to accumulate RPC response deltas
    timer:sleep(50),
    couch_srt:get_resource(PidRef).

view_cb({row, Row}, Acc) ->
    {ok, [Row | Acc]};
view_cb(_Msg, Acc) ->
    {ok, Acc}.

matcher_gte(Field, Value) ->
    matcher_for(Field, Value, fun erlang:'>='/2).

matcher_on(Field, Value) ->
    matcher_for(Field, Value, fun erlang:'=:='/2).

matcher_for(Field, Value, Op) ->
    fun(Rctx) -> Op(couch_srt_entry:value(Field, Rctx), Value) end.

matcher_on_coordinators() ->
    fun
        (#rctx{type = #coordinator{}}) -> true;
        (_) -> false
    end.

matcher_on_rpc_workers() ->
    fun
        (#rctx{type = #rpc_worker{}}) -> true;
        (_) -> false
    end.

matcher_for_csrt(MatcherName) ->
    Matchers = #{MatcherName => {_, _} = couch_srt_logger:get_matcher(MatcherName)},
    case couch_srt_logger:get_matcher(MatcherName) of
        {_, _} = Matcher ->
            Matchers = #{MatcherName => Matcher},
            fun(Rctx) -> couch_srt_logger:is_match(Rctx, Matchers) end;
        _ ->
            throw({missing_matcher, MatcherName})
    end.

matcher_for_dbnames_io(Dbname0, Threshold) ->
    Dbname = list_to_binary(Dbname0),
    fun(Rctx) ->
        DbnameA = couch_srt_entry:value(dbname, Rctx),
        Fields = [ioq_calls, get_kv_node, get_kp_node, docs_read, rows_read],
        Vals = [{F, couch_srt_entry:value(F, Rctx)} || F <- Fields],
        Dbname =:= mem3:dbname(DbnameA) andalso lists:any(fun({_K, V}) -> V >= Threshold end, Vals)
    end.
