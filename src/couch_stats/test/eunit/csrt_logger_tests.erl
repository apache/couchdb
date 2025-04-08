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

-define(RCTX_RANGE, 1000).
-define(RCTX_COUNT, 10000).

%% Dirty hack for hidden records as .hrl is only in src/
-define(RCTX_RPC, {rpc_worker, foo, bar, {self(), make_ref()}}).
-define(RCTX_COORDINATOR, {coordinator, foo, bar, 'GET', "/foo/_all_docs"}).

-define(THRESHOLD_DBNAME, <<"foo">>).
-define(THRESHOLD_DBNAME_IO, 91).
-define(THRESHOLD_DOCS_READ, 123).
-define(THRESHOLD_IOQ_CALLS, 439).
-define(THRESHOLD_ROWS_READ, 143).
-define(THRESHOLD_CHANGES, 79).

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


csrt_logger_matchers_test_() ->
    {
        foreach,
        fun setup/0,
        fun teardown/1,
        [
            ?TDEF_FE(t_matcher_on_dbname),
            ?TDEF_FE(t_matcher_on_dbnames_io),
            ?TDEF_FE(t_matcher_on_docs_read),
            ?TDEF_FE(t_matcher_on_docs_written),
            ?TDEF_FE(t_matcher_on_rows_read),
            ?TDEF_FE(t_matcher_on_worker_changes_processed),
            ?TDEF_FE(t_matcher_on_ioq_calls),
            ?TDEF_FE(t_matcher_on_nonce)
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
    Req = #httpd{method=Method, nonce=Nonce},
    {_, _} = PidRef = csrt:create_coordinator_context(Req, Path),
    MArgs = #mrargs{include_docs = false},
    _Res = fabric:all_docs(DbName, [?ADMIN_CTX], fun view_cb/2, [], MArgs),
    Rctx = load_rctx(PidRef),
    ok = config:set("csrt_logger.matchers_threshold", "docs_read", integer_to_list(?THRESHOLD_DOCS_READ), false),
    ok = config:set("csrt_logger.matchers_threshold", "ioq_calls", integer_to_list(?THRESHOLD_IOQ_CALLS), false),
    ok = config:set("csrt_logger.matchers_threshold", "rows_read", integer_to_list(?THRESHOLD_ROWS_READ), false),
    ok = config:set("csrt_logger.matchers_threshold", "worker_changes_processed", integer_to_list(?THRESHOLD_CHANGES), false),
    ok = config:set("csrt_logger.dbnames_io", "foo", integer_to_list(?THRESHOLD_DBNAME_IO), false),
    ok = config:set("csrt_logger.dbnames_io", "bar", integer_to_list(?THRESHOLD_DBNAME_IO), false),
    ok = config:set("csrt_logger.dbnames_io", "foo/bar", integer_to_list(?THRESHOLD_DBNAME_IO), false),
    csrt_logger:reload_matchers(),
    #{ctx => Ctx, dbname => DbName, rctx => Rctx, rctxs => rctxs()}.

teardown(#{ctx := Ctx, dbname := DbName}) ->
    ok = fabric:delete_db(DbName, [?ADMIN_CTX]),
    ok = meck:unload(ioq),
    test_util:stop_couch(Ctx).

rctx_gen() ->
    rctx_gen(#{}).

rctx_gen(Opts0) ->
    DbnameGen = one_of([<<"foo">>, <<"bar">>, ?tempdb]),
    TypeGen = one_of([?RCTX_RPC, ?RCTX_COORDINATOR]),
    R = fun() -> rand:uniform(?RCTX_RANGE) end,
    R10 = fun() -> 3 + rand:uniform(round(?RCTX_RANGE / 10)) end,
    Occasional = one_of([0, 0, 0, 0, 0, R]),
    Nonce = one_of(["9c54fa9283", "foobar7799", lists:duplicate(10, fun nonce/0)]),
    Base = #{
        dbname => DbnameGen,
        db_open => R10,
        docs_read => R,
        docs_written => Occasional,
        get_kp_node => R10,
        get_kv_node => R,
        nonce => Nonce,
        pid_ref => {self(), make_ref()},
        ioq_calls => R,
        rows_read => R,
        type => TypeGen,
        '_do_changes' => true %% Hack because we need to modify both fields
    },
    Opts = maps:merge(Base, Opts0),
    csrt_util:map_to_rctx(maps:fold(
        fun
            %% Hack for changes because we need to modify both changes_processed
            %% and changes_returned but the latter must be <= the former
            ('_do_changes', V, Acc) ->
                case V of
                    true ->
                        Processed = R(),
                        Returned = (one_of([0, 0, 1, Processed, rand:uniform(Processed)]))(),
                        maps:put(
                            changes_processed,
                            Processed,
                            maps:put(changes_returned, Returned, Acc));
                    _ ->
                        Acc
                end;
            (K, F, Acc) when is_function(F) ->
                maps:put(K, F(), Acc);
            (K, V, Acc) ->
                maps:put(K, V, Acc)
        end, #{}, Opts
    )).

rctxs() ->
    [rctx_gen() || _ <- lists:seq(1, ?RCTX_COUNT)].

t_do_report(#{rctx := Rctx}) ->
    JRctx = csrt_util:to_json(Rctx),
    ReportName = "foo",
    ok = meck:new(couch_log),
    ok = meck:expect(couch_log, report, fun(_, _) -> true end),
    ?assert(csrt_logger:do_report(ReportName, Rctx), "CSRT _logger:do_report " ++ ReportName),
    ?assert(meck:validate(couch_log), "CSRT do_report"),
    ?assert(meck:validate(couch_log), "CSRT validate couch_log"),
    ?assert(
        meck:called(couch_log, report, [ReportName, JRctx]),
        "CSRT couch_log:report"
    ),
    ok = meck:unload(couch_log).

t_do_lifetime_report(#{rctx := Rctx}) ->
    JRctx = csrt_util:to_json(Rctx),
    ReportName = "csrt-pid-usage-lifetime",
    ok = meck:new(couch_log),
    ok = meck:expect(couch_log, report, fun(_, _) -> true end),
    ?assert(
        csrt_logger:do_lifetime_report(Rctx),
        "CSRT _logger:do_report " ++ ReportName
    ),
    ?assert(meck:validate(couch_log), "CSRT validate couch_log"),
    ?assert(
        meck:called(couch_log, report, [ReportName, JRctx]),
        "CSRT couch_log:report"
    ),
    ok = meck:unload(couch_log).

t_do_status_report(#{rctx := Rctx}) ->
    JRctx = csrt_util:to_json(Rctx),
    ReportName = "csrt-pid-usage-status",
    ok = meck:new(couch_log),
    ok = meck:expect(couch_log, report, fun(_, _) -> true end),
    ?assert(csrt_logger:do_status_report(Rctx), "csrt_logger:do_ " ++ ReportName),
    ?assert(meck:validate(couch_log), "CSRT validate couch_log"),
    ?assert(
        meck:called(couch_log, report, [ReportName, JRctx]),
        "CSRT couch_log:report"
    ),
    ok = meck:unload(couch_log).

t_matcher_on_dbname(#{rctx := _Rctx, rctxs := Rctxs0}) ->
    %% Make sure we have at least one match
    Rctxs = [rctx_gen(#{dbname => <<"foo">>}) | Rctxs0],
    ?assertEqual(
        lists:sort(lists:filter(matcher_on(dbname, <<"foo">>), Rctxs)),
        lists:sort(lists:filter(matcher_for_csrt("dbname"), Rctxs)),
        "Dbname matcher on <<\"foo\">>"
    ).

t_matcher_on_docs_read(#{rctxs := Rctxs0}) ->
    Threshold = ?THRESHOLD_DOCS_READ,
    %% Make sure we have at least one match
    Rctxs = [rctx_gen(#{docs_read => Threshold + 10}) | Rctxs0],
    ?assertEqual(
        lists:sort(lists:filter(matcher_gte(docs_read, Threshold), Rctxs)),
        lists:sort(lists:filter(matcher_for_csrt("docs_read"), Rctxs)),
        "Docs read matcher"
    ).

t_matcher_on_docs_written(#{rctxs := Rctxs0}) ->
    %% Make sure we have at least one match
    Rctxs = [rctx_gen(#{docs_written => 73}) | Rctxs0],
    ?assertEqual(
        lists:sort(lists:filter(matcher_gte(docs_written, 1), Rctxs)),
        lists:sort(lists:filter(matcher_for_csrt("docs_written"), Rctxs)),
        "Docs written matcher"
    ).

t_matcher_on_rows_read(#{rctxs := Rctxs0}) ->
    Threshold = ?THRESHOLD_ROWS_READ,
    %% Make sure we have at least one match
    Rctxs = [rctx_gen(#{rows_read => Threshold + 10}) | Rctxs0],
    ?assertEqual(
        lists:sort(lists:filter(matcher_gte(rows_read, Threshold), Rctxs)),
        lists:sort(lists:filter(matcher_for_csrt("rows_read"), Rctxs)),
        "Rows read matcher"
    ).

t_matcher_on_worker_changes_processed(#{rctxs := Rctxs0}) ->
    Threshold = ?THRESHOLD_CHANGES,
    %% Make sure we have at least one match
    Rctxs = [rctx_gen(#{changes_processed => Threshold + 10}) | Rctxs0],
    ChangesFilter = fun(R) ->
        Ret = csrt_util:field(changes_returned, R),
        Proc = csrt_util:field(changes_processed, R),
        (Proc - Ret) >= Threshold
    end,
    ?assertEqual(
        lists:sort(lists:filter(ChangesFilter, Rctxs)),
        lists:sort(lists:filter(matcher_for_csrt("worker_changes_processed"), Rctxs)),
        "Changes processed matcher"
    ).

t_matcher_on_ioq_calls(#{rctxs := Rctxs0}) ->
    Threshold = ?THRESHOLD_IOQ_CALLS,
    %% Make sure we have at least one match
    Rctxs = [rctx_gen(#{ioq_calls => Threshold + 10}) | Rctxs0],
    ?assertEqual(
        lists:sort(lists:filter(matcher_gte(ioq_calls, Threshold), Rctxs)),
        lists:sort(lists:filter(matcher_for_csrt("ioq_calls"), Rctxs)),
        "IOQ calls matcher"
    ).

t_matcher_on_nonce(#{rctxs := Rctxs0}) ->
    Nonce = "foobar7799",
    %% Make sure we have at least one match
    Rctxs = [rctx_gen(#{nonce => Nonce}) | Rctxs0],
    %% Nonce requires dynamic matcher as it's a static match
    %% TODO: add pattern based nonce matching
    MSpec = csrt_logger:matcher_on_nonce(Nonce),
    CompMSpec = ets:match_spec_compile(MSpec),
    Matchers = #{"nonce" => {MSpec, CompMSpec}},
    IsMatch = fun(ARctx) -> csrt_logger:is_match(ARctx, Matchers) end,
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
    MatcherFoo = matcher_for_csrt("dbname_io__" ++ DbFoo ++ "__" ++ SThreshold),
    MatcherBar = matcher_for_csrt("dbname_io__" ++ DbBar ++ "__" ++ SThreshold),
    MatcherFooBar = matcher_for_csrt("dbname_io__foo/bar__" ++ SThreshold),
    %% Add an extra Rctx with dbname foo/bar to ensure correct naming matches
    ExtraRctx = rctx_gen(#{dbname => <<"foo/bar">>, get_kp_node => Threshold + 10}),
    %% Make sure we have at least one match
    Rctxs = [ExtraRctx, rctx_gen(#{ioq_calls => Threshold + 10}) | Rctxs0],
    ?assertEqual(
        lists:sort(lists:filter(matcher_for_dbname_io(DbFoo, Threshold), Rctxs)),
        lists:sort(lists:filter(MatcherFoo, Rctxs)),
        "dbname_io foo matcher"
    ),
    ?assertEqual(
        lists:sort(lists:filter(matcher_for_dbname_io(DbBar, Threshold), Rctxs)),
        lists:sort(lists:filter(MatcherBar, Rctxs)),
        "dbname_io bar matcher"
    ),
    ?assertEqual(
        [ExtraRctx],
        lists:sort(lists:filter(MatcherFooBar, Rctxs)),
        "dbname_io foo/bar matcher"
    ).

load_rctx(PidRef) ->
    timer:sleep(50), %% Add slight delay to accumulate RPC response deltas
    csrt:get_resource(PidRef).

view_cb({row, Row}, Acc) ->
    {ok, [Row | Acc]};
view_cb(_Msg, Acc) ->
    {ok, Acc}.

matcher_gte(Field, Value) ->
    matcher_for(Field, Value, fun erlang:'>='/2).

matcher_on(Field, Value) ->
    matcher_for(Field, Value, fun erlang:'=:='/2).

matcher_for(Field, Value, Op) ->
    fun(Rctx) -> Op(csrt_util:field(Field, Rctx), Value) end.

matcher_for_csrt(MatcherName) ->
    Matchers = #{MatcherName => {_, _} = csrt_logger:get_matcher(MatcherName)},
    fun(Rctx) -> csrt_logger:is_match(Rctx, Matchers) end.

matcher_for_dbname_io(Dbname0, Threshold) ->
    Dbname = list_to_binary(Dbname0),
    fun(Rctx) ->
        DbnameA = csrt_util:field(dbname, Rctx),
        Fields = [ioq_calls, get_kv_node, get_kp_node, docs_read, rows_read, changes_processed],
        Vals = [{F, csrt_util:field(F, Rctx)} || F <- Fields],
        Dbname =:= mem3:dbname(DbnameA) andalso lists:any(fun(V) -> V >= Threshold end, Vals)
    end.

nonce() ->
    couch_util:to_hex(crypto:strong_rand_bytes(5)).

one_of(L) ->
    fun() ->
        case lists:nth(rand:uniform(length(L)), L) of
            F when is_function(F) ->
                F();
            N ->
                N
        end
    end.
