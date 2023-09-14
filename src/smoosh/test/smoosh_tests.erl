-module(smoosh_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

smoosh_test_() ->
    {
        setup,
        fun setup_all/0,
        fun teardown_all/1,
        {
            foreach,
            fun setup/0,
            fun teardown/1,
            [
                ?TDEF_FE(t_default_channels),
                ?TDEF_FE(t_channels_recreated_on_crash),
                ?TDEF_FE(t_can_create_and_delete_channels),
                ?TDEF_FE(t_db_is_enqueued_and_compacted),
                ?TDEF_FE(t_view_is_enqueued_and_compacted),
                ?TDEF_FE(t_index_cleanup_happens_by_default),
                ?TDEF_FE(t_index_cleanup_can_be_disabled, 10),
                ?TDEF_FE(t_suspend_resume),
                ?TDEF_FE(t_check_window_can_resume),
                ?TDEF_FE(t_renqueue_on_crashes),
                ?TDEF_FE(t_update_status_works),
                ?TDEF_FE(t_checkpointing_works, 10),
                ?TDEF_FE(t_ignore_checkpoint_resume_if_compacted_already, 10),
                ?TDEF_FE(t_access_cleaner_restarts),
                ?TDEF_FE(t_event_handler_restarts),
                ?TDEF_FE(t_manual_enqueue_api_works),
                ?TDEF_FE(t_access_cleaner_works)
            ]
        }
    }.

setup_all() ->
    meck:new(smoosh_server, [passthrough]),
    meck:new(smoosh_channel, [passthrough]),
    meck:new(file, [unstick, passthrough]),
    meck:new(fabric, [passthrough]),
    meck:new(couch_emsort, [passthrough]),
    Ctx = test_util:start_couch([fabric]),
    config:set("query_server_config", "commit_freq", "0", false),
    Ctx.

teardown_all(Ctx) ->
    catch application:stop(smoosh),
    config:delete("query_server_config", "commit_freq", false),
    test_util:stop(Ctx),
    meck:unload().

setup() ->
    config:set("smoosh", "persist", "false", false),
    config:set("smoosh", "wait_secs", "9999", false),
    DbName = ?tempdb(),
    fabric:create_db(DbName, [{q, 1}]),
    {ok, _} = create_ddoc(DbName, <<"_design/foo">>, <<"bar">>),
    {ok, _} = create_doc(DbName, <<"doc1">>, 1100000),
    {ok, _} = fabric:query_view(DbName, <<"foo">>, <<"bar">>),
    application:start(smoosh),
    wait_for_channels(),
    flush(),
    get_channel_pid("ratio_dbs") ! unpause,
    DbName.

teardown(DbName) ->
    catch flush(),
    catch application:stop(smoosh),
    fabric:delete_db(DbName),
    meck:reset(smoosh_server),
    meck:reset(smoosh_channel),
    meck:reset(couch_emsort),
    meck:reset(fabric),
    config:delete("smoosh", "db_channels", false),
    config:delete("smoosh.ratio_dbs", "min_priority", false),
    config:delete("smoosh.ratio_views", "min_priority", false),
    config:delete("smoosh", "view_channels", false),
    config:delete("smoosh", "cleanup_channels", false),
    config:delete("smoosh", "wait_secs", false),
    config:delete("smoosh", "persist", false),
    config:delete("smoosh", "cleanup_index_files", false).

t_default_channels(_) ->
    ChannelStatus = maps:get(channels, status()),
    ?assertMatch(
        [
            index_cleanup,
            ratio_dbs,
            ratio_views,
            slack_dbs,
            slack_views,
            upgrade_dbs,
            upgrade_views
        ],
        lists:sort(maps:keys(ChannelStatus))
    ),
    % If app hasn't started status won't crash
    application:stop(smoosh),
    ?assertEqual(#{channels => #{}}, status()).

t_channels_recreated_on_crash(_) ->
    RatioDbsPid = get_channel_pid("ratio_dbs"),
    meck:reset(smoosh_channel),
    exit(RatioDbsPid, kill),
    meck:wait(1, smoosh_channel, start_link, 1, 3000),
    wait_for_channels(7),
    ChannelStatus = maps:get(channels, status()),
    ?assertMatch(true, maps:is_key(ratio_dbs, ChannelStatus)),
    ?assertNotEqual(RatioDbsPid, get_channel_pid("ratio_dbs")).

t_can_create_and_delete_channels(_) ->
    config:set("smoosh", "db_channels", "mychan1", false),
    config:set("smoosh", "view_channels", "mychan2", false),
    config:set("smoosh", "cleanup_channels", "mychan3", false),
    % 7 default ones + 3 new ones
    wait_for_channels(10),
    meck:reset(smoosh_channel),
    config:delete("smoosh", "db_channels", false),
    config:delete("smoosh", "view_channels", false),
    config:delete("smoosh", "cleanup_channels", false),
    wait_for_channels(7).

t_db_is_enqueued_and_compacted(DbName) ->
    ?assertEqual({0, 0, 0}, sync_status("ratio_dbs")),
    meck:reset(smoosh_channel),
    {ok, _} = delete_doc(DbName, <<"doc1">>),
    ok = wait_to_enqueue(DbName),
    wait_compacted(DbName).

t_view_is_enqueued_and_compacted(DbName) ->
    % We don't want index cleanup to interfere for now
    config:set("smoosh", "cleanup_index_files", "false", false),
    % Ensure db is compacted
    meck:reset(smoosh_channel),
    {ok, _} = delete_doc(DbName, <<"doc1">>),
    wait_compacted(DbName),
    % Check view
    meck:reset(smoosh_channel),
    get_channel_pid("ratio_views") ! unpause,
    {ok, _} = fabric:query_view(DbName, <<"foo">>, <<"bar">>),
    ok = wait_to_enqueue({DbName, <<"_design/foo">>}),
    wait_view_compacted(DbName, <<"foo">>).

t_index_cleanup_happens_by_default(DbName) ->
    ?assert(config:get_boolean("smoosh", "cleanup_index_files", true)),
    % Db compacts
    meck:reset(smoosh_channel),
    {ok, _} = delete_doc(DbName, <<"doc1">>),
    wait_compacted(DbName),
    % View should compact as well
    meck:reset(fabric),
    meck:reset(smoosh_channel),
    get_channel_pid("ratio_views") ! unpause,
    get_channel_pid("index_cleanup") ! unpause,
    {ok, _} = fabric:query_view(DbName, <<"foo">>, <<"bar">>),
    % View cleanup should have been invoked
    meck:wait(fabric, cleanup_index_files, [DbName], 4000),
    wait_view_compacted(DbName, <<"foo">>).

t_index_cleanup_can_be_disabled(DbName) ->
    config:set("smoosh", "cleanup_index_files", "false", false),
    % Db compacts
    meck:reset(smoosh_channel),
    {ok, _} = delete_doc(DbName, <<"doc1">>),
    wait_compacted(DbName),
    % View should compact as well
    meck:reset(fabric),
    meck:reset(smoosh_channel),
    get_channel_pid("ratio_views") ! unpause,
    get_channel_pid("index_cleanup") ! unpause,
    {ok, _} = fabric:query_view(DbName, <<"foo">>, <<"bar">>),
    wait_view_compacted(DbName, <<"foo">>),
    % View cleanup was not called
    timer:sleep(500),
    ?assertEqual(0, meck:num_calls(fabric, cleanup_index_files, 1)).

t_suspend_resume(DbName) ->
    ?assertEqual({0, 0, 0}, sync_status("ratio_dbs")),
    meck:reset(smoosh_channel),
    setup_db_compactor_intercept(),
    {ok, _} = delete_doc(DbName, <<"doc1">>),
    ok = wait_to_enqueue(DbName),
    CompPid = wait_db_compactor_pid(),
    ok = smoosh:suspend(),
    ?assertEqual({status, suspended}, erlang:process_info(CompPid, status)),
    ?assertEqual({1, 0, 0}, sync_status("ratio_dbs")),
    % Suspending twice should work too
    ok = smoosh:suspend(),
    ?assertEqual({status, suspended}, erlang:process_info(CompPid, status)),
    ?assertEqual({1, 0, 0}, sync_status("ratio_dbs")),
    ok = smoosh:resume(),
    ?assertNotEqual({status, suspended}, erlang:process_info(CompPid, status)),
    % Resuming twice should work too
    ok = smoosh:resume(),
    ?assertNotEqual({status, suspended}, erlang:process_info(CompPid, status)),
    CompPid ! continue,
    wait_compacted(DbName).

t_check_window_can_resume(DbName) ->
    ?assertEqual({0, 0, 0}, sync_status("ratio_dbs")),
    meck:reset(smoosh_channel),
    setup_db_compactor_intercept(),
    {ok, _} = delete_doc(DbName, <<"doc1">>),
    ok = wait_to_enqueue(DbName),
    CompPid = wait_db_compactor_pid(),
    ok = smoosh:suspend(),
    ?assertEqual({status, suspended}, erlang:process_info(CompPid, status)),
    get_channel_pid("ratio_dbs") ! check_window,
    CompPid ! continue,
    wait_compacted(DbName).

t_renqueue_on_crashes(DbName) ->
    ?assertEqual({0, 0, 0}, sync_status("ratio_dbs")),
    meck:reset(smoosh_channel),
    setup_db_compactor_intercept(),
    {ok, _} = delete_doc(DbName, <<"doc1">>),
    ok = wait_to_enqueue(DbName),
    CompPid = wait_db_compactor_pid(),
    meck:reset(smoosh_channel),
    CompPid ! {raise, error, boom},
    ok = wait_to_enqueue(DbName),
    CompPid2 = wait_db_compactor_pid(),
    CompPid2 ! continue,
    wait_compacted(DbName).

t_update_status_works(DbName) ->
    setup_db_compactor_intercept(),
    {ok, _} = delete_doc(DbName, <<"doc1">>),
    ok = wait_to_enqueue(DbName),
    CompPid = wait_db_compactor_pid(),
    % status should have 1 starting job, but it may have not been updated yet so
    % we wait until update_status is called
    wait_update_status(),
    WaitFun = fun() ->
        case {1, 0, 0} =:= status("ratio_dbs") of
            true -> ok;
            _ -> wait
        end
    end,
    test_util:wait(WaitFun),
    CompPid ! continue,
    wait_compacted(DbName).

t_checkpointing_works(DbName) ->
    setup_db_compactor_intercept(),
    {ok, _} = delete_doc(DbName, <<"doc1">>),
    ok = wait_to_enqueue(DbName),
    CompPid = wait_db_compactor_pid(),
    config:set("smoosh", "persist", "true", false),
    meck:reset(smoosh_channel),
    checkpoint(),
    % Stop smoosh and then crash the compaction
    ok = application:stop(smoosh),
    Ref = erlang:monitor(process, CompPid),
    CompPid ! {raise, exit, kapow},
    receive
        {'DOWN', Ref, _, _, kapow} ->
            ok
    end,
    ok = application:start(smoosh),
    wait_for_channels(),
    get_channel_pid("ratio_dbs") ! unpause,
    CompPid2 = wait_db_compactor_pid(),
    ?assertEqual({1, 0, 0}, sync_status("ratio_dbs")),
    CompPid2 ! continue,
    wait_compacted(DbName).

t_ignore_checkpoint_resume_if_compacted_already(DbName) ->
    setup_db_compactor_intercept(),
    {ok, _} = delete_doc(DbName, <<"doc1">>),
    ok = wait_to_enqueue(DbName),
    CompPid = wait_db_compactor_pid(),
    config:set("smoosh", "persist", "true", false),
    meck:reset(smoosh_channel),
    checkpoint(),
    % Stop smoosh and then let the compaction finish
    ok = application:stop(smoosh),
    Ref = erlang:monitor(process, CompPid),
    CompPid ! continue,
    receive
        {'DOWN', Ref, _, _, normal} -> ok
    end,
    wait_compacted(DbName),
    % Smoosh should resume job and *not* compact
    meck:reset(smoosh_channel),
    ok = application:start(smoosh),
    wait_for_channels(),
    get_channel_pid("ratio_dbs") ! unpause,
    timer:sleep(500),
    StartPat = {'_', {ok, '_'}},
    ?assertEqual(0, meck:num_calls(smoosh_channel, handle_info, [StartPat, '_'])).

t_access_cleaner_restarts(_) ->
    ACPid = get_access_cleaner_pid(),
    exit(ACPid, kill),
    WaitFun = fun() ->
        case get_access_cleaner_pid() of
            Pid when Pid =/= ACPid -> ok;
            _ -> wait
        end
    end,
    test_util:wait(WaitFun),
    ?assertNotEqual(ACPid, get_access_cleaner_pid()).

t_event_handler_restarts(_) ->
    EHPid = get_event_handler_pid(),
    exit(EHPid, kill),
    WaitFun = fun() ->
        case get_event_handler_pid() of
            Pid when Pid =/= EHPid -> ok;
            _ -> wait
        end
    end,
    test_util:wait(WaitFun),
    ?assertNotEqual(EHPid, get_access_cleaner_pid()).

t_manual_enqueue_api_works(DbName) ->
    Shard = shard_name(DbName),

    SmooshPid = whereis(smoosh_server),
    RatioDbsPid = get_channel_pid("ratio_dbs"),
    RatioViewsPid = get_channel_pid("ratio_views"),
    CleanupPid = get_channel_pid("index_cleanup"),

    % Lower min priority so that enqueued shards would try to compact
    config:set("smoosh.ratio_dbs", "min_priority", "1", false),
    config:set("smoosh.ratio_views", "min_priority", "1", false),

    ?assertEqual(ok, smoosh_server:sync_enqueue(<<"invalid">>)),
    ?assertEqual(ok, smoosh_server:sync_enqueue({index_cleanup, <<"invalid">>})),
    ?assertEqual(ok, smoosh_server:sync_enqueue({Shard, <<"_design/invalid">>})),

    ?assertEqual(ok, smoosh_server:sync_enqueue(Shard)),
    ?assertEqual(ok, smoosh_server:sync_enqueue({index_cleanup, Shard})),
    ?assertEqual(ok, smoosh_server:sync_enqueue({Shard, <<"_design/foo">>})),

    ?assertEqual(ok, smoosh:enqueue(Shard)),
    ?assertEqual(ok, smoosh:enqueue({index_cleanup, Shard})),
    ?assertEqual(ok, smoosh:enqueue({Shard, <<"_design/foo">>})),

    smoosh:enqueue_all_dbs(),
    smoosh:enqueue_all_views(),

    % Enqueuing the same items in a loop should work
    lists:foreach(
        fun(_) ->
            ?assertEqual(ok, smoosh:enqueue(Shard)),
            ?assertEqual(ok, smoosh:enqueue({index_cleanup, Shard})),
            ?assertEqual(ok, smoosh:enqueue({Shard, <<"_design/foo">>}))
        end,
        lists:seq(1, 1000)
    ),

    ?assertEqual(ok, smoosh_server:sync_enqueue(Shard)),
    ?assertEqual(ok, smoosh_server:sync_enqueue({index_cleanup, Shard})),
    ?assertEqual(ok, smoosh_server:sync_enqueue({Shard, <<"_design/foo">>})),

    % Assert that channels and smoosh server didn't crash
    ?assertEqual(SmooshPid, whereis(smoosh_server)),
    ?assertEqual(RatioDbsPid, get_channel_pid("ratio_dbs")),
    ?assertEqual(RatioViewsPid, get_channel_pid("ratio_views")),
    ?assertEqual(CleanupPid, get_channel_pid("index_cleanup")).

t_access_cleaner_works(_) ->
    Now = erlang:monotonic_time(second),
    ets:insert(smoosh_access, {<<"db1">>, Now - 3600}),
    WaitFun = fun() ->
        case ets:tab2list(smoosh_access) == [] of
            true -> ok;
            _ -> wait
        end
    end,
    test_util:wait(WaitFun),
    ?assertEqual([], ets:tab2list(smoosh_access)).

create_doc(DbName, DocId, Size) ->
    Data = b64url:encode(crypto:strong_rand_bytes(Size)),
    Doc = #doc{id = DocId, body = {[{<<"value">>, Data}]}},
    fabric:update_doc(DbName, Doc, [?ADMIN_CTX]).

create_ddoc(DbName, DocId, ViewName) ->
    MapFun = <<"function(doc) {emit(doc._id, doc.value);}">>,
    DDoc = couch_doc:from_json_obj(
        {[
            {<<"_id">>, DocId},
            {<<"language">>, <<"javascript">>},
            {<<"autoupdate">>, false},
            {<<"views">>,
                {[
                    {ViewName,
                        {[
                            {<<"map">>, MapFun}
                        ]}}
                ]}}
        ]}
    ),
    fabric:update_doc(DbName, DDoc, [?ADMIN_CTX]).

delete_doc(DbName, DDocId) ->
    {ok, DDoc0} = fabric:open_doc(DbName, DDocId, [?ADMIN_CTX]),
    DDoc = DDoc0#doc{deleted = true, body = {[]}},
    fabric:update_doc(DbName, DDoc, [?ADMIN_CTX]).

status() ->
    {ok, Props} = smoosh:status(),
    Props.

status(Channel) ->
    ChannelStatus = maps:get(channels, status()),
    ChannelAtom = list_to_atom(Channel),
    case maps:is_key(ChannelAtom, ChannelStatus) of
        true ->
            #{active := Active, starting := Starting, waiting := Waiting} = maps:get(
                ChannelAtom, ChannelStatus
            ),
            {Active, Starting, maps:get(size, Waiting)};
        false ->
            false
    end.

sync_status(Channel) ->
    Pid = get_channel_pid(Channel),
    gen_server:call(Pid, get_status_table, infinity),
    status(Channel).

flush() ->
    ok = smoosh_server:flush().

get_channel_pid(Chan) ->
    [{channel, _, Pid, _}] = ets:lookup(smoosh_server, Chan),
    Pid.

get_access_cleaner_pid() ->
    {state, _, _, _, _, _, _, ACPid} = sys:get_state(smoosh_server),
    ACPid.

get_event_handler_pid() ->
    {state, _, _, _, EHPid, _, _, _} = sys:get_state(smoosh_server),
    EHPid.

wait_for_channels() ->
    % 3 default ratios + 3 default views + 1 index cleanup = 7
    wait_for_channels(7).

wait_for_channels(N) when is_integer(N), N >= 0 ->
    WaitFun = fun() ->
        ChannelStatus = maps:get(channels, status()),
        case length(maps:keys(ChannelStatus)) of
            N -> ok;
            _ -> wait
        end
    end,
    test_util:wait(WaitFun).

wait_to_enqueue(DbName) when is_binary(DbName) ->
    wait_enqueue(shard_name(DbName));
wait_to_enqueue({DbName, View}) when is_binary(DbName) ->
    wait_enqueue({shard_name(DbName), View});
wait_to_enqueue({index_cleanup, DbName}) when is_binary(DbName) ->
    wait_enqueue({index_cleanup, DbName}).

wait_enqueue(Obj) ->
    Enqueue = {enqueue, Obj, '_'},
    meck:wait(smoosh_channel, handle_cast, [Enqueue, '_'], 4000).

shard_name(DbName) ->
    [Shard] = mem3:shards(DbName),
    mem3:name(Shard).

wait_update_status() ->
    meck:wait(smoosh_channel, handle_info, [update_status, '_'], 4000).

setup_db_compactor_intercept() ->
    TestPid = self(),
    meck:expect(couch_emsort, open, fun(Fd) ->
        TestPid ! {compactor_paused, self()},
        receive
            continue -> meck:passthrough([Fd]);
            {raise, Tag, Reason} -> meck:exception(Tag, Reason)
        end
    end).

wait_db_compactor_pid() ->
    receive
        {compactor_paused, Pid} ->
            Pid
    end.

checkpoint() ->
    ChanPid = get_channel_pid("ratio_dbs"),
    meck:reset(file),
    ChanPid ! checkpoint,
    meck:wait(1, file, write_file, 3, 2000),
    meck:wait(1, file, rename, 2, 2000).

file_size(DbName) ->
    {ok, Info} = fabric:get_db_info(DbName),
    {Sizes} = couch_util:get_value(sizes, Info),
    couch_util:get_value(file, Sizes).

view_file_size(DbName, DDoc) ->
    {ok, ViewInfo} = fabric:get_view_group_info(DbName, DDoc),
    {Sizes} = couch_util:get_value(sizes, ViewInfo),
    couch_util:get_value(file, Sizes).

wait_compacted(DbName) ->
    wait_compacted(DbName, 32000).

wait_compacted(DbName, MinSize) ->
    WaitFun = fun() ->
        Size = file_size(DbName),
        case Size =< MinSize of
            true -> ok;
            false -> wait
        end
    end,
    test_util:wait(WaitFun, 5000, 100).

wait_view_compacted(DbName, DDoc) ->
    wait_view_compacted(DbName, DDoc, 64000).

wait_view_compacted(DbName, DDoc, MinSize) ->
    WaitFun = fun() ->
        Size = view_file_size(DbName, DDoc),
        case Size =< MinSize of
            true -> ok;
            false -> wait
        end
    end,
    test_util:wait(WaitFun, 5000, 200).
