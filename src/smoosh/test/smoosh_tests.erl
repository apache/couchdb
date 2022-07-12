-module(smoosh_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

%% ==========
%% Setup
%% ----------

setup(ChannelType) ->
    DbName = ?tempdb(),
    {ok, Db} = couch_db:create(DbName, [?ADMIN_CTX]),
    couch_db:close(Db),
    {ok, ChannelPid} = smoosh_server:get_channel(ChannelType),
    smoosh_channel:flush(ChannelPid),
    ok = config:set("smoosh", "persist", "true", false),
    ok = config:set(config_section(ChannelType), "min_size", "1", false),
    ok = config:set(config_section(ChannelType), "min_priority", "1", false),
    DbName.

teardown(ChannelType, DbName) ->
    ok = couch_server:delete(DbName, [?ADMIN_CTX]),
    ok = config:delete("smoosh", "persist", false),
    ok = config:delete(config_section(DbName), "min_size", false),
    ok = config:delete(config_section(DbName), "min_priority", false),
    {ok, ChannelPid} = smoosh_server:get_channel(ChannelType),
    smoosh_channel:flush(ChannelPid),
    ok.

config_section(ChannelType) ->
    "smoosh." ++ ChannelType.

%% ==========
%% Tests
%% ----------

smoosh_test_() ->
    {
        "Testing smoosh",
        {
            setup,
            fun() -> test_util:start_couch([smoosh]) end,
            fun test_util:stop/1,
            [
                channels_tests(),
                persistence_tests()
            ]
        }
    }.

persistence_tests() ->
    Tests = [
        fun should_persist_queue/2
    ],
    {
        "Should persist queue state",
        [
            make_test_case("ratio_dbs", Tests)
        ]
    }.

channels_tests() ->
    Tests = [
        fun should_enqueue/2,
        fun should_start_compact/2
    ],
    {
        "Various channels tests",
        [
            make_test_case("ratio_dbs", Tests)
        ]
    }.

make_test_case(Type, Funs) ->
    {foreachx, fun setup/1, fun teardown/2, [{Type, Fun} || Fun <- Funs]}.

should_enqueue(ChannelType, DbName) ->
    ?_test(begin
        ok = grow_db_file(DbName, 300),
        ok = wait_enqueue(ChannelType, DbName),
        ?assert(is_enqueued(ChannelType, DbName)),
        ok
    end).

should_persist_queue(ChannelType, DbName) ->
    ?_test(begin
        {ok, ChannelPid} = smoosh_server:get_channel(ChannelType),
        ok = grow_db_file(DbName, 300),
        ok = wait_enqueue(ChannelType, DbName),
        ok = smoosh_channel:persist(ChannelPid),
        Q0 = channel_queue(ChannelType),
        ok = application:stop(smoosh),
        ok = application:start(smoosh),
        Q1 = channel_queue(ChannelType),
        % Assert that queues are not empty
        ?assertNotEqual(Q0, smoosh_priority_queue:new(ChannelType)),
        ?assertNotEqual(Q1, smoosh_priority_queue:new(ChannelType)),
        ?assertEqual(Q0, Q1),
        ok
    end).

should_start_compact(ChannelType, DbName) ->
    ?_test(begin
        {ok, ChannelPid} = smoosh_server:get_channel(ChannelType),
        ok = grow_db_file(DbName, 3000),
        smoosh_channel:resume(ChannelPid),
        ok = wait_compact(ChannelType),
        ?assertEqual(true, couch_db:is_compacting(DbName)),
        application:start(smoosh),
        ok
    end).

grow_db_file(DbName, SizeInKb) ->
    {ok, Db} = couch_db:open_int(DbName, [?ADMIN_CTX]),
    Data = b64url:encode(crypto:strong_rand_bytes(SizeInKb * 1024)),
    Body = {[{<<"value">>, Data}]},
    Doc = #doc{id = <<"doc1">>, body = Body},
    {ok, _} = couch_db:update_doc(Db, Doc, []),
    couch_db:close(Db),
    ok.

is_enqueued(ChannelType, DbName) ->
    {ok, ChannelPid} = smoosh_server:get_channel(ChannelType),
    smoosh_channel:is_key(ChannelPid, DbName).

wait_enqueue(ChannelType, DbName) ->
    test_util:wait(fun() ->
        case is_enqueued(ChannelType, DbName) of
            false ->
                wait;
            true ->
                ok
        end
    end).

wait_compact(ChannelType) ->
    {ok, ChannelPid} = smoosh_server:get_channel(ChannelType),
    test_util:wait(
        fun() ->
            {ok, Status} = smoosh_channel:get_status(ChannelPid),
            {active, Active} = lists:keyfind(active, 1, Status),
            case Active of
                1 ->
                    application:stop(smoosh),
                    ok;
                _ ->
                    wait
            end
        end,
        10000
    ).

channel_queue(ChannelType) ->
    Q0 = smoosh_priority_queue:new(ChannelType),
    smoosh_priority_queue:recover(Q0).
