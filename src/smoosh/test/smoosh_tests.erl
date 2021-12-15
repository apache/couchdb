-module(smoosh_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-include("couch/src/couch_db_int.hrl").

-define(KILOBYTE, binary:copy(<<"x">>, 1024)).

%% ==========
%% Setup
%% ----------

setup(ChannelType) ->
    DbName = ?tempdb(),
    {ok, Db} = couch_db:create(DbName, [?ADMIN_CTX]),
    couch_db:close(Db),
    {ok, ChannelPid} = smoosh_server:get_channel(ChannelType),
    smoosh_channel:flush(ChannelPid),
    ok = config:set(config_section(ChannelType), "min_size", "200000", false),
    ok = config:set("smoosh", "state_checkpoint_interval_in_sec", "1", false),
    DbName.

teardown(ChannelType, DbName) ->
    ok = couch_server:delete(DbName, [?ADMIN_CTX]),
    ok = config:delete(config_section(DbName), "min_size", false),
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
                persistence_tests(),
                channels_tests()
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
        fun should_enqueue/2
    ],
    {
        "Various channels tests",
        [
            make_test_case("ratio_dbs", Tests)
        ]
    }.

make_test_case(Type, Funs) ->
    {foreachx, fun setup/1, fun teardown/2, [{Type, Fun} || Fun <- Funs]}.

should_enqueue("ratio_dbs" = ChannelType, DbName) ->
    ?_test(begin
        ok = grow_db_file(DbName, 300),
        ok = wait_enqueue(ChannelType, DbName),
        ?assert(is_enqueued(ChannelType, DbName)),
        ok
    end).

should_persist_queue(ChannelType, DbName) ->
    ?_test(begin
        ok = grow_db_file(DbName, 300),
        ok = wait_enqueue(ChannelType, DbName),
        Q = channel_queue(ChannelType),
        ok = application:stop(smoosh),
        ok = application:start(smoosh),
        ?assertEqual(Q, channel_queue(ChannelType)),
        ok
    end).

grow_db_file(DbName, SizeInKb) ->
    {ok, #db{filepath = FilePath} = Db} = couch_db:open_int(DbName, [?ADMIN_CTX]),
    {ok, Fd} = file:open(FilePath, [append]),
    Bytes = binary:copy(?KILOBYTE, SizeInKb),
    file:write(Fd, Bytes),
    ok = file:close(Fd),
    Doc = couch_doc:from_json_obj(
        {[
            {<<"_id">>, ?l2b(?docid())},
            {<<"value">>, ?l2b(?docid())}
        ]}
    ),
    {ok, _} = couch_db:update_docs(Db, [Doc], []),
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

channel_queue(ChannelType) ->
    Q0 = smoosh_priority_queue:new(ChannelType),
    smoosh_priority_queue:open(Q0).
