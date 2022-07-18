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

-module(couch_server_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-include("../src/couch_db_int.hrl").
-include("../src/couch_server_int.hrl").

start() ->
    Ctx = test_util:start_couch(),
    config:set("log", "include_sasl", "false", false),
    Ctx.

setup() ->
    DbName = ?tempdb(),
    {ok, Db} = couch_db:create(DbName, []),
    Db.

setup(rename) ->
    config:set("couchdb", "enable_database_recovery", "true", false),
    setup();
setup(_) ->
    setup().

teardown(Db) ->
    FilePath = couch_db:get_filepath(Db),
    (catch couch_db:close(Db)),
    (catch file:delete(FilePath)).

teardown(rename, Db) ->
    config:set("couchdb", "enable_database_recovery", "false", false),
    teardown(Db);
teardown(_, Db) ->
    teardown(Db).

delete_db_test_() ->
    {
        "Test for proper deletion of db file",
        {
            setup,
            fun start/0,
            fun test_util:stop/1,
            [
                make_test_case(rename, [fun should_rename_on_delete/2]),
                make_test_case(delete, [fun should_delete/2])
            ]
        }
    }.

make_test_case(Mod, Funs) ->
    {
        lists:flatten(io_lib:format("~s", [Mod])),
        {foreachx, fun setup/1, fun teardown/2, [{Mod, Fun} || Fun <- Funs]}
    }.

should_rename_on_delete(_, Db) ->
    DbName = couch_db:name(Db),
    Origin = couch_db:get_filepath(Db),
    ?_test(begin
        ?assert(filelib:is_regular(Origin)),
        ?assertMatch(ok, couch_server:delete(DbName, [])),
        ?assertNot(filelib:is_regular(Origin)),
        DeletedFiles = deleted_files(Origin),
        ?assertMatch([_], DeletedFiles),
        [Renamed] = DeletedFiles,
        ?assertEqual(
            filename:extension(Origin), filename:extension(Renamed)
        ),
        ?assert(filelib:is_regular(Renamed))
    end).

should_delete(_, Db) ->
    DbName = couch_db:name(Db),
    Origin = couch_db:get_filepath(Db),
    ?_test(begin
        ?assert(filelib:is_regular(Origin)),
        ?assertMatch(ok, couch_server:delete(DbName, [])),
        ?assertNot(filelib:is_regular(Origin)),
        ?assertMatch([], deleted_files(Origin))
    end).

deleted_files(ViewFile) ->
    filelib:wildcard(filename:rootname(ViewFile) ++ "*.deleted.*").

bad_engine_option_test_() ->
    {
        setup,
        fun start/0,
        fun test_util:stop/1,
        [
            fun t_bad_engine_option/0
        ]
    }.

t_bad_engine_option() ->
    Resp = couch_server:create(?tempdb(), [{engine, <<"cowabunga!">>}]),
    ?assertEqual(Resp, {error, {invalid_engine_extension, <<"cowabunga!">>}}).

get_engine_path_test_() ->
    {
        setup,
        fun start/0,
        fun test_util:stop/1,
        {
            foreach,
            fun setup/0,
            fun teardown/1,
            [
                fun should_return_engine_path/1,
                fun should_return_invalid_engine_error/1
            ]
        }
    }.

should_return_engine_path(Db) ->
    DbName = couch_db:name(Db),
    Engine = couch_db_engine:get_engine(Db),
    Resp = couch_server:get_engine_path(DbName, Engine),
    FilePath = couch_db:get_filepath(Db),
    ?_assertMatch({ok, FilePath}, Resp).

should_return_invalid_engine_error(Db) ->
    DbName = couch_db:name(Db),
    Engine = fake_engine,
    Resp = couch_server:get_engine_path(DbName, Engine),
    ?_assertMatch({error, {invalid_engine, Engine}}, Resp).

interleaved_requests_test_() ->
    {
        setup,
        fun start_interleaved/0,
        fun stop_interleaved/1,
        fun make_interleaved_requests/1
    }.

start_interleaved() ->
    TestDbName = ?tempdb(),
    meck:new(couch_db, [passthrough]),
    meck:expect(couch_db, start_link, fun(Engine, DbName, Filename, Options) ->
        case DbName of
            TestDbName ->
                receive
                    go -> ok
                end,
                Res = meck:passthrough([Engine, DbName, Filename, Options]),
                % We're unlinking and sending a delayed
                % EXIT signal so that we can mimic a specific
                % message order in couch_server. On a test machine
                % this is a big race condition which affects the
                % ability to induce the bug.
                case Res of
                    {ok, Db} ->
                        DbPid = couch_db:get_pid(Db),
                        unlink(DbPid),
                        Msg = {'EXIT', DbPid, killed},
                        erlang:send_after(2000, whereis(couch_server:couch_server(DbName)), Msg);
                    _ ->
                        ok
                end,
                Res;
            _ ->
                meck:passthrough([Engine, DbName, Filename, Options])
        end
    end),
    {test_util:start_couch(), TestDbName}.

stop_interleaved({Ctx, TestDbName}) ->
    couch_server:delete(TestDbName, [?ADMIN_CTX]),
    meck:unload(),
    test_util:stop_couch(Ctx).

make_interleaved_requests({_, TestDbName}) ->
    [
        fun() -> t_interleaved_create_delete_open(TestDbName) end
    ].

t_interleaved_create_delete_open(DbName) ->
    {CrtRef, OpenRef} = {make_ref(), make_ref()},
    CrtMsg = {'$gen_call', {self(), CrtRef}, {create, DbName, [?ADMIN_CTX]}},
    FakePid = spawn(fun() -> ok end),
    OpenResult = {open_result, DbName, {ok, #db{main_pid = FakePid}}},
    OpenResultMsg = {'$gen_call', {self(), OpenRef}, OpenResult},

    % Get the current couch_server pid so we're sure
    % to not end up messaging two different pids
    CouchServer = whereis(couch_server:couch_server(DbName)),

    % Start our first instance that will succeed in
    % an invalid state. Notice that the opener pid
    % spawned by couch_server:open_async/5 will halt
    % in our meck expect function waiting for a message.
    %
    % We're using raw message passing here so that we don't
    % have to coordinate multiple processes for this test.
    CouchServer ! CrtMsg,
    {ok, Opener} = get_opener_pid(DbName),

    % We have to suspend couch_server so that we can enqueue
    % our next requests and let the opener finish processing.
    erlang:suspend_process(CouchServer),

    % We queue a confused open_result message in front of
    % the correct response from the opener.
    CouchServer ! OpenResultMsg,

    % Release the opener pid so it can continue
    Opener ! go,

    % Wait for the '$gen_call' message from OpenerPid to arrive
    % in couch_server's mailbox
    ok = wait_for_open_async_result(CouchServer, Opener),

    % Now monitor and resume the couch_server and assert that
    % couch_server does not crash while processing OpenResultMsg
    CSRef = erlang:monitor(process, CouchServer),
    erlang:resume_process(CouchServer),
    check_monitor_not_triggered(CSRef),

    % Our open_result message was processed and ignored
    ?assertEqual({OpenRef, ok}, get_next_message()),

    % Our create request was processed normally after we
    % ignored the spurious open_result
    ?assertMatch({CrtRef, {ok, _}}, get_next_message()),

    % And finally assert that couch_server is still
    % alive.
    ?assert(is_process_alive(CouchServer)),
    check_monitor_not_triggered(CSRef).

get_opener_pid(DbName) ->
    WaitFun = fun() ->
        case ets:lookup(couch_server:couch_dbs(DbName), DbName) of
            [#entry{pid = Pid}] ->
                {ok, Pid};
            [] ->
                wait
        end
    end,
    test_util:wait(WaitFun).

wait_for_open_async_result(CouchServer, Opener) ->
    WaitFun = fun() ->
        {_, Messages} = erlang:process_info(CouchServer, messages),
        Found = lists:foldl(
            fun(Msg, Acc) ->
                case Msg of
                    {'$gen_call', {Opener, _}, {open_result, _, {ok, _}}} ->
                        true;
                    _ ->
                        Acc
                end
            end,
            false,
            Messages
        ),
        if
            Found -> ok;
            true -> wait
        end
    end,
    test_util:wait(WaitFun).

check_monitor_not_triggered(Ref) ->
    receive
        {'DOWN', Ref, _, _, Reason0} ->
            erlang:error({monitor_triggered, Reason0})
    after 100 ->
        ok
    end.

get_next_message() ->
    receive
        Msg ->
            Msg
    after 5000 ->
        erlang:error(timeout)
    end.
