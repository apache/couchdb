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

-module(couchdb_compaction_daemon_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(TIMEOUT, 120000).
-define(TIMEOUT_S, ?TIMEOUT div 1000).
-define(MODS_TO_MOCK,
        [couch_db_updater, couch_mrview_compactor, couch_compaction_daemon]).


start() ->
    Ctx = test_util:start_couch(),
    ok = config:set("compaction_daemon", "check_interval", "3", false),
    ok = config:set("compaction_daemon", "min_file_size", "100000", false),
    ok = config:delete("compactions", "_default", false),
    ok = meck:new(?MODS_TO_MOCK, [passthrough]),
    Ctx.

stop(Ctx) ->
    test_util:stop_couch(Ctx),
    meck:unload(?MODS_TO_MOCK).

setup() ->
    DbName = ?tempdb(),
    {ok, Db} = couch_db:create(DbName, [?ADMIN_CTX]),
    create_design_doc(Db),
    populate(DbName, 70, 70, 200 * 1024),
    ok = couch_db:close(Db),
    meck:reset(?MODS_TO_MOCK),
    DbName.

teardown(DbName) ->
    Configs = config:get("compactions"),
    lists:foreach(
        fun({Key, _}) ->
            ok = config:delete("compactions", Key, false)
        end,
        Configs),
    couch_server:delete(DbName, [?ADMIN_CTX]),
    exit(whereis(couch_index_server), shutdown).


compaction_daemon_test_() ->
    {
        "Compaction daemon tests",
        {
            setup,
            fun start/0, fun stop/1,
            {
                foreach,
                fun setup/0, fun teardown/1,
                [
                    fun should_compact_by_default_rule/1,
                    fun should_compact_by_dbname_rule/1
                ]
            }
        }
    }.


should_compact_by_default_rule(DbName) ->
    {timeout, ?TIMEOUT_S, ?_test(begin
        CompactionMonitor = spawn_compaction_monitor(DbName),

        {_, DbFileSize} = get_db_frag(DbName),
        {_, ViewFileSize} = get_view_frag(DbName),

        with_config_change(DbName, fun() ->
            ok = config:set("compactions", "_default",
                "[{db_fragmentation, \"70%\"}, {view_fragmentation, \"70%\"}]",
                false)
        end),

        wait_for_compaction(CompactionMonitor),

        with_config_change(DbName, fun() ->
            ok = config:delete("compactions", "_default", false)
        end),

        {DbFrag2, DbFileSize2} = get_db_frag(DbName),
        {ViewFrag2, ViewFileSize2} = get_view_frag(DbName),

        ?assert(DbFrag2 < 70),
        ?assert(ViewFrag2 < 70),

        ?assert(DbFileSize > DbFileSize2),
        ?assert(ViewFileSize > ViewFileSize2),

        ?assert(is_idle(DbName))
    end)}.

should_compact_by_dbname_rule(DbName) ->
    {timeout, ?TIMEOUT_S, ?_test(begin
        CompactionMonitor = spawn_compaction_monitor(DbName),

        {_, DbFileSize} = get_db_frag(DbName),
        {_, ViewFileSize} = get_view_frag(DbName),

        with_config_change(DbName, fun() ->
            ok = config:set("compactions", ?b2l(DbName),
                "[{db_fragmentation, \"70%\"}, {view_fragmentation, \"70%\"}]",
                false)
        end),

        wait_for_compaction(CompactionMonitor),

        with_config_change(DbName, fun() ->
            ok = config:delete("compactions", ?b2l(DbName), false)
        end),

        {DbFrag2, DbFileSize2} = get_db_frag(DbName),
        {ViewFrag2, ViewFileSize2} = get_view_frag(DbName),

        ?assert(DbFrag2 < 70),
        ?assert(ViewFrag2 < 70),

        ?assert(DbFileSize > DbFileSize2),
        ?assert(ViewFileSize > ViewFileSize2),

        ?assert(is_idle(DbName))
    end)}.


create_design_doc(Db) ->
    DDoc = couch_doc:from_json_obj({[
        {<<"_id">>, <<"_design/foo">>},
        {<<"language">>, <<"javascript">>},
        {<<"views">>, {[
            {<<"foo">>, {[
                {<<"map">>, <<"function(doc) { emit(doc._id, doc); }">>}
            ]}},
            {<<"foo2">>, {[
                {<<"map">>, <<"function(doc) { emit(doc._id, doc); }">>}
            ]}},
            {<<"foo3">>, {[
                {<<"map">>, <<"function(doc) { emit(doc._id, doc); }">>}
            ]}}
        ]}}
    ]}),
    {ok, _} = couch_db:update_docs(Db, [DDoc]),
    {ok, _} = couch_db:ensure_full_commit(Db),
    ok.

populate(DbName, DbFrag, ViewFrag, MinFileSize) ->
    {CurDbFrag, DbFileSize} = get_db_frag(DbName),
    {CurViewFrag, ViewFileSize} = get_view_frag(DbName),
    populate(DbName, DbFrag, ViewFrag, MinFileSize, CurDbFrag, CurViewFrag,
             lists:min([DbFileSize, ViewFileSize])).

populate(_Db, DbFrag, ViewFrag, MinFileSize, CurDbFrag, CurViewFrag, FileSize)
    when CurDbFrag >= DbFrag, CurViewFrag >= ViewFrag, FileSize >= MinFileSize ->
    ok;
populate(DbName, DbFrag, ViewFrag, MinFileSize, _, _, _) ->
    update(DbName),
    {CurDbFrag, DbFileSize} = get_db_frag(DbName),
    {CurViewFrag, ViewFileSize} = get_view_frag(DbName),
    populate(DbName, DbFrag, ViewFrag, MinFileSize, CurDbFrag, CurViewFrag,
             lists:min([DbFileSize, ViewFileSize])).

update(DbName) ->
    {ok, Db} = couch_db:open_int(DbName, []),
    lists:foreach(fun(_) ->
        Doc = couch_doc:from_json_obj({[{<<"_id">>, couch_uuids:new()}]}),
        {ok, _} = couch_db:update_docs(Db, [Doc]),
        query_view(couch_db:name(Db))
    end, lists:seq(1, 200)),
    couch_db:close(Db).

db_url(DbName) ->
    Addr = config:get("httpd", "bind_address", "127.0.0.1"),
    Port = integer_to_list(mochiweb_socket_server:get(couch_httpd, port)),
    "http://" ++ Addr ++ ":" ++ Port ++ "/" ++ ?b2l(DbName).

query_view(DbName) ->
    {ok, Code, _Headers, _Body} = test_request:get(
        db_url(DbName) ++ "/_design/foo/_view/foo"),
    ?assertEqual(200, Code).

get_db_frag(DbName) ->
    {ok, Db} = couch_db:open_int(DbName, []),
    {ok, Info} = couch_db:get_db_info(Db),
    couch_db:close(Db),
    FileSize = get_size(file, Info),
    DataSize = get_size(active, Info),
    {round((FileSize - DataSize) / FileSize * 100), FileSize}.

get_view_frag(DbName) ->
    {ok, Db} = couch_db:open_int(DbName, []),
    {ok, Info} = couch_mrview:get_info(Db, <<"_design/foo">>),
    couch_db:close(Db),
    FileSize = get_size(file, Info),
    DataSize = get_size(active, Info),
    {round((FileSize - DataSize) / FileSize * 100), FileSize}.

get_size(Kind, Info) ->
    couch_util:get_nested_json_value({Info}, [sizes, Kind]).

spawn_compaction_monitor(DbName) ->
    TestPid = self(),
    {Pid, Ref} = spawn_monitor(fun() ->
        DaemonPid = whereis(couch_compaction_daemon),
        DbPid = couch_util:with_db(DbName, fun(Db) ->
            couch_db:get_pid(Db)
        end),
        {ok, ViewPid} = couch_index_server:get_index(couch_mrview_index,
                DbName, <<"_design/foo">>),
        {ok, CompactorPid} = couch_index:get_compactor_pid(ViewPid),
        TestPid ! {self(), started},
        receive
            {TestPid, go} -> ok
        after ?TIMEOUT ->
            erlang:error(timeout)
        end,
        meck:wait(
                1,
                couch_compaction_daemon,
                handle_cast,
                [{config_update, '_', '_'}, '_'],
                DaemonPid,
                ?TIMEOUT
            ),
        meck:wait(
                1,
                couch_db_updater,
                handle_cast,
                [{compact_done, '_', '_'}, '_'],
                DbPid,
                ?TIMEOUT
            ),
        meck:reset(couch_mrview_compactor),
        meck:wait(
                1,
                couch_mrview_compactor,
                compact,
                ['_', '_', '_'],
                ?TIMEOUT
            ),
        {ok, CPid} = couch_index_compactor:get_compacting_pid(CompactorPid),
        CRef = erlang:monitor(process, CPid),
        meck:wait(
                1,
                couch_mrview_compactor,
                swap_compacted,
                ['_', '_'],
                ViewPid,
                ?TIMEOUT
            ),
        receive
            {'DOWN', CRef, process, _, _} -> ok
        after ?TIMEOUT ->
            erlang:error(timeout)
        end
    end),
    receive
        {Pid, started} -> ok;
        {'DOWN', Ref, _, _, Reason} -> erlang:error({monitor_failure, Reason})
    after ?TIMEOUT ->
        erlang:error({assertion_failed, [
                {module, ?MODULE},
                {line, ?LINE},
                {reason, "Compaction starting timeout"}
            ]})
    end,
    {Pid, Ref}.

wait_for_compaction({Pid, Ref}) ->
    Pid ! {self(), go},
    receive
        {'DOWN', Ref, _, _, normal} -> ok;
        {'DOWN', Ref, _, _, Other} -> erlang:error(Other)
    after ?TIMEOUT ->
        erlang:error({assertion_failed, [
                {module, ?MODULE},
                {line, ?LINE},
                {reason, "Compaction finishing timeout"}
            ]})
    end.

is_idle(DbName) ->
    {ok, Db} = couch_db:open_int(DbName, [?ADMIN_CTX]),
    Monitors = couch_db:monitored_by(Db),
    ok = couch_db:close(Db),
    Others = [M || M <- Monitors, M /= self()],
    if Others == [] -> ok; true ->
        lists:foreach(fun(Other) ->
            Args = [Other, process_info(Other)],
            couch_log:error("XKCD: MONITORED BY ~p :: ~p", Args)
        end, Others)
    end,
    not lists:any(fun(M) -> M /= self() end, Monitors).

with_config_change(_DbName, Fun) ->
    Current = ets:info(couch_compaction_daemon_config, size),
    Fun(),
    test_util:wait(fun() ->
        case ets:info(couch_compaction_daemon_config, size) == Current of
            false -> ok;
            true -> wait
        end
    end).
