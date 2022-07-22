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

-module(couch_bt_engine_compactor_ev_tests).

-include_lib("couch/include/couch_db.hrl").
-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/src/couch_server_int.hrl").

-define(TIMEOUT_EUNIT, 60).
-define(EV_MOD, couch_bt_engine_compactor_ev).
-define(INIT_DOCS, 2500).
-define(WRITE_DOCS, 20).

% The idea behind the tests in this module are to attempt to
% cover the number of restart/recopy events during compaction
% so that we can be as sure as possible that the compactor
% is resilient to errors in the face of external conditions
% (i.e., the VM rebooted). The single linear pass is easy enough
% to prove, however restarting is important enough that we don't
% want to waste work if a VM happens to bounce a lot.
%
% To try and cover as many restart situations we have created a
% number of events in the compactor code that are present during
% a test compiled version of the module. These events can then
% be used (via meck) to introduce errors and coordinate writes
% to the database while compaction is in progress.

% This list of events is where we'll insert our errors.

events() ->
    [
        % The compactor process is spawned
        init,
        % After compaction files have opened
        files_opened,

        % Just before apply purge changes
        purge_init,
        % Just after finish purge updates
        purge_done,

        % The firs phase is when we copy all document body and attachment
        % data to the new database file in order of update sequence so
        % that we can resume on crash.

        % Before the first change is copied
        seq_init,
        % After change N is copied
        {seq_copy, 0},
        {seq_copy, ?INIT_DOCS div 2},
        {seq_copy, ?INIT_DOCS - 2},
        % After last change is copied
        seq_done,

        % The id copy phases come in two flavors. Before a compaction
        % swap is attempted they're copied from the id_tree in the
        % database being compacted. After a swap attempt they are
        % stored in an emsort file on disk. Thus the two sets of
        % related events here.

        % Just before metadata sort starts
        md_sort_init,
        % Justa after metadata sort finished
        md_sort_done,
        % Just before metadata copy starts
        md_copy_init,
        % After docid N is copied
        {md_copy_row, 0},
        {md_copy_row, ?INIT_DOCS div 2},
        {md_copy_row, ?INIT_DOCS - 2},
        % Just after the last docid is copied
        md_copy_done,

        % And then the final steps before we finish

        % Just before final sync
        before_final_sync,
        % Just after the final sync
        after_final_sync,
        % Just before the final notification
        before_notify
    ].

% Mark which evens only happen when documents are present

requires_docs({seq_copy, _}) -> true;
requires_docs(md_sort_init) -> true;
requires_docs(md_sort_done) -> true;
requires_docs(md_copy_init) -> true;
requires_docs({md_copy_row, _}) -> true;
requires_docs(md_copy_done) -> true;
requires_docs(_) -> false.

% Mark which events only happen when there's write activity during
% a compaction.

requires_write(md_sort_init) -> true;
requires_write(md_sort_done) -> true;
requires_write(md_copy_init) -> true;
requires_write({md_copy_row, _}) -> true;
requires_write(md_copy_done) -> true;
requires_write(_) -> false.

setup() ->
    purge_module(),
    ?EV_MOD:init(),
    test_util:start_couch().

teardown(Ctx) ->
    test_util:stop_couch(Ctx),
    ?EV_MOD:terminate().

start_empty_db_test(_Event) ->
    ?EV_MOD:clear(),
    DbName = ?tempdb(),
    {ok, _} = couch_db:create(DbName, [?ADMIN_CTX]),
    DbName.

start_populated_db_test(Event) ->
    DbName = start_empty_db_test(Event),
    {ok, Db} = couch_db:open_int(DbName, []),
    try
        populate_db(Db, ?INIT_DOCS)
    after
        couch_db:close(Db)
    end,
    DbName.

stop_test(_Event, DbName) ->
    couch_server:delete(DbName, [?ADMIN_CTX]).

static_empty_db_test_() ->
    FiltFun = fun(E) ->
        not (requires_docs(E) or requires_write(E))
    end,
    Events = lists:filter(FiltFun, events()) -- [init],
    {
        "Idle empty database",
        {
            setup,
            fun setup/0,
            fun teardown/1,
            [
                {
                    foreachx,
                    fun start_empty_db_test/1,
                    fun stop_test/2,
                    [{Event, fun run_static_init/2} || Event <- Events]
                }
            ]
        }
    }.

static_populated_db_test_() ->
    FiltFun = fun(E) -> not requires_write(E) end,
    Events = lists:filter(FiltFun, events()) -- [init],
    {
        "Idle populated database",
        {
            setup,
            fun setup/0,
            fun teardown/1,
            [
                {
                    foreachx,
                    fun start_populated_db_test/1,
                    fun stop_test/2,
                    [{Event, fun run_static_init/2} || Event <- Events]
                }
            ]
        }
    }.

dynamic_empty_db_test_() ->
    FiltFun = fun(E) -> not requires_docs(E) end,
    Events = lists:filter(FiltFun, events()) -- [init],
    {
        "Writes to empty database",
        {
            setup,
            fun setup/0,
            fun teardown/1,
            [
                {
                    foreachx,
                    fun start_empty_db_test/1,
                    fun stop_test/2,
                    [{Event, fun run_dynamic_init/2} || Event <- Events]
                }
            ]
        }
    }.

dynamic_populated_db_test_() ->
    Events = events() -- [init],
    {
        "Writes to populated database",
        {
            setup,
            fun setup/0,
            fun teardown/1,
            [
                {
                    foreachx,
                    fun start_populated_db_test/1,
                    fun stop_test/2,
                    [{Event, fun run_dynamic_init/2} || Event <- Events]
                }
            ]
        }
    }.

run_static_init(Event, DbName) ->
    Name = lists:flatten(io_lib:format("~p", [Event])),
    Test = {timeout, ?TIMEOUT_EUNIT, ?_test(run_static(Event, DbName))},
    {Name, Test}.

run_static(Event, DbName) ->
    {ok, ContinueFun} = ?EV_MOD:set_wait(init),
    {ok, Reason} = ?EV_MOD:set_crash(Event),
    {ok, Db} = couch_db:open_int(DbName, []),
    Ref = couch_db:monitor(Db),
    {ok, CPid} = couch_db:start_compact(Db),
    ContinueFun(CPid),
    receive
        {'DOWN', Ref, _, _, Reason} ->
            wait_db_cleared(Db)
    end,
    run_successful_compaction(DbName),
    couch_db:close(Db).

run_dynamic_init(Event, DbName) ->
    Name = lists:flatten(io_lib:format("~p", [Event])),
    Test = {timeout, ?TIMEOUT_EUNIT, ?_test(run_dynamic(Event, DbName))},
    {Name, Test}.

run_dynamic(Event, DbName) ->
    {ok, ContinueFun} = ?EV_MOD:set_wait(init),
    {ok, Reason} = ?EV_MOD:set_crash(Event),
    {ok, Db} = couch_db:open_int(DbName, []),
    Ref = couch_db:monitor(Db),
    {ok, CPid} = couch_db:start_compact(Db),
    ok = populate_db(Db, 10),
    ContinueFun(CPid),
    receive
        {'DOWN', Ref, _, _, Reason} ->
            wait_db_cleared(Db)
    end,
    run_successful_compaction(DbName),
    couch_db:close(Db).

run_successful_compaction(DbName) ->
    ?EV_MOD:clear(),
    {ok, ContinueFun} = ?EV_MOD:set_wait(init),
    {ok, Db} = couch_db:open_int(DbName, []),
    {ok, CPid} = couch_db:start_compact(Db),
    Ref = erlang:monitor(process, CPid),
    ContinueFun(CPid),
    receive
        {'DOWN', Ref, _, _, normal} -> ok
    end,
    Pid = couch_db:get_pid(Db),
    {ok, NewDb} = gen_server:call(Pid, get_db),
    validate_compaction(NewDb),
    couch_db:close(Db).

wait_db_cleared(Db) ->
    wait_db_cleared(Db, 5).

wait_db_cleared(Db, N) when N < 0 ->
    erlang:error({db_clear_timeout, couch_db:name(Db)});
wait_db_cleared(Db, N) ->
    Tab = couch_server:couch_dbs(couch_db:name(Db)),
    case ets:lookup(Tab, couch_db:name(Db)) of
        [] ->
            ok;
        [#entry{db = NewDb}] ->
            OldPid = couch_db:get_pid(Db),
            NewPid = couch_db:get_pid(NewDb),
            if
                NewPid /= OldPid ->
                    ok;
                true ->
                    timer:sleep(100),
                    wait_db_cleared(Db, N - 1)
            end
    end.

populate_db(_Db, NumDocs) when NumDocs =< 0 ->
    ok;
populate_db(Db, NumDocs) ->
    String = [$a || _ <- lists:seq(1, erlang:min(NumDocs, 500))],
    Docs = lists:map(
        fun(_) ->
            couch_doc:from_json_obj(
                {[
                    {<<"_id">>, couch_uuids:random()},
                    {<<"string">>, list_to_binary(String)}
                ]}
            )
        end,
        lists:seq(1, 500)
    ),
    {ok, _} = couch_db:update_docs(Db, Docs, []),
    populate_db(Db, NumDocs - 500).

validate_compaction(Db) ->
    {ok, DocCount} = couch_db:get_doc_count(Db),
    {ok, DelDocCount} = couch_db:get_del_doc_count(Db),
    NumChanges = couch_db:count_changes_since(Db, 0),
    FoldFun = fun(FDI, {PrevId, CountAcc}) ->
        ?assert(FDI#full_doc_info.id > PrevId),
        {ok, {FDI#full_doc_info.id, CountAcc + 1}}
    end,
    {ok, {_, LastCount}} = couch_db:fold_docs(Db, FoldFun, {<<>>, 0}),
    ?assertEqual(DocCount + DelDocCount, LastCount),
    ?assertEqual(NumChanges, LastCount).

purge_module() ->
    case code:which(couch_db_updater) of
        cover_compiled ->
            ok;
        _ ->
            code:delete(couch_db_updater),
            code:purge(couch_db_updater)
    end.
