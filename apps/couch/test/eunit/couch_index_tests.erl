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

-module(couch_index_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-define(TIMEOUT, 1000).

setup() ->
    DbName = ?tempdb(),
    {ok, Db} = couch_db:create(DbName, [?ADMIN_CTX]),
    ok = couch_db:close(Db),
    create_design_doc(DbName, <<"_design/foo">>, <<"bar">>),
    tracer_new(),
    DbName.

teardown(DbName) ->
    tracer_delete(),
    couch_server:delete(DbName, [?ADMIN_CTX]).

couch_index_ioq_priority_test_() ->
    {
        "Test ioq_priority for views",
        {
            setup,
            fun test_util:start_couch/0,
            fun test_util:stop_couch/1,
            {
                foreach,
                fun setup/0,
                fun teardown/1,
                [
                    fun check_io_priority_for_updater/1,
                    fun check_io_priority_for_compactor/1
                ]
            }
        }
    }.

check_io_priority_for_updater(DbName) ->
    ?_test(begin
        {ok, IndexerPid} = couch_index_server:get_index(
            couch_mrview_index, DbName, <<"_design/foo">>
        ),
        CouchIndexUpdaterPid = updater_pid(IndexerPid),
        tracer_record(CouchIndexUpdaterPid),

        create_docs(DbName),

        CommittedSeq = couch_util:with_db(DbName, fun(Db) -> couch_db:get_update_seq(Db) end),
        couch_index:get_state(IndexerPid, CommittedSeq),
        [UpdaterPid] = wait_spawn_event_for_pid(CouchIndexUpdaterPid),

        [UpdaterMapProcess] = wait_spawn_by_anonymous_fun(
            UpdaterPid, '-start_update/4-fun-0-'
        ),

        ?assert(
            wait_set_io_priority(
                UpdaterMapProcess, {view_update, DbName, <<"_design/foo">>}
            )
        ),

        [UpdaterWriterProcess] = wait_spawn_by_anonymous_fun(
            UpdaterPid, '-start_update/4-fun-1-'
        ),
        ?assert(
            wait_set_io_priority(
                UpdaterWriterProcess, {view_update, DbName, <<"_design/foo">>}
            )
        ),

        ok
    end).

check_io_priority_for_compactor(DbName) ->
    ?_test(begin
        {ok, IndexerPid} = couch_index_server:get_index(
            couch_mrview_index, DbName, <<"_design/foo">>
        ),
        {ok, CompactorPid} = couch_index:get_compactor_pid(IndexerPid),
        tracer_record(CompactorPid),

        create_docs(DbName),

        couch_index:compact(IndexerPid),
        wait_spawn_event_for_pid(CompactorPid),

        [CompactorProcess] = wait_spawn_by_anonymous_fun(
            CompactorPid, '-handle_call/3-fun-0-'
        ),
        ?assert(
            wait_set_io_priority(
                CompactorProcess, {view_compact, DbName, <<"_design/foo">>}
            )
        ),
        ok
    end).

create_docs(DbName) ->
    {ok, Db} = couch_db:open(DbName, [?ADMIN_CTX]),
    Doc1 = couch_doc:from_json_obj(
        {[
            {<<"_id">>, <<"doc1">>},
            {<<"value">>, 1}
        ]}
    ),
    Doc2 = couch_doc:from_json_obj(
        {[
            {<<"_id">>, <<"doc2">>},
            {<<"value">>, 2}
        ]}
    ),
    Doc3 = couch_doc:from_json_obj(
        {[
            {<<"_id">>, <<"doc3">>},
            {<<"value">>, 3}
        ]}
    ),
    {ok, _} = couch_db:update_docs(Db, [Doc1, Doc2, Doc3]),
    couch_db:close(Db).

create_design_doc(DbName, DDName, ViewName) ->
    {ok, Db} = couch_db:open(DbName, [?ADMIN_CTX]),
    DDoc = couch_doc:from_json_obj(
        {[
            {<<"_id">>, DDName},
            {<<"language">>, <<"javascript">>},
            {<<"views">>,
                {[
                    {ViewName,
                        {[
                            {<<"map">>, <<"function(doc) { emit(doc.value, null); }">>}
                        ]}}
                ]}}
        ]}
    ),
    {ok, Rev} = couch_db:update_doc(Db, DDoc, []),
    couch_db:close(Db),
    Rev.

wait_set_io_priority(Pid, IOPriority) ->
    test_util:wait_value(
        fun() ->
            does_process_set_io_priority(Pid, IOPriority)
        end,
        true
    ).

does_process_set_io_priority(Pid, IOPriority) ->
    PutCallsArgs = find_calls_to_fun(Pid, {erlang, put, 2}),
    lists:any(fun([_, Priority]) -> Priority =:= IOPriority end, PutCallsArgs).

wait_events(MatchSpec) ->
    test_util:wait_other_value(fun() -> select(MatchSpec) end, []).

find_spawned_by_anonymous_fun(ParentPid, Name) ->
    AnonymousFuns = select(
        ets:fun2ms(fun({spawned, Pid, _TS, _Name, _Dict, [PPid, {erlang, apply, [Fun, _]}]}) when
            is_function(Fun) andalso PPid =:= ParentPid
        ->
            {Pid, Fun}
        end)
    ),
    lists:filtermap(
        fun({Pid, Fun}) ->
            case erlang:fun_info(Fun, name) of
                {name, Name} -> {true, Pid};
                _ -> false
            end
        end,
        AnonymousFuns
    ).

find_calls_to_fun(Pid, {Module, Function, Arity}) ->
    select(
        ets:fun2ms(fun({call, P, _TS, _Name, _Dict, [{M, F, Args}]}) when
            length(Args) =:= Arity andalso
                M =:= Module andalso
                F =:= Function andalso
                P =:= Pid
        ->
            Args
        end)
    ).

wait_spawn_event_for_pid(ParentPid) ->
    wait_events(
        ets:fun2ms(fun({spawned, Pid, _TS, _Name, _Dict, [P, _]}) when P =:= ParentPid -> Pid end)
    ).

wait_spawn_by_anonymous_fun(ParentPid, Name) ->
    test_util:wait_other_value(
        fun() ->
            find_spawned_by_anonymous_fun(ParentPid, Name)
        end,
        []
    ).

updater_pid(IndexerPid) ->
    {links, Links} = process_info(IndexerPid, links),
    [Pid] = select_process_by_name_prefix(Links, "couch_index_updater:init/1"),
    Pid.

select_process_by_name_prefix(Pids, Name) ->
    lists:filter(
        fun(Pid) ->
            Key = couch_debug:process_name(Pid),
            string:str(Key, Name) =:= 1
        end,
        Pids
    ).

select(MatchSpec) ->
    lists:filtermap(
        fun(Event) ->
            case ets:test_ms(Event, MatchSpec) of
                {ok, false} -> false;
                {ok, Result} -> {true, Result};
                _ -> false
            end
        end,
        tracer_events()
    ).

%% ========================
%% Tracer related functions
%% ------------------------
tracer_new() ->
    ets:new(?MODULE, [public, named_table]),
    {ok, _Tracer} = dbg:tracer(process, {fun tracer_collector/2, 0}),
    ok.

tracer_delete() ->
    dbg:stop_clear(),
    (catch ets:delete(?MODULE)),
    ok.

tracer_record(Pid) ->
    {ok, _} = dbg:tp(erlang, put, x),
    {ok, _} = dbg:p(Pid, [c, p, sos]),
    ok.

tracer_events() ->
    Events = [{Idx, E} || [Idx, E] <- ets:match(?MODULE, {{trace, '$1'}, '$2'})],
    {_, Sorted} = lists:unzip(lists:keysort(1, Events)),
    Sorted.

tracer_collector(Msg, Seq) ->
    ets:insert(?MODULE, {{trace, Seq}, normalize_trace_msg(Msg)}),
    Seq + 1.

normalize_trace_msg(TraceMsg) ->
    case tuple_to_list(TraceMsg) of
        [trace_ts, Pid, Type | Info] ->
            {TraceInfo, [Timestamp]} = lists:split(length(Info) - 1, Info),
            {Type, Pid, Timestamp, couch_debug:process_name(Pid), process_info(Pid), TraceInfo};
        [trace, Pid, Type | TraceInfo] ->
            {Type, Pid, os:timestamp(), couch_debug:process_name(Pid), process_info(Pid), TraceInfo}
    end.
