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

-module(couch_mrview_compact_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(TIMEOUT, 1000).


setup() ->
    {ok, Db} = couch_mrview_test_util:init_db(?tempdb(), map, 1000),
    ok = meck:new(couch_mrview_compactor, [passthrough]),
    Db.

teardown(Db) ->
    meck:unload(),
    couch_db:close(Db),
    couch_server:delete(couch_db:name(Db), [?ADMIN_CTX]),
    ok.


compaction_test_() ->
    {
        "Compaction tests",
        {
            setup,
            fun test_util:start_couch/0, fun test_util:stop_couch/1,
            {
                foreach,
                fun setup/0, fun teardown/1,
                [
                    fun should_swap/1,
                    fun should_remove/1
                ]
            }
        }
    }.


should_swap(Db) ->
    ?_test(begin
        couch_mrview:query_view(Db, <<"_design/bar">>, <<"baz">>),
        {ok, QPid} = start_query(Db),
        {ok, MonRef} = couch_mrview:compact(Db, <<"_design/bar">>, [monitor]),
        receive
            {'DOWN', MonRef, process, _, _} -> ok
        after ?TIMEOUT ->
            erlang:error(
                {assertion_failed,
                 [{module, ?MODULE}, {line, ?LINE},
                  {reason, "compaction failed"}]})
        end,
        QPid ! {self(), continue},
        receive
            {QPid, Count} ->
                ?assertEqual(1000, Count)
        after ?TIMEOUT ->
            erlang:error(
                {assertion_failed,
                 [{module, ?MODULE}, {line, ?LINE},
                  {reason, "query failed"}]})
        end
    end).


should_remove(Db) ->
    ?_test(begin
        DDoc = <<"_design/bar">>,
        {ok, _Results} = couch_mrview:query_view(Db, DDoc, <<"baz">>),
        {ok, IndexPid} = couch_index_server:get_index(couch_mrview_index, Db, DDoc),
        ok = couch_index:compact(IndexPid, []),
        {ok, CompactorPid} = couch_index:get_compactor_pid(IndexPid),
        {ok, CompactingPid} = couch_index_compactor:get_compacting_pid(CompactorPid),
        MonRef = erlang:monitor(process, CompactingPid),
        exit(CompactingPid, crash),
        receive
            {'DOWN', MonRef, process, _, crash} ->
                meck:wait(couch_mrview_compactor, remove_compacted, '_', 100),
                ?assertEqual(1, meck:num_calls(
                    couch_mrview_compactor, remove_compacted, '_', IndexPid)),
                ?assert(is_process_alive(IndexPid)),
                ?assert(is_process_alive(CompactorPid))
        after ?TIMEOUT ->
            erlang:error(
                {assertion_failed, [
                    {module, ?MODULE}, {line, ?LINE},
                    {reason, "compaction didn't exit :/"}]})
        end
    end).


start_query(Db) ->
    Self = self(),
    Pid = spawn(fun() ->
        CB = fun
            (_, wait) -> receive {Self, continue} -> {ok, 0} end;
            ({row, _}, Count) -> {ok, Count+1};
            (_, Count) -> {ok, Count}
        end,
        {ok, Result} =
        couch_mrview:query_view(Db, <<"_design/bar">>, <<"baz">>, [], CB, wait),
        Self ! {self(), Result}
    end),
    {ok, Pid}.
