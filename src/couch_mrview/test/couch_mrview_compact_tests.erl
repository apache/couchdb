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
    Db.

teardown(Db) ->
    couch_db:close(Db),
    couch_server:delete(Db#db.name, [?ADMIN_CTX]),
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
        couch_mrview:query_view(Db, DDoc, <<"baz">>),
        {ok, Pid} = couch_index_server:get_index(couch_mrview_index, Db, DDoc),
        ok = couch_index:compact(Pid, []),
        {ok, CPid} = couch_index:get_compactor_pid(Pid),
        Info = recon:info(CPid),
        Ancs = couch_util:get_value('$ancestors',
            couch_util:get_value(dictionary,
                couch_util:get_value(meta, Info))),
        Links = couch_util:get_value(links,
            couch_util:get_value(signals, Info)),
        [CompactionPid] = Links -- Ancs,
        MonRef = erlang:monitor(process, CompactionPid),
        exit(CompactionPid, crash),
        receive
            {'DOWN', MonRef, process, _, crash} ->
                ?assert(is_process_alive(Pid)),
                ?assert(is_process_alive(CPid))
        after ?TIMEOUT ->
            erlang:error(
                {assertion_failed,
                 [{module, ?MODULE}, {line, ?LINE},
                  {reason, "compaction didn't failed :/"}]})
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
