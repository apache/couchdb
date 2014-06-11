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

-include("couch_eunit.hrl").
-include_lib("couchdb/couch_db.hrl").

-define(ADMIN_USER, {user_ctx, #user_ctx{roles=[<<"_admin">>]}}).
-define(TIMEOUT, 1000).


start() ->
    {ok, Pid} = couch_server_sup:start_link(?CONFIG_CHAIN),
    Pid.

stop(Pid) ->
    erlang:monitor(process, Pid),
    couch_server_sup:stop(),
    receive
        {'DOWN', _, _, Pid, _} ->
            ok
    after ?TIMEOUT ->
        throw({timeout, server_stop})
    end.

setup() ->
    {ok, Db} = couch_mrview_test_util:init_db(?tempdb(), map, 1000),
    Db.

teardown(Db) ->
    couch_db:close(Db),
    couch_server:delete(Db#db.name, [?ADMIN_USER]),
    ok.


compaction_test_() ->
    {
        "Compaction tests",
        {
            setup,
            fun start/0, fun stop/1,
            {
                foreach,
                fun setup/0, fun teardown/1,
                [
                    fun should_swap/1
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
