#!/usr/bin/env escript
%% -*- erlang -*-

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

main(_) ->
    test_util:run(1, fun() -> test() end).


test() ->
    couch_server_sup:start_link(test_util:config_files()),
    {ok, Db} = couch_mrview_test_util:init_db(<<"foo">>, map, 1000),
    couch_mrview:query_view(Db, <<"_design/bar">>, <<"baz">>),
    test_swap(Db),
    ok.


test_swap(Db) ->
    {ok, QPid} = start_query(Db),    
    {ok, MonRef} = couch_mrview:compact(Db, <<"_design/bar">>, [monitor]),
    receive
        {'DOWN', MonRef, process, _, _} -> ok
    after 1000 ->
        throw(compaction_failed)
    end,
    QPid ! {self(), continue},
    receive
        {QPid, Count} ->
            etap:is(Count, 1000, "View finished successfully.")
    after 1000 ->
        throw("query failed")
    end.


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
