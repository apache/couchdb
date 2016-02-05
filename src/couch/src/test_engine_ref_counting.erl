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

-module(test_engine_ref_counting).
-compile(export_all).


-include_lib("eunit/include/eunit.hrl").
-include_lib("couch/include/couch_db.hrl").


-define(NUM_CLIENTS, 1000).


cet_empty_monitors() ->
    {ok, Engine, St} = test_engine_util:init_engine(),
    Pids = Engine:monitored_by(St),
    ?assert(is_list(Pids)),
    ?assertEqual([], Pids -- [self(), whereis(couch_stats_process_tracker)]).


cet_incref_decref() ->
    {ok, Engine, St} = test_engine_util:init_engine(),

    {Pid, _} = Client = start_client(Engine, St),
    wait_client(Client),

    Pids1 = Engine:monitored_by(St),
    ?assert(lists:member(Pid, Pids1)),

    close_client(Client),

    Pids2 = Engine:monitored_by(St),
    ?assert(not lists:member(Pid, Pids2)).


cet_incref_decref_many() ->
    {ok, Engine, St} = test_engine_util:init_engine(),
    Clients = lists:map(fun(_) ->
        start_client(Engine, St)
    end, lists:seq(1, ?NUM_CLIENTS)),

    lists:foreach(fun(C) -> wait_client(C) end, Clients),

    Pids1 = Engine:monitored_by(St),
    % +2 for db pid and process tracker
    ?assertEqual(?NUM_CLIENTS + 2, length(Pids1)),

    lists:foreach(fun(C) -> close_client(C) end, Clients),

    Pids2 = Engine:monitored_by(St),
    ?assertEqual(2, length(Pids2)).


start_client(Engine, St1) ->
    spawn_monitor(fun() ->
        {ok, St2} = Engine:incref(St1),

        receive
            {waiting, Pid} ->
                Pid ! go
        after 1000 ->
            erlang:error(timeout)
        end,

        receive
            close ->
                ok
        after 1000 ->
            erlang:error(timeout)
        end,

        Engine:decref(St2)
    end).


wait_client({Pid, _Ref}) ->
    Pid ! {waiting, self()},
    receive
        go -> ok
    after 1000 ->
        erlang:error(timeout)
    end.


close_client({Pid, Ref}) ->
    Pid ! close,
    receive
        {'DOWN', Ref, _, _, _} ->
            ok
    after 1000 ->
        erlang:error(timeout)
    end.

