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

-module(cpse_test_ref_counting).
-compile(export_all).


-include_lib("eunit/include/eunit.hrl").
-include_lib("couch/include/couch_db.hrl").


-define(NUM_CLIENTS, 1000).


setup_each() ->
    {ok, Db} = cpse_util:create_db(),
    {Db, self()}.


teardown_each({Db, _}) ->
    ok = couch_server:delete(couch_db:name(Db), []).


cpse_empty_monitors({Db, Pid}) ->
    Pids = couch_db_engine:monitored_by(Db),
    ?assert(is_list(Pids)),
    Expected = [
        Pid,
        couch_db:get_pid(Db),
        whereis(couch_stats_process_tracker)
    ],
    ?assertEqual([], Pids -- Expected).


cpse_incref_decref({Db, _}) ->
    {Pid, _} = Client = start_client(Db),
    wait_client(Client),

    Pids1 = couch_db_engine:monitored_by(Db),
    ?assert(lists:member(Pid, Pids1)),

    close_client(Client),

    Pids2 = couch_db_engine:monitored_by(Db),
    ?assert(not lists:member(Pid, Pids2)).


cpse_incref_decref_many({Db, _}) ->
    Clients = lists:map(fun(_) ->
        start_client(Db)
    end, lists:seq(1, ?NUM_CLIENTS)),

    lists:foreach(fun(C) -> wait_client(C) end, Clients),

    Pids1 = couch_db_engine:monitored_by(Db),
    % +3 for self, db pid, and process tracker
    ?assertEqual(?NUM_CLIENTS + 3, length(Pids1)),

    lists:foreach(fun(C) -> close_client(C) end, Clients),

    Pids2 = couch_db_engine:monitored_by(Db),
    ?assertEqual(3, length(Pids2)).


start_client(Db0) ->
    spawn_monitor(fun() ->
        {ok, Db1} = couch_db:open_int(couch_db:name(Db0), []),

        receive
            {waiting, Pid} ->
                Pid ! go
        after 1000 ->
            erlang:error(timeout)
        end,

        receive
            close ->
                couch_db:close(Db1),
                ok
        after 1000 ->
            erlang:error(timeout)
        end
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

