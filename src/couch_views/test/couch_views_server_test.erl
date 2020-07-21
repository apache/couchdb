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

-module(couch_views_server_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("fabric/test/fabric2_test.hrl").


couch_views_server_test_() ->
    {
        "Test couch_views_server",
        {
            setup,
            fun setup/0,
            fun cleanup/1,
            {
                foreach,
                fun foreach_setup/0,
                fun foreach_teardown/1,
                [
                    ?TDEF_FE(max_acceptors_started),
                    ?TDEF_FE(acceptors_become_workers),
                    ?TDEF_FE(handle_worker_death),
                    ?TDEF_FE(handle_acceptor_death),
                    ?TDEF_FE(handle_unknown_process_death),
                    ?TDEF_FE(max_workers_limit_works),
                    ?TDEF_FE(max_acceptors_greater_than_max_workers)
                ]
            }
        }
    }.


setup() ->
    Ctx = test_util:start_couch([
            fabric,
            couch_jobs,
            couch_rate,
            couch_js,
            couch_eval,
            ebtree
        ]),
    Ctx.


cleanup(Ctx) ->
    test_util:stop_couch(Ctx).


foreach_setup() ->
    config:set("couch_views", "max_acceptors", "2", false),
    config:set("couch_views", "max_workers", "4", false),
    meck:new(couch_views_server, [passthrough]),
    meck:new(couch_views_indexer, [passthrough]),
    meck:expect(couch_views_indexer, init, fun() ->
        receive pls_accept -> ok end,
        couch_views_server:accepted(self()),
        receive pls_die -> ok end
    end),
    ok = application:start(couch_views).


foreach_teardown(_) ->
    ok = application:stop(couch_views),
    meck:unload(),
    config:delete("couch_views", "max_acceptors", false),
    config:delete("couch_views", "max_workers", false),
    ok.


max_acceptors_started(_) ->
    #{max_acceptors := MaxAcceptors, max_workers := MaxWorkers} = get_state(),
    ?assertEqual(2, MaxAcceptors),
    ?assertEqual(4, MaxWorkers),

    ?assertEqual(0, maps:size(workers())),

    [Pid1, Pid2] = maps:keys(acceptors()),
    ?assert(is_pid(Pid1)),
    ?assert(is_pid(Pid2)),
    ?assert(is_process_alive(Pid1)),
    ?assert(is_process_alive(Pid2)).


acceptors_become_workers(_) ->
    ?assertEqual(0, maps:size(workers())),

    InitAcceptors = acceptors(),
    accept_all(),

    ?assertEqual(2, maps:size(acceptors())),
    ?assertEqual(2, maps:size(workers())),

    ?assertEqual(InitAcceptors, workers()).


handle_worker_death(_) ->
    [Pid1, Pid2] = maps:keys(acceptors()),
    accept_all(),

    % One worker exits normal
    finish_normal([Pid1]),
    ?assertEqual(2, maps:size(acceptors())),
    ?assertEqual(1, maps:size(workers())),

    % The other blows up with an error
    finish_error([Pid2]),
    ?assertEqual(2, maps:size(acceptors())),
    ?assertEqual(0, maps:size(workers())).


handle_acceptor_death(_) ->
    [Pid1, Pid2] = maps:keys(acceptors()),
    finish_error([Pid1]),

    NewAcceptors = acceptors(),
    ?assertEqual(2, maps:size(NewAcceptors)),
    ?assert(lists:member(Pid2, maps:keys(NewAcceptors))),
    ?assert(not lists:member(Pid1, maps:keys(NewAcceptors))).


handle_unknown_process_death(_) ->
    meck:reset(couch_views_server),
    Pid = self(),
    whereis(couch_views_server) ! {'EXIT', Pid, blah},
    meck:wait(1, couch_views_server, terminate,
        [{unknown_pid_exit, Pid}, '_'], 5000).


max_workers_limit_works(_) ->
    % Accept 2 jobs -> 2 workers
    accept_all(),
    ?assertEqual(2, maps:size(workers())),

    % Accept 2 more jobs -> 4 workers
    accept_all(),
    ?assertEqual(0, maps:size(acceptors())),
    ?assertEqual(4, maps:size(workers())),

    % Kill 1 worker -> 1 acceptor and 3 workers
    [Worker1 | _] = maps:keys(workers()),
    finish_normal([Worker1]),
    ?assertEqual(1, maps:size(acceptors())),
    ?assertEqual(3, maps:size(workers())),

    % Kill 2 more workers -> 2 acceptors and 1 worker
    [Worker2, Worker3 | _] = maps:keys(workers()),
    finish_normal([Worker2, Worker3]),
    ?assertEqual(2, maps:size(acceptors())),
    ?assertEqual(1, maps:size(workers())),

    % Kill 1 last worker -> 2 acceptors and 0 workers
    [Worker4] = maps:keys(workers()),
    finish_normal([Worker4]),
    ?assertEqual(2, maps:size(acceptors())),
    ?assertEqual(0, maps:size(workers())).

max_acceptors_greater_than_max_workers(_) ->
    [Pid1, Pid2] = maps:keys(acceptors()),

    sys:replace_state(couch_views_server, fun(#{} = St) ->
        St#{max_workers := 1}
    end),

    accept_all(),

    finish_normal([Pid1]),
    finish_normal([Pid2]),

    % Only 1 acceptor should start as it is effectively limited by max_workers
    ?assertEqual(1, maps:size(acceptors())),
    ?assertEqual(0, maps:size(workers())).


% Utility functions

accept_all() ->
    Acceptors = acceptors(),
    meck:reset(couch_views_server),
    [Pid ! pls_accept || Pid <- maps:keys(Acceptors)],
    meck:wait(maps:size(Acceptors), couch_views_server, handle_call, 3, 5000).


acceptors() ->
    #{acceptors := Acceptors} = get_state(),
    Acceptors.


workers() ->
    #{workers := Workers} = get_state(),
    Workers.


get_state() ->
    sys:get_state(couch_views_server, infinity).


finish_normal(Workers) when is_list(Workers) ->
    meck:reset(couch_views_server),
    [Pid ! pls_die || Pid <- Workers],
    meck:wait(length(Workers), couch_views_server, handle_info,
        [{'_', '_', normal}, '_'], 5000).


finish_error(Workers) when is_list(Workers) ->
    meck:reset(couch_views_server),
    [exit(Pid, badness) || Pid <- Workers],
    meck:wait(length(Workers), couch_views_server, handle_info,
        [{'_', '_', badness}, '_'], 5000).
