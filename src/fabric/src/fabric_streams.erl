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

-module(fabric_streams).

-export([
    submit_jobs/4,
    start/2,
    start/3,
    start/4,
    start/5,
    cleanup/1,
    spawn_worker_cleaner/3,
    add_worker_to_cleaner/2
]).

-include_lib("mem3/include/mem3.hrl").

-record(stream_acc, {
    workers,
    ready,
    start_fun,
    replacements,
    ring_opts
}).

-define(WORKER_CLEANER, fabric_worker_cleaner).

% This is the streams equivalent of fabric_util:submit_jobs/4. Besides
% submitting the jobs it also starts the worker cleaner and adds each started
% job to the cleaner first before the job is submitted.
%
submit_jobs(Shards, Module, EndPoint, ExtraArgs) ->
    % Create refs first and add them to the cleaner to ensure if our process
    % gets killed, the remote workers will be cleaned up as well.
    RefFun = fun(#shard{} = Shard) -> Shard#shard{ref = make_ref()} end,
    Workers = lists:map(RefFun, Shards),
    ClientReq = chttpd_util:mochiweb_client_req_get(),
    spawn_worker_cleaner(self(), Workers, ClientReq),
    SubmitFun = fun(#shard{node = Node, name = ShardName, ref = Ref}) ->
        rexi:cast_ref(Ref, Node, {Module, EndPoint, [ShardName | ExtraArgs]})
    end,
    ok = lists:foreach(SubmitFun, Workers),
    Workers.

start(Workers, Keypos) ->
    start(Workers, Keypos, undefined, undefined).

start(Workers, Keypos, RingOpts) ->
    start(Workers, Keypos, undefined, undefined, RingOpts).

start(Workers, Keypos, StartFun, Replacements) ->
    start(Workers, Keypos, StartFun, Replacements, []).

start(Workers0, Keypos, StartFun, Replacements, RingOpts) ->
    Fun = fun handle_stream_start/3,
    Acc = #stream_acc{
        workers = fabric_dict:init(Workers0, waiting),
        ready = [],
        start_fun = StartFun,
        replacements = Replacements,
        ring_opts = RingOpts
    },
    ClientReq = chttpd_util:mochiweb_client_req_get(),
    spawn_worker_cleaner(self(), Workers0, ClientReq),
    Timeout = fabric_util:request_timeout(),
    case rexi_utils:recv(Workers0, Keypos, Fun, Acc, Timeout, infinity) of
        {ok, #stream_acc{ready = Workers}} ->
            AckedWorkers = fabric_dict:fold(
                fun(Worker, From, WorkerAcc) ->
                    rexi:stream_start(From),
                    [Worker | WorkerAcc]
                end,
                [],
                Workers
            ),
            {ok, AckedWorkers};
        {timeout, #stream_acc{workers = Defunct}} ->
            cleanup(Workers0),
            DefunctWorkers = fabric_util:remove_done_workers(Defunct, waiting),
            {timeout, DefunctWorkers};
        Else ->
            cleanup(Workers0),
            Else
    end.

cleanup(Workers) ->
    % Stop the auxiliary cleaner process as we got to the point where cleanup
    % happens in the regular fashion and we don't want to send 2x the number
    % of kill messages.
    %
    % First, we run the cleanup/1 function, then, we stop the cleaner;
    % otherwise there is a tiny risk we get killed after we stop the process
    % and before finish calling cleanup/1. This early, forced process kill may
    % happen when running the recovery login in the ddoc cache.
    %
    Res = fabric_util:cleanup(Workers),
    case get(?WORKER_CLEANER) of
        CleanerPid when is_pid(CleanerPid) ->
            erase(?WORKER_CLEANER),
            exit(CleanerPid, kill);
        _ ->
            ok
    end,
    Res.

handle_stream_start({rexi_DOWN, _, {_, NodeRef}, _}, _, St) ->
    #stream_acc{workers = Workers, ready = Ready, ring_opts = RingOpts} = St,
    case fabric_ring:node_down(NodeRef, Workers, Ready, RingOpts) of
        {ok, Workers1} ->
            {ok, St#stream_acc{workers = Workers1}};
        error ->
            {error, {nodedown, <<"progress not possible">>}}
    end;
handle_stream_start({rexi_EXIT, Reason}, Worker, St) ->
    #stream_acc{
        workers = Workers,
        ready = Ready,
        replacements = Replacements,
        ring_opts = RingOpts
    } = St,
    case {fabric_ring:handle_error(Worker, Workers, Ready, RingOpts), Reason} of
        {{ok, Workers1}, _Reason} ->
            {ok, St#stream_acc{workers = Workers1}};
        {error, {maintenance_mode, _Node}} when Replacements /= undefined ->
            % Check if we have replacements for this range
            % and start the new workers if so.
            case lists:keytake(Worker#shard.range, 1, Replacements) of
                {value, {_Range, WorkerReplacements}, NewReplacements} ->
                    FinalWorkers = lists:foldl(
                        fun(Repl, NewWorkers) ->
                            NewWorker = (St#stream_acc.start_fun)(Repl),
                            add_worker_to_cleaner(self(), NewWorker),
                            fabric_dict:store(NewWorker, waiting, NewWorkers)
                        end,
                        Workers,
                        WorkerReplacements
                    ),
                    % Assert that our replaced worker provides us
                    % the oppurtunity to make progress. Need to make sure
                    % to include already processed responses, since we are
                    % checking the full range and some workers have already
                    % responded and were removed from the workers list
                    ReadyWorkers = [{W, R} || {_, W, R} <- Ready],
                    AllWorkers = FinalWorkers ++ ReadyWorkers,
                    true = fabric_ring:is_progress_possible(AllWorkers),
                    NewRefs = fabric_dict:fetch_keys(FinalWorkers),
                    {new_refs, NewRefs, St#stream_acc{
                        workers = FinalWorkers,
                        replacements = NewReplacements
                    }};
                false ->
                    % If we progress isn't possible and we don't have any
                    % replacements then we're dead in the water.
                    {error, {nodedown, <<"progress not possible">>}}
            end;
        {error, _} ->
            {error, fabric_util:error_info(Reason)}
    end;
handle_stream_start(rexi_STREAM_INIT, {Worker, From}, St) ->
    #stream_acc{workers = Workers, ready = Ready, ring_opts = RingOpts} = St,
    case fabric_dict:lookup_element(Worker, Workers) of
        undefined ->
            % This worker lost the race with other partition copies, terminate
            rexi:stream_cancel(From),
            {ok, St};
        waiting ->
            case fabric_ring:handle_response(Worker, From, Workers, Ready, RingOpts) of
                {ok, {Workers1, Ready1}} ->
                    % Don't have a full ring yet. Keep getting responses
                    {ok, St#stream_acc{workers = Workers1, ready = Ready1}};
                {stop, Ready1} ->
                    % Have a full ring of workers. But don't ack the worker
                    % yet so they don't start sending us rows until we're ready
                    {stop, St#stream_acc{workers = [], ready = Ready1}}
            end
    end;
handle_stream_start({ok, Error}, _, _) when Error == ddoc_updated; Error == insufficient_storage ->
    {stop, Error};
handle_stream_start(Else, _, _) ->
    exit({invalid_stream_start, Else}).

% Spawn an auxiliary rexi worker cleaner. This will be used in cases
% when the coordinator (request) process is forceably killed and doesn't
% get a chance to process its `after` fabric:clean/1 clause.
spawn_worker_cleaner(Coordinator, Workers, ClientReq) when
    is_pid(Coordinator), is_list(Workers)
->
    case get(?WORKER_CLEANER) of
        undefined ->
            Pid = spawn(fun() ->
                monitor(process, Coordinator),
                NodeRefSet = couch_util:set_from_list(shards_to_node_refs(Workers)),
                cleaner_loop(Coordinator, NodeRefSet, ClientReq)
            end),
            put(?WORKER_CLEANER, Pid),
            Pid;
        ExistingCleaner when is_pid(ExistingCleaner) ->
            ExistingCleaner
    end.

cleaner_loop(Pid, NodeRefSet, ClientReq) ->
    CheckMSec = chttpd_util:mochiweb_client_req_check_msec(),
    receive
        {add_node_ref, Pid, {_, _} = NodeRef} ->
            cleaner_loop(Pid, sets:add_element(NodeRef, NodeRefSet), ClientReq);
        {'DOWN', _, _, Pid, _} ->
            rexi:kill_all(sets:to_list(NodeRefSet))
    after CheckMSec ->
        chttpd_util:stop_client_process_if_disconnected(Pid, ClientReq),
        cleaner_loop(Pid, NodeRefSet, ClientReq)
    end.

add_worker_to_cleaner(CoordinatorPid, #shard{node = Node, ref = Ref}) ->
    case get(?WORKER_CLEANER) of
        CleanerPid when is_pid(CleanerPid) ->
            CleanerPid ! {add_node_ref, CoordinatorPid, {Node, Ref}};
        _ ->
            ok
    end.

shards_to_node_refs(Workers) when is_list(Workers) ->
    Fun = fun(#shard{node = Node, ref = Ref}) -> {Node, Ref} end,
    lists:map(Fun, Workers).

-ifdef(TEST).

-include_lib("couch/include/couch_eunit.hrl").

worker_cleaner_test_() ->
    {
        "Fabric spawn_worker_cleaner test",
        {
            foreach,
            fun setup/0,
            fun teardown/1,
            [
                ?TDEF_FE(should_clean_workers),
                ?TDEF_FE(does_not_fire_if_cleanup_called),
                ?TDEF_FE(should_clean_additional_worker_too),
                ?TDEF_FE(coordinator_is_killed_if_client_disconnects),
                ?TDEF_FE(coordinator_is_not_killed_if_client_is_connected),
                ?TDEF_FE(submit_jobs_sets_up_cleaner),
                ?TDEF_FE(cleanup_called_on_timeout),
                ?TDEF_FE(cleanup_called_on_error)
            ]
        }
    }.

should_clean_workers(_) ->
    meck:reset(rexi),
    erase(?WORKER_CLEANER),
    Workers = [
        #shard{node = 'n1', ref = make_ref()},
        #shard{node = 'n2', ref = make_ref()}
    ],
    {Coord, _} = spawn_monitor(fun() ->
        receive
            die -> ok
        end
    end),
    Cleaner = spawn_worker_cleaner(Coord, Workers, undefined),
    Ref = monitor(process, Cleaner),
    Coord ! die,
    receive
        {'DOWN', Ref, _, Cleaner, _} -> ok
    end,
    ?assertEqual(1, meck:num_calls(rexi, kill_all, 1)).

does_not_fire_if_cleanup_called(_) ->
    meck:reset(rexi),
    erase(?WORKER_CLEANER),
    Workers = [
        #shard{node = 'n1', ref = make_ref()},
        #shard{node = 'n2', ref = make_ref()}
    ],
    {Coord, _} = spawn_monitor(fun() ->
        receive
            die -> ok
        end
    end),
    Cleaner = spawn_worker_cleaner(Coord, Workers, undefined),
    Ref = monitor(process, Cleaner),
    cleanup(Workers),
    Coord ! die,
    receive
        {'DOWN', Ref, _, _, _} -> ok
    end,
    % 2 calls would be from cleanup/1 function. If cleanup process fired
    % too it would have been 4 calls total.
    ?assertEqual(1, meck:num_calls(rexi, kill_all, 1)).

should_clean_additional_worker_too(_) ->
    meck:reset(rexi),
    erase(?WORKER_CLEANER),
    Workers = [
        #shard{node = 'n1', ref = make_ref()}
    ],
    {Coord, _} = spawn_monitor(fun() ->
        receive
            die -> ok
        end
    end),
    Cleaner = spawn_worker_cleaner(Coord, Workers, undefined),
    add_worker_to_cleaner(Coord, #shard{node = 'n2', ref = make_ref()}),
    Ref = monitor(process, Cleaner),
    Coord ! die,
    receive
        {'DOWN', Ref, _, Cleaner, _} -> ok
    end,
    ?assertEqual(1, meck:num_calls(rexi, kill_all, 1)).

coordinator_is_killed_if_client_disconnects(_) ->
    meck:reset(rexi),
    erase(?WORKER_CLEANER),
    Workers = [
        #shard{node = 'n1', ref = make_ref()},
        #shard{node = 'n2', ref = make_ref()}
    ],
    {Coord, CoordRef} = spawn_monitor(fun() ->
        receive
            die -> ok
        end
    end),
    Headers = mochiweb_headers:make([]),
    {ok, Sock} = gen_tcp:listen(0, [{active, false}]),
    ClientReq = mochiweb_request:new(Sock, 'GET', "/foo", {1, 1}, Headers),
    % Close the socket and then expect coordinator to be killed
    ok = gen_tcp:close(Sock),
    Cleaner = spawn_worker_cleaner(Coord, Workers, ClientReq),
    CleanerRef = monitor(process, Cleaner),
    % Assert the correct behavior on the support platforms (all except Windows so far)
    case os:type() of
        {unix, Type} when
            Type =:= linux;
            Type =:= darwin;
            Type =:= freebsd;
            Type =:= openbsd;
            Type =:= netbsd
        ->
            % Coordinator should be torn down
            receive
                {'DOWN', CoordRef, _, _, Reason} ->
                    ?assertEqual({shutdown, client_disconnected}, Reason)
            end,
            % Cleaner process itself should exit
            receive
                {'DOWN', CleanerRef, _, _, _} -> ok
            end,
            % Workers should have been killed
            ?assertEqual(1, meck:num_calls(rexi, kill_all, 1));
        {_, _} = OsType ->
            ?debugFmt("~n * Client disconnect test not yet supported on ~p~n", [OsType])
    end.

coordinator_is_not_killed_if_client_is_connected(_) ->
    meck:reset(rexi),
    erase(?WORKER_CLEANER),
    Workers = [
        #shard{node = 'n1', ref = make_ref()},
        #shard{node = 'n2', ref = make_ref()}
    ],
    {Coord, CoordRef} = spawn_monitor(fun() ->
        receive
            die -> ok
        end
    end),
    Headers = mochiweb_headers:make([]),
    {ok, Sock} = gen_tcp:listen(0, [{active, false}]),
    ClientReq = mochiweb_request:new(Sock, 'GET', "/foo", {1, 1}, Headers),
    Cleaner = spawn_worker_cleaner(Coord, Workers, ClientReq),
    CleanerRef = monitor(process, Cleaner),
    % Coordinator should stay up
    receive
        {'DOWN', CoordRef, _, Coord, _} ->
            ?assert(false, {unexpected_coordinator_exit, Coord})
    after 1000 ->
        ?assert(is_process_alive(Coord))
    end,
    % Cleaner process stays up
    ?assert(is_process_alive(Cleaner)),
    % Tear everything down at the end of the test
    gen_tcp:close(Sock),
    Coord ! die,
    receive
        {'DOWN', CleanerRef, _, _, _} -> ok
    end.

submit_jobs_sets_up_cleaner(_) ->
    meck:reset(rexi),
    erase(?WORKER_CLEANER),
    Shards = [
        #shard{node = 'n1'},
        #shard{node = 'n2'}
    ],
    meck:expect(rexi, cast_ref, fun(Ref, _, _) -> Ref end),
    {Coord, CoordRef} = spawn_monitor(fun() ->
        Workers = submit_jobs(Shards, fabric_rpc, potatoes, []),
        receive
            {get_workers_and_cleaner, From} ->
                From ! {Workers, get(?WORKER_CLEANER)},
                timer:sleep(999999)
        end
    end),
    Coord ! {get_workers_and_cleaner, self()},
    {Workers, Cleaner} =
        receive
            Msg -> Msg
        end,
    ?assert(is_pid(Cleaner)),
    ?assert(is_process_alive(Cleaner)),
    ?assert(is_process_alive(Coord)),
    CheckWorkerFun = fun(#shard{node = Node, ref = Ref}) ->
        ?assert(is_reference(Ref)),
        {Node, Ref}
    end,
    NodeRefs = lists:map(CheckWorkerFun, Workers),
    ?assertEqual(length(Shards), length(Workers)),
    ?assertEqual(length(lists:usort(NodeRefs)), length(NodeRefs)),
    % Were the jobs actually submitted?
    meck:wait(2, rexi, cast_ref, '_', 1000),
    % If we kill the coordinator, the cleaner should kill the workers
    meck:reset(rexi),
    CleanupMon = monitor(process, Cleaner),
    exit(Coord, kill),
    receive
        {'DOWN', CoordRef, _, _, WorkerReason} ->
            ?assertEqual(killed, WorkerReason)
    after 1000 ->
        ?assert(is_process_alive(Coord))
    end,
    % Cleaner should do the cleanup
    meck:wait(1, rexi, kill_all, '_', 1000),
    History = meck:history(rexi),
    ?assertMatch([{_, {rexi, kill_all, _}, ok}], History),
    [{Pid, {rexi, kill_all, Args}, ok}] = History,
    % It was the cleaner who called it
    ?assertEqual(Cleaner, Pid),
    ?assertMatch([[{_, _}, {_, _}]], Args),
    [NodeRefsKilled] = Args,
    % The node refs killed are the ones we expect
    ?assertEqual(lists:sort(NodeRefs), lists:sort(NodeRefsKilled)),
    % Cleanup process should exit when done
    receive
        {'DOWN', CleanupMon, _, _, CleanerReason} ->
            ?assertEqual(normal, CleanerReason)
    after 1000 ->
        ?assert(is_process_alive(Cleaner))
    end.

cleanup_called_on_timeout(_) ->
    Ref1 = make_ref(),
    Ref2 = make_ref(),
    W1 = #shard{node = 'n1', ref = Ref1},
    W2 = #shard{node = 'n2', ref = Ref2},
    Workers = [W1, W2],
    meck:expect(rexi_utils, recv, fun(_, _, _, Acc, _, _) ->
        {timeout, Acc#stream_acc{workers = [{W2, waiting}]}}
    end),
    meck:reset(fabric_util),
    Res = start(Workers, #shard.ref, undefined, undefined, []),
    ?assertEqual({timeout, [W2]}, Res),
    ?assert(meck:called(fabric_util, cleanup, 1)).

cleanup_called_on_error(_) ->
    Ref1 = make_ref(),
    Ref2 = make_ref(),
    W1 = #shard{node = 'n1', ref = Ref1},
    W2 = #shard{node = 'n2', ref = Ref2},
    Workers = [W1, W2],
    meck:expect(rexi_utils, recv, fun(_, _, _, _, _, _) ->
        {error, foo}
    end),
    meck:reset(fabric_util),
    Res = start(Workers, #shard.ref, undefined, undefined, []),
    ?assertEqual({error, foo}, Res),
    ?assert(meck:called(fabric_util, cleanup, 1)).

setup() ->
    meck:new(rexi_utils, [passthrough]),
    meck:new(config, [passthrough]),
    meck:new(fabric_util, [passthrough]),
    meck:expect(config, get, fun(_, _, Default) -> Default end),
    meck:expect(rexi, kill_all, fun(_) -> ok end),
    % Speed up disconnect socket timeout for the test to 200 msec
    meck:expect(chttpd_util, mochiweb_client_req_check_msec, 0, 200).

teardown(_) ->
    meck:unload().

-endif.
