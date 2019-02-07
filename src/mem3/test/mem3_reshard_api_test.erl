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

-module(mem3_reshard_api_test).


-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("mem3/src/mem3_reshard.hrl").


-define(USER, "mem3_reshard_api_test_admin").
-define(PASS, "pass").
-define(AUTH, {basic_auth, {?USER, ?PASS}}).
-define(JSON, {"Content-Type", "application/json"}).
-define(DB1, "mem3_reshard_api_test_db1").
-define(DB2, "mem3_reshard_api_test_db2").
-define(DB3, "mem3_reshard_api_test_db3").
-define(RESHARD, "_reshard/").
-define(JOBS, "_reshard/jobs/").
-define(STATE, "_reshard/state").
-define(ID, <<"id">>).


setup() ->
    Hashed = couch_passwords:hash_admin_password(?PASS),
    ok = config:set("admins", ?USER, ?b2l(Hashed), _Persist=false),
    %ok = config:set("mem3_reshard", "store_state", "false", false),
    Addr = config:get("chttpd", "bind_address", "127.0.0.1"),
    Port = mochiweb_socket_server:get(chttpd, port),
    Url = lists:concat(["http://", Addr, ":", Port, "/"]),
    create_db(Url ++ ?DB1 ++ "?q=1&n=1"),
    create_db(Url ++ ?DB2 ++ "?q=1&n=1"),
    create_db(Url ++ ?DB3 ++ "?q=2&n=1"),
    %mem3_reshard_debug:state_intercept_init(),
    Url.


teardown(Url) ->
    meck:unload(),
    mem3_reshard:reset_state(),
    %mem3_reshard_debug:state_intercept_cleanup(),
    delete_db(Url ++ ?DB1),
    delete_db(Url ++ ?DB2),
    delete_db(Url ++ ?DB3),
    ok = config:delete("admins", ?USER, _Persist=false).


start_couch() ->
    test_util:start_couch(?CONFIG_CHAIN, [mem3, chttpd]).


stop_couch(Ctx) ->
    test_util:stop_couch(Ctx).


mem3_reshard_api_test_() ->
    {
        "mem3 shard split api tests",
        {
            setup,
            fun start_couch/0, fun stop_couch/1,
            {
                foreach,
                fun setup/0, fun teardown/1,
                [
                    fun basics/1,
                    fun create_job_basic/1,
                    fun create_two_jobs/1,
                    fun start_stop_cluster_basic/1,
                    fun start_stop_cluster_with_a_job/1,
                    fun individual_job_start_stop/1,
                    fun individual_job_stop_when_cluster_stopped/1
                ]
            }
        }
    }.


basics(Top) ->
    ?_test(begin
        % GET /_reshard
        ?assertMatch({200, #{
            <<"state">> := <<"running">>,
            <<"state_reason">> := null,
            <<"completed">> := 0,
            <<"failed">> := 0,
            <<"running">> := 0,
            <<"stopped">> := 0,
            <<"total">> := 0
        }}, req(get, Top ++ ?RESHARD)),
        % GET _reshard/state
        ?assertMatch({200, #{<<"state">> := <<"running">>}},
            req(get, Top ++ ?STATE)),
        % GET _reshard/jobs
        ?assertMatch({200, #{
            <<"jobs">> := [],
            <<"offset">> := 0,
            <<"total_rows">> := 0
        }}, req(get, Top ++ ?JOBS)),
        % Some invalid paths and methods
        ?assertMatch({404, _}, req(get, Top ++ ?RESHARD ++ "/invalidpath")),
        ?assertMatch({405, _}, req(put, Top ++ ?RESHARD, #{dont => thinkso})),
        ?assertMatch({405, _}, req(post, Top ++ ?RESHARD, #{notgonna => happen}))
    end).


create_job_basic(Top) ->
    ?_test(begin
        % POST /_reshard/jobs
        {C1, R1} = req(post, Top ++ ?JOBS,  #{type => split, db => <<?DB1>>}),
        ?assertEqual(201, C1),
        ?assertMatch([#{<<"ok">> := true, <<"id">> := J, <<"shard">> :=  S}]
            when is_binary(J) andalso is_binary(S), R1),
        [#{?ID := Id, <<"shard">> := Shard}] = R1,
        % GET /_reshard/jobs
        ?assertMatch({200, #{
            <<"jobs">> := [#{?ID := Id, <<"type">> := <<"split">>}],
            <<"offset">> := 0,
            <<"total_rows">> := 1
        }}, req(get, Top ++ ?JOBS)),
        % GET /_reshard/job/$jobid
        {C2, R2} = req(get, Top ++ ?JOBS ++ ?b2l(Id)),
        ?assertEqual(200, C2),
        ThisNode = atom_to_binary(node(), utf8),
        ?assertMatch(#{?ID := Id}, R2),
        ?assertMatch(#{<<"type">> := <<"split">>}, R2),
        ?assertMatch(#{<<"source">> := Shard}, R2),
        ?assertMatch(#{<<"history">> := History} when length(History) > 1, R2),
        ?assertMatch(#{<<"node">> := ThisNode}, R2),
        ?assertMatch(#{<<"split_state">> := SSt} when is_binary(SSt), R2),
        ?assertMatch(#{<<"job_state">> := JSt} when is_binary(JSt), R2),
        ?assertMatch(#{<<"state_info">> := #{}}, R2),
        ?assertMatch(#{<<"targets">> := Targets} when length(Targets) == 2, R2),
        % GET  /_reshard/job/$jobid/state
        ?assertMatch({200, #{<<"state">> := S, <<"reason">> := R}}
            when is_binary(S) andalso (is_binary(R) orelse R =:= null),
            req(get, Top ++ ?JOBS ++ ?b2l(Id) ++ "/state")),
        % GET /_reshard
        ?assertMatch({200, #{<<"state">> := <<"running">>, <<"total">> := 1}},
            req(get, Top ++ ?RESHARD)),
        % DELETE /_reshard/jobs/$jobid
        ?assertMatch({200, #{<<"ok">> := true}},
            req(delete, Top ++ ?JOBS ++ ?b2l(Id))),
        % GET _reshard/jobs
        ?assertMatch({200, #{<<"jobs">> := [], <<"total_rows">> := 0}},
            req(get, Top ++ ?JOBS)),
        % GET /_reshard/job/$jobid should be a 404
        ?assertMatch({404, #{}}, req(get, Top ++ ?JOBS ++ ?b2l(Id))),
        % DELETE /_reshard/jobs/$jobid  should be a 404 as well
        ?assertMatch({404, #{}}, req(delete, Top ++ ?JOBS ++ ?b2l(Id)))
    end).


create_two_jobs(Top) ->
    ?_test(begin
        ?assertMatch({201, [#{<<"ok">> := true}]},
            req(post, Top ++ ?JOBS, #{type => split, db => <<?DB1>>})),
        ?assertMatch({201, [#{<<"ok">> := true}]},
            req(post, Top ++ ?JOBS, #{type => split, db => <<?DB2>>})),
        ?assertMatch({200, #{
            <<"jobs">> := [#{?ID := Id1}, #{?ID := Id2}],
            <<"offset">> := 0,
            <<"total_rows">> := 2
        }} when Id1 =/= Id2, req(get, Top ++ ?JOBS)),
        ?assertMatch({200, #{<<"total">> := 2}}, req(get, Top ++ ?RESHARD)),
        {200, #{<<"jobs">> := [#{?ID := Id1}, #{?ID := Id2}]}} =
            req(get, Top ++ ?JOBS),
        {200, #{<<"ok">> := true}} = req(delete, Top ++ ?JOBS ++ ?b2l(Id1)),
        ?assertMatch({200, #{<<"total">> := 1}}, req(get, Top ++ ?RESHARD)),
        {200, #{<<"ok">> := true}} = req(delete, Top ++ ?JOBS ++ ?b2l(Id2)),
        ?assertMatch({200, #{<<"total">> := 0}}, req(get, Top ++ ?RESHARD))
    end).


start_stop_cluster_basic(Top) ->
    ?_test(begin
        Url = Top ++ ?STATE,
        ?assertMatch({200, #{
            <<"state">> := <<"running">>,
            <<"reason">> := null
        }}, req(get, Url)),
        ?assertMatch({200, _}, req(put, Url, #{state => stopped})),
        ?assertMatch({200, #{
            <<"state">> := <<"stopped">>,
            <<"reason">> := R
        }} when is_binary(R), req(get, Url)),
        ?assertMatch({200, _}, req(put, Url, #{state => running})),
        % Make sure the reason shows in the state GET request
        Reason = <<"somereason">>,
        ?assertMatch({200, _}, req(put, Url, #{state => stopped,
            reason => Reason})),
        ?assertMatch({200, #{<<"state">> := <<"stopped">>,
            <<"reason">> := Reason}}, req(get, Url)),
        % Top level summary also shows the reason
        ?assertMatch({200, #{
            <<"state">> := <<"stopped">>,
            <<"state_reason">> := Reason
        }}, req(get, Top ++ ?RESHARD)),
        ?assertMatch({200, _}, req(put, Url, #{state => running})),
        ?assertMatch({200, #{<<"state">> := <<"running">>}}, req(get, Url))
    end).


start_stop_cluster_with_a_job(Top) ->
    ?_test(begin
        Url = Top ++ ?STATE,
        ?assertMatch({200, _}, req(put, Url, #{state => stopped})),
        ?assertMatch({200, #{<<"state">> := <<"stopped">>}}, req(get, Url)),
        % Can add jobs with global state stopped, they just won't be running
        {201, R1} = req(post, Top ++ ?JOBS, #{type => split, db => <<?DB1>>}),
        ?assertMatch([#{<<"ok">> := true}], R1),
        [#{?ID := Id1}] = R1,
        {200, J1} = req(get, Top ++ ?JOBS ++ ?b2l(Id1)),
        ?assertMatch(#{?ID := Id1, <<"job_state">> := <<"stopped">>}, J1),
        % Check summary stats
        ?assertMatch({200, #{
            <<"state">> := <<"stopped">>,
            <<"running">> := 0,
            <<"stopped">> := 1,
            <<"total">> := 1
        }}, req(get, Top ++ ?RESHARD)),
        % Can delete the job when stopped
        {200, #{<<"ok">> := true}} = req(delete, Top ++ ?JOBS ++ ?b2l(Id1)),
        ?assertMatch({200, #{
            <<"state">> := <<"stopped">>,
            <<"running">> := 0,
            <<"stopped">> := 0,
            <<"total">> := 0
        }}, req(get, Top ++ ?RESHARD)),
        % Add same job again
        {201, [#{?ID := Id2}]} = req(post, Top ++ ?JOBS, #{type => split,
            db => <<?DB1>>}),
        ?assertMatch({200, #{?ID := Id2, <<"job_state">> := <<"stopped">>}},
            req(get, Top ++ ?JOBS ++ ?b2l(Id2))),
        % Job should start after resharding is started on the cluster
        ?assertMatch({200, _}, req(put, Url, #{state => running})),
        ?assertMatch({200, #{?ID := Id2, <<"job_state">> := JSt}}
            when JSt =/= <<"stopped">>, req(get, Top ++ ?JOBS ++ ?b2l(Id2))),
        {200, #{<<"ok">> := true}} = req(delete, Top ++ ?JOBS ++ ?b2l(Id2))
     end).


individual_job_start_stop(Top) ->
    ?_test(begin
        intercept_state(topoff1),
        Body = #{type => split, db => <<?DB1>>},
        {201, [#{?ID := Id}]} = req(post, Top ++ ?JOBS, Body),
        JobUrl = Top ++ ?JOBS ++ ?b2l(Id),
        StUrl =  JobUrl ++ "/state",
        % Wait for the the job to start running and intercept it in topoff1 state
        receive {JobPid, topoff1} -> ok end,
        % Tell the intercept to never finish checkpointing so job is left hanging
        % forever in running state
        JobPid ! cancel,
        ?assertMatch({200, #{<<"state">> := <<"running">>}}, req(get, StUrl)),
        {200, _} = req(put, StUrl, #{state => stopped}),
        wait_state(StUrl, <<"stopped">>),
        % Stop/start resharding globally and job should still stay stopped
        ?assertMatch({200, _}, req(put, Top ++ ?STATE, #{state => stopped})),
        ?assertMatch({200, _}, req(put, Top ++ ?STATE, #{state => running})),
        ?assertMatch({200, #{<<"state">> := <<"stopped">>}}, req(get, StUrl)),
        % Start the job again
        ?assertMatch({200, _}, req(put, StUrl, #{state => running})),
        % Wait for the the job to start running and intercept it in topoff1 state
        receive {JobPid2, topoff1} -> ok end,
        ?assertMatch({200, #{<<"state">> := <<"running">>}}, req(get, StUrl)),
        % Let it continue running and it should complete eventually
        JobPid2 ! continue,
        wait_state(StUrl, <<"completed">>),
        ?assertMatch({200, #{<<"ok">> := true}}, req(delete, JobUrl))
    end).


individual_job_stop_when_cluster_stopped(Top) ->
    ?_test(begin
        intercept_state(topoff1),
        Body = #{type => split, db => <<?DB1>>},
        {201, [#{?ID := Id}]} = req(post, Top ++ ?JOBS, Body),
        JobUrl = Top ++ ?JOBS ++ ?b2l(Id),
        StUrl =  JobUrl ++ "/state",
        % Wait for the the job to start running and intercept it in topoff1 state
        receive {JobPid, topoff1} -> ok end,
        % Tell the intercept to never finish checkpointing so job is left hanging
        % forever in running state
        JobPid ! cancel,
        ?assertMatch({200, #{<<"state">> := <<"running">>}}, req(get, StUrl)),
        % Stop resharding globally
        ?assertMatch({200, _}, req(put, Top ++ ?STATE, #{state => stopped})),
        wait_state(StUrl, <<"stopped">>),
        % Stop the job specifically
        {200, _} = req(put, StUrl, #{state => stopped}),
        % Job stays stopped
        ?assertMatch({200, #{<<"state">> := <<"stopped">>}}, req(get, StUrl)),
        % Set cluster to running again
        ?assertMatch({200, _}, req(put, Top ++ ?STATE, #{state => running})),
        % The job should stay stopped
        ?assertMatch({200, #{<<"state">> := <<"stopped">>}}, req(get, StUrl)),
        % The it should be possible to resume the job and it should complete
        ?assertMatch({200, _}, req(put, StUrl, #{state => running})),
        % Wait for the the job to start running and intercept it in topoff1 state
        receive {JobPid2, topoff1} -> ok end,
        ?assertMatch({200, #{<<"state">> := <<"running">>}}, req(get, StUrl)),
        % Let it continue running and it should complete eventually
        JobPid2 ! continue,
        wait_state(StUrl, <<"completed">>),
        ?assertMatch({200, #{<<"ok">> := true}}, req(delete, JobUrl))
    end).


intercept_state(State) ->
    TestPid = self(),
    meck:new(mem3_reshard_job, [passthrough]),
    meck:expect(mem3_reshard_job, checkpoint_done, fun(Job) ->
            case Job#job.split_state of
                State ->
                    TestPid ! {self(), State},
                    receive
                        continue -> meck:passthrough([Job]);
                        cancel -> ok
                    end;
                _ ->
                   meck:passthrough([Job])
            end
        end).


wait_state(Url, State) ->
    test_util:wait(fun() ->
            case req(get, Url) of
                {200, #{<<"state">> := State}} -> ok;
                {200, #{}} -> timer:sleep(250), wait
            end
    end).


create_db(Url) ->
    {ok, Status, _, _} = test_request:put(Url, [?JSON, ?AUTH], "{}"),
    ?assert(Status =:= 201 orelse Status =:= 202).


delete_db(Url) ->
    {ok, 200, _, _} = test_request:delete(Url, [?AUTH]).


req(Method, Url) ->
    Headers = [?AUTH],
    {ok, Code, _, Res} = test_request:request(Method, Url, Headers),
    {Code, jiffy:decode(Res, [return_maps])}.


req(Method, Url, #{} = Body) ->
    req(Method, Url, jiffy:encode(Body));

req(Method, Url, Body) ->
    Headers = [?JSON, ?AUTH],
    {ok, Code, _, Res} = test_request:request(Method, Url, Headers, Body),
    {Code, jiffy:decode(Res, [return_maps])}.
