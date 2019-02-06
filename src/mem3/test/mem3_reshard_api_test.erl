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
    Url.


teardown(Url) ->
    mem3_reshard:reset_state(),
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
                    fun reshard_basics/1,
                    fun reshard_create_job_basic/1,
                    fun reshard_create_two_jobs/1,
                    fun reshard_start_stop_cluster_basic/1,
                    fun reshard_start_stop_cluster_with_a_job/1
                ]
            }
        }
    }.


reshard_basics(Top) ->
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


reshard_create_job_basic(Top) ->
    ?_test(begin
        % POST /_reshard/jobs
        {C1, R1} = req(post, Top ++ ?JOBS,  #{type => split, db => <<?DB1>>}),
        ?assertEqual(201, C1),
        ?assertMatch([#{<<"ok">> := true, <<"id">> := J, <<"shard">> :=  S}]
            when is_binary(J) andalso is_binary(S), R1),
        [#{<<"id">> := Id, <<"shard">> := Shard}] = R1,
        % GET /_reshard/jobs
        ?assertMatch({200, #{
            <<"jobs">> := [#{<<"id">> := Id, <<"type">> := <<"split">>}],
            <<"offset">> := 0,
            <<"total_rows">> := 1
        }}, req(get, Top ++ ?JOBS)),
        % GET /_reshard/job/$jobid
        {C2, R2} = req(get, Top ++ ?JOBS ++ ?b2l(Id)),
        ?assertEqual(200, C2),
        ThisNode = atom_to_binary(node(), utf8),
        ?assertMatch(#{<<"id">> := Id}, R2),
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


reshard_create_two_jobs(Top) ->
    ?_test(begin
        ?assertMatch({201, [#{<<"ok">> := true}]},
            req(post, Top ++ ?JOBS, #{type => split, db => <<?DB1>>})),
        ?assertMatch({201, [#{<<"ok">> := true}]},
            req(post, Top ++ ?JOBS, #{type => split, db => <<?DB2>>})),
        ?assertMatch({200, #{
            <<"jobs">> := [#{<<"id">> := Id1}, #{<<"id">> := Id2}],
            <<"offset">> := 0,
            <<"total_rows">> := 2
        }} when Id1 =/= Id2, req(get, Top ++ ?JOBS)),
        ?assertMatch({200, #{<<"total">> := 2}}, req(get, Top ++ ?RESHARD)),
        {200, #{<<"jobs">> := [#{<<"id">> := Id1}, #{<<"id">> := Id2}]}} =
            req(get, Top ++ ?JOBS),
        {200, #{<<"ok">> := true}} = req(delete, Top ++ ?JOBS ++ ?b2l(Id1)),
        ?assertMatch({200, #{<<"total">> := 1}}, req(get, Top ++ ?RESHARD)),
        {200, #{<<"ok">> := true}} = req(delete, Top ++ ?JOBS ++ ?b2l(Id2)),
        ?assertMatch({200, #{<<"total">> := 0}}, req(get, Top ++ ?RESHARD))
    end).


reshard_start_stop_cluster_basic(Top) ->
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


reshard_start_stop_cluster_with_a_job(Top) ->
    ?_test(begin
        Url = Top ++ ?STATE,
        ?assertMatch({200, _}, req(put, Url, #{state => stopped})),
        ?assertMatch({200, #{<<"state">> := <<"stopped">>}}, req(get, Url)),
        % Can add jobs with global state stopped, they just won't be running
        {201, R1} = req(post, Top ++ ?JOBS, #{type => split, db => <<?DB1>>}),
        ?assertMatch([#{<<"ok">> := true}], R1),
        [#{<<"id">> := Id1}] = R1,
        {200, J1} = req(get, Top ++ ?JOBS ++ ?b2l(Id1)),
        ?assertMatch(#{<<"id">> := Id1, <<"job_state">> := <<"stopped">>}, J1),
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
        {201, [#{<<"id">> := Id2}]} = req(post, Top ++ ?JOBS, #{type => split,
            db => <<?DB1>>}),
        ?assertMatch({200, #{<<"id">> := Id2, <<"job_state">> := <<"stopped">>}},
            req(get, Top ++ ?JOBS ++ ?b2l(Id2))),
        % Job should start after resharding is started on the cluster
        ?assertMatch({200, _}, req(put, Url, #{state => running})),
        ?assertMatch({200, #{<<"id">> := Id2, <<"job_state">> := JSt}}
            when JSt =/= <<"stopped">>, req(get, Top ++ ?JOBS ++ ?b2l(Id2))),
        {200, #{<<"ok">> := true}} = req(delete, Top ++ ?JOBS ++ ?b2l(Id2))
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
