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
-define(RESHARD, "_reshard/").
-define(RESHARD_JOBS, "_reshard/jobs/").


setup() ->
    Hashed = couch_passwords:hash_admin_password(?PASS),
    ok = config:set("admins", ?USER, ?b2l(Hashed), _Persist=false),
    ok = config:set("mem3_reshard", "store_state", "false", false),
    Addr = config:get("chttpd", "bind_address", "127.0.0.1"),
    Port = mochiweb_socket_server:get(chttpd, port),
    Url = lists:concat(["http://", Addr, ":", Port, "/"]),
    Db1Url = Url ++ ?DB1,
    create_db(Db1Url ++ "?q=1&n=1"),
    Url.


teardown(Url) ->
    mem3_reshard:reset_state(),
    delete_db(Url ++ ?DB1),
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
                    fun reshard_create_job_basic/1
                ]
            }
        }
    }.


reshard_basics(Url) ->
    ?_test(begin
        % GET /_reshard
        {C1, R1} = req(get, Url ++ ?RESHARD),
        ?assertEqual(200, C1),
        ?assertMatch(#{
            <<"state">> := <<"running">>,
            <<"state_reason">> := null,
            <<"completed">> := 0,
            <<"failed">> := 0,
            <<"running">> := 0,
            <<"stopped">> := 0,
            <<"total">> := 0
        }, R1),
        % GET _reshard/state
        {C2, R2} = req(get, Url ++ ?RESHARD ++ "/state"),
        ?assertEqual(200, C2),
        ?assertMatch(#{
            <<"state">> := <<"running">>,
            <<"reason">> := null
        }, R2),
        % GET _reshard/jobs
        {C3, R3} = req(get, Url ++ ?RESHARD_JOBS),
        ?assertEqual(200, C3),
        ?assertMatch(#{
            <<"jobs">> := [],
            <<"offset">> := 0,
            <<"total_rows">> := 0
        }, R3),
        % Some invalid paths and methods
        ?assertMatch({404, _}, req(get, Url ++ ?RESHARD ++ "/invalidpath")),
        ?assertMatch({405, _}, req(put, Url ++ ?RESHARD, #{dont => thinkso})),
        ?assertMatch({405, _}, req(post, Url ++ ?RESHARD, #{notgonna => happen}))
    end).


reshard_create_job_basic(Url) ->
    ?_test(begin
        % POST /_reshard/jobs
        Body = #{type => split, db => <<?DB1>>},
        {C1, R1} = req(post, Url ++ ?RESHARD_JOBS, Body),
        ?assertEqual(201, C1),
        ?assertMatch([#{<<"ok">> := true, <<"id">> := J, <<"shard">> :=  S}]
            when is_binary(J) andalso is_binary(S), R1),
        [#{<<"id">> := Id, <<"shard">> := Shard}] = R1,
        % GET /_reshard/jobs
        {C2, R2} = req(get, Url ++ ?RESHARD_JOBS),
        ?assertEqual(200, C2),
        ?assertMatch(#{
            <<"jobs">> := [#{<<"id">> := Id, <<"type">> := <<"split">>}],
            <<"offset">> := 0,
            <<"total_rows">> := 1
        }, R2),
        % GET /_reshard/job/$jobid
        {C3, R3} = req(get, Url ++ ?RESHARD_JOBS ++ ?b2l(Id)),
        ?assertEqual(200, C3),
        ThisNode = atom_to_binary(node(), utf8),
        ?assertMatch(#{<<"id">> := Id}, R3),
        ?assertMatch(#{<<"type">> := <<"split">>}, R3),
        ?assertMatch(#{<<"source">> := Shard}, R3),
        ?assertMatch(#{<<"history">> := History} when length(History) > 1, R3),
        ?assertMatch(#{<<"node">> := ThisNode}, R3),
        ?assertMatch(#{<<"split_state">> := SSt} when is_binary(SSt), R3),
        ?assertMatch(#{<<"job_state">> := JSt} when is_binary(JSt), R3),
        ?assertMatch(#{<<"state_info">> := #{}}, R3),
        ?assertMatch(#{<<"targets">> := Targets} when length(Targets) == 2, R3),
        % GET  /_reshard/job/$jobid/state
        {C4, R4} = req(get, Url ++ ?RESHARD_JOBS ++ ?b2l(Id) ++ "/state"),
        ?assertEqual(200, C4),
        ?assertMatch(#{<<"state">> := JSt} when is_binary(JSt), R4),
        ?assertMatch(#{<<"reason">> := Reason} when is_binary(Reason)
            orelse Reason =:= null, R4),
        % GET /_reshard
        {C5, R5} = req(get, Url ++ ?RESHARD),
        ?assertEqual(200, C5),
        ?assertMatch(#{
            <<"state">> := <<"running">>,
            <<"state_reason">> := null,
            <<"total">> := 1
        }, R5),
        % DELETE /_reshard/jobs/$jobid
        DelResult = req(delete, Url ++ ?RESHARD_JOBS ++ ?b2l(Id)),
        ?assertMatch({200, #{<<"ok">> := true}}, DelResult),
        % GET _reshard/jobs
        {C6, R6} = req(get, Url ++ ?RESHARD_JOBS),
        ?assertEqual(200, C6),
        ?assertMatch(#{
            <<"jobs">> := [],
            <<"offset">> := 0,
            <<"total_rows">> := 0
        }, R6)
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
