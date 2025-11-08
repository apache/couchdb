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

-module(fabric_timeout_test).

-include_lib("couch/include/couch_db.hrl").
-include_lib("couch/include/couch_eunit.hrl").

-define(ADM, "adm").
-define(PASS, "pass").
-define(AUTH, {basic_auth, {?ADM, ?PASS}}).
-define(JSON, {"Content-Type", "application/json"}).
-define(TIMEOUT_RESP, #{
    <<"error">> => <<"timeout">>,
    <<"reason">> => <<"The request could not be processed in a reasonable amount of time.">>
}).

timeout_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        with([
            ?TDEF(t_find_no_index)
        ])
    }.

setup() ->
    Ctx = test_util:start_couch([chttpd, fabric]),
    DbName = ?tempdb(),
    ok = fabric:create_db(DbName, []),
    ok = batch_insert_docs(DbName, 100, 100),
    ok = stagger_timeouts(),
    HashedPass = couch_passwords:hash_admin_password(?PASS),
    ok = config:set("admins", ?ADM, ?b2l(HashedPass), _Persist = false),
    {Ctx, DbName}.

teardown({Ctx, DbName}) ->
    ok = fabric:delete_db(DbName),
    ok = config:delete("admins", ?ADM, _Persist = false),
    ok = test_util:stop_couch(Ctx).

t_find_no_index({_, DbName}) ->
    Body = #{
        selector => #{x => #{'$regex' => <<"^(((a+)+)+)+$">>}},
        execution_stats => true
    },
    {MicroSec, {Code, Resp}} = timer:tc(fun() ->
        req(post, url(DbName) ++ "/_find", [], Body)
    end),
    ?assertEqual(500, Code),
    Expect = ?TIMEOUT_RESP,
    ?assertMatch(Expect, Resp),

    Duration = MicroSec/1000,
    ?debugVal(Duration),
    ExpectLo = config:get_integer("fabric", "view_timeout", 0),
    ExpectHi = ExpectLo * 1.5,
    ?assert(ExpectLo < Duration andalso Duration < ExpectHi),
    ok.

batch_insert_docs(DbName, BatchSize, BatchCount) ->
    lists:foreach(
        fun(Batch) ->
            Docs = [doc(I, Batch) || I <- lists:seq(1, BatchSize)],
            {ok, _} = fabric:update_docs(DbName, Docs, [])
        end,
        lists:seq(1, BatchCount)
    ).

doc(I, Batch) ->
    #doc{
         id = couch_uuids:random(),
         body = {[
             {<<"i">>, I},
             {<<"b">>, Batch},
             {<<"x">>, <<"aaaaaaaaaaab">>}
         ]}
    }.

stagger_timeouts() ->
    lists:foreach(
        fun({Section, Key, Val}) ->
            ok = config:set(Section, Key, Val, false)
        end,
        [
            {"fabric", "all_docs_timeout", "100"},
            {"fabric", "request_timeout", "200"},
            {"fabric", "search_timeout", "400"},
            {"fabric", "view_timeout", "500"}
       ]
   ).

req(Method, Url, Headers, #{} = Body) ->
    {ok, Code, _, Resp} = test_request:request(
        Method,
        Url,
        Headers ++ [?AUTH, ?JSON],
        jiffy:encode(Body)
    ),
    {Code, jiffy:decode(Resp, [return_maps])}.

url(DbName) when is_binary(DbName) ->
    base_url() ++ "/" ++ binary_to_list(DbName).

base_url() ->
    Addr = config:get("chttpd", "bind_address", "127.0.0.1"),
    Port = mochiweb_socket_server:get(chttpd, port),
    lists:concat(["http://", Addr, ":", Port]).
