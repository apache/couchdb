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

-module(couchdb_mrview_cors_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").



-define(DDOC, {[
    {<<"_id">>, <<"_design/foo">>},
    {<<"shows">>, {[
        {<<"bar">>, <<"function(doc, req) {return '<h1>wosh</h1>';}">>}
    ]}}
]}).

start() ->
    ok = test_util:start_couch([chttpd]),
    ok = config:set("httpd", "enable_cors", "true", false),
    ok = config:set("vhosts", "example.com", "/", false),
    ok.

setup(PortType) ->
    DbName = ?tempdb(),
    ok = create_db(PortType, DbName),

    config:set("cors", "credentials", "false", false),
    config:set("cors", "origins", "http://example.com", false),

    Addr = config:get("httpd", "bind_address", "127.0.0.1"),
    Host = "http://" ++ Addr ++ ":" ++ port(PortType),
    upload_ddoc(Host, ?b2l(DbName)),
    {Host, ?b2l(DbName)}.

teardown(PortType, {_Host, DbName}) ->
    delete_db(PortType, ?l2b(DbName)),
    ok.

cors_test_() ->
    {
        "CORS for mrview",
        {
            setup,
            fun start/0, fun test_util:stop_couch/1,
            [show_tests()]
        }
    }.

show_tests() ->
    {
        "Check CORS for show",
        [
            make_test_case(clustered, [fun should_make_shows_request/2]),
            make_test_case(backdoor, [fun should_make_shows_request/2])
        ]
    }.

make_test_case(Mod, Funs) ->
    {
        lists:flatten(io_lib:format("~s", [Mod])),
        {foreachx, fun setup/1, fun teardown/2, [{Mod, Fun} || Fun <- Funs]}
    }.

should_make_shows_request(_, {Host, DbName}) ->
    ?_test(begin
         ReqUrl = Host ++ "/" ++ DbName ++ "/_design/foo/_show/bar",
         Headers = [{"Origin", "http://example.com"},
                    {"Access-Control-Request-Method", "GET"}],
         {ok, _, Resp, Body} = test_request:get(ReqUrl, Headers),
         Origin = proplists:get_value("Access-Control-Allow-Origin", Resp),
         ?assertEqual("http://example.com", Origin),
         ?assertEqual(<<"<h1>wosh</h1>">>, Body)
    end).

create_db(backdoor, DbName) ->
    {ok, Db} = couch_db:create(DbName, [?ADMIN_CTX]),
    couch_db:close(Db);
create_db(clustered, DbName) ->
    ok = fabric:create_db(DbName, [?ADMIN_CTX]).

delete_db(backdoor, DbName) ->
    couch_server:delete(DbName, [?ADMIN_CTX]);
delete_db(clustered, DbName) ->
    ok = fabric:delete_db(DbName, [?ADMIN_CTX]).



port(clustered) ->
    integer_to_list(mochiweb_socket_server:get(chttpd, port));
port(backdoor) ->
    integer_to_list(mochiweb_socket_server:get(couch_httpd, port)).


upload_ddoc(Host, DbName) ->
    Url = Host ++ "/" ++ DbName ++ "/_design/foo",
    Body = couch_util:json_encode(?DDOC),
    {ok, 201, _Resp, _Body} = test_request:put(Url, Body),
    ok.
