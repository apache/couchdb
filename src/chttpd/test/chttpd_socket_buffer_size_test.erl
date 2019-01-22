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

-module(chttpd_socket_buffer_size_test).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(USER, "chttpd_db_socket_buffer_size_test_admin").
-define(PASS, "pass").
-define(AUTH, {basic_auth, {?USER, ?PASS}}).
-define(CONTENT_JSON, {"Content-Type", "application/json"}).


setup() ->
    Hashed = couch_passwords:hash_admin_password(?PASS),
    ok = config:set("admins", ?USER, ?b2l(Hashed), _Persist=false),
    SocketOptions = config:get("chttpd", "socket_options"),
    Db = ?tempdb(),
    Addr = config:get("chttpd", "bind_address", "127.0.0.1"),
    Port = integer_to_list(mochiweb_socket_server:get(chttpd, port)),
    Url = "http://" ++ Addr ++ ":" ++ Port ++ "/" ++ ?b2l(Db),
    create_db(Url),
    {Db, SocketOptions}.


teardown({Db, SocketOptions}) ->
    delete_db(url(Db)),
    ok = config:delete("chttpd", "socket_options", _Persist=false),
    ok = config:delete("admins", ?USER, _Persist=false),
    case SocketOptions of
        undefined ->
            ok;
        _ ->
            ok = config:set("chttpd", "socket_options", SocketOptions)
    end.


socket_buffer_size_test_() ->
    {
        "chttpd socket_buffer_size_test",
        {
            setup,
            fun chttpd_test_util:start_couch/0,
            fun chttpd_test_util:stop_couch/1,
            {
                foreach,
                fun setup/0, fun teardown/1,
                [
                    fun buffer_too_small_url_fails/1,
                    fun buffer_too_small_header_fails/1,
                    fun recbuf_too_small_url_fails/1,
                    fun recbuf_too_small_header_fails/1,
                    fun default_buffer_settings_work/1
                ]
            }
        }
    }.


buffer_too_small_url_fails({Db, _}) ->
    ?_test(begin
        restart_chttpd("[{buffer, 1024}]"),
        Id = data(1500),
        Status1 = put_req(url(Db) ++ "/" ++ Id, "{}"),
        ?assertEqual(400, Status1),
        restart_chttpd("[{buffer, 2048}]"),
        Status2 = put_req(url(Db) ++ "/" ++ Id, "{}"),
        ?assert(Status2 =:= 201 orelse Status2 =:= 202)
    end).


buffer_too_small_header_fails({Db, _}) ->
    ?_test(begin
        restart_chttpd("[{buffer, 1024}]"),
        Headers = [{"Blah", data(1500)}],
        Status1 = put_req(url(Db) ++ "/d", Headers, "{}"),
        ?assertEqual(400, Status1),
        restart_chttpd("[{buffer, 2048}]"),
        Status2 = put_req(url(Db) ++ "/d", Headers, "{}"),
        ?assert(Status2 =:= 201 orelse Status2 =:= 202)
    end).


recbuf_too_small_url_fails({Db, _}) ->
    ?_test(begin
        restart_chttpd("[{recbuf, 1024}]"),
        Id = data(1500),
        Status1 = put_req(url(Db) ++ "/" ++ Id, "{}"),
        ?assertEqual(400, Status1),
        restart_chttpd("[{recbuf, 2048}]"),
        Status2 = put_req(url(Db) ++ "/" ++ Id, "{}"),
        ?assert(Status2 =:= 201 orelse Status2 =:= 202)
    end).


recbuf_too_small_header_fails({Db, _}) ->
    ?_test(begin
        restart_chttpd("[{recbuf, 1024}]"),
        Headers = [{"Blah", data(1500)}],
        Status1 = put_req(url(Db) ++ "/d", Headers, "{}"),
        ?assertEqual(400, Status1),
        restart_chttpd("[{recbuf, 2048}]"),
        Status2 = put_req(url(Db) ++ "/d", Headers, "{}"),
        ?assert(Status2 =:= 201 orelse Status2 =:= 202)
    end).


default_buffer_settings_work({Db, _}) ->
    ?_test(begin
        restart_chttpd("[{recbuf, undefined}]"),
        Id = data(7000),
        Status = put_req(url(Db) ++ "/" ++ Id, "{}"),
        ?assert(Status =:= 201 orelse Status =:= 202)
    end).


% Helper functions

url(Db) ->
    Addr = config:get("chttpd", "bind_address", "127.0.0.1"),
    Port = integer_to_list(mochiweb_socket_server:get(chttpd, port)),
    "http://" ++ Addr ++ ":" ++ Port ++ "/" ++ ?b2l(Db).


create_db(Url) ->
    Status = put_req(Url ++ "?q=1&n=1", "{}"),
    ?assert(Status =:= 201 orelse Status =:= 202).


delete_db(Url) ->
    {ok, 200, _, _} = test_request:delete(Url, [?AUTH]).


put_req(Url, Body) ->
    put_req(Url, [], Body).


put_req(Url, Headers, Body) ->
    AllHeaders = Headers ++ [?CONTENT_JSON, ?AUTH],
    {ok, Status, _, _} = test_request:put(Url, AllHeaders, Body),
    Status.


data(Size) ->
    string:copies("x", Size).


restart_chttpd(ServerOptions) ->
    ok = application:stop(chttpd),
    ok = application:stop(mochiweb),
    config:set("chttpd", "server_options", ServerOptions, _Persist=false),
    ok = application:start(mochiweb),
    ok = application:start(chttpd).
