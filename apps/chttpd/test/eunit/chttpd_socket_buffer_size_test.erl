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

setup(SocketOpts) ->
    StartCtx = start_couch_with_cfg(SocketOpts),
    Db = ?tempdb(),
    create_db(url(Db)),
    {StartCtx, Db}.

teardown(_, {StartCtx, Db}) ->
    delete_db(url(Db)),
    ok = config:delete("admins", ?USER, _Persist = false),
    test_util:stop_couch(StartCtx).

socket_buffer_size_test_() ->
    {
        "chttpd socket_buffer_size_test",
        {
            foreachx,
            fun setup/1,
            fun teardown/2,
            [
                {"[{recbuf, undefined}]", fun default_buffer/2},
                {"[{recbuf, 1024}]", fun small_recbuf/2},
                {"[{buffer, 1024}]", fun small_buffer/2}
            ]
        }
    }.

small_recbuf(_, {_, Db}) ->
    {timeout, 30,
        ?_test(begin
            Id = data(2048),
            Response = put_req(url(Db) ++ "/" ++ Id, "{}"),
            ?assert(Response =:= 400 orelse Response =:= request_failed)
        end)}.

small_buffer(_, {_, Db}) ->
    {timeout, 30,
        ?_test(begin
            Id = data(2048),
            Response = put_req(url(Db) ++ "/" ++ Id, "{}"),
            ?assert(Response =:= 400 orelse Response =:= request_failed)
        end)}.

default_buffer(_, {_, Db}) ->
    {timeout, 30,
        ?_test(begin
            Id = data(7000),
            Headers = [{"Blah", data(7000)}],
            Status = put_req(url(Db) ++ "/" ++ Id, Headers, "{}"),
            ?assert(Status =:= 201 orelse Status =:= 202)
        end)}.

% Helper functions

url() ->
    Addr = config:get("chttpd", "bind_address", "127.0.0.1"),
    Port = integer_to_list(mochiweb_socket_server:get(chttpd, port)),
    "http://" ++ Addr ++ ":" ++ Port.

url(Db) ->
    url() ++ "/" ++ ?b2l(Db).

create_db(Url) ->
    Status = put_req(Url ++ "?q=1&n=1", "{}"),
    ?assert(Status =:= 201 orelse Status =:= 202).

delete_db(Url) ->
    {ok, 200, _, _} = test_request:delete(Url, [?AUTH]).

put_req(Url, Body) ->
    put_req(Url, [], Body).

put_req(Url, Headers, Body) ->
    AllHeaders = Headers ++ [?CONTENT_JSON, ?AUTH],
    case test_request:put(Url, AllHeaders, Body) of
        {ok, Status, _, _} -> Status;
        {error, Error} -> Error
    end.

data(Size) ->
    string:copies("x", Size).

append_to_cfg_chain(Cfg) ->
    CfgDir = filename:dirname(lists:last(?CONFIG_CHAIN)),
    CfgFile = filename:join([CfgDir, "chttpd_socket_buffer_extra_cfg.ini"]),
    CfgSect = io_lib:format("[chttpd]~nserver_options = ~s~n", [Cfg]),
    ok = file:write_file(CfgFile, CfgSect),
    ?CONFIG_CHAIN ++ [CfgFile].

start_couch_with_cfg(Cfg) ->
    CfgChain = append_to_cfg_chain(Cfg),
    StartCtx = test_util:start_couch(CfgChain, [chttpd]),
    Hashed = couch_passwords:hash_admin_password(?PASS),
    ok = config:set("admins", ?USER, ?b2l(Hashed), _Persist = false),
    StartCtx.
