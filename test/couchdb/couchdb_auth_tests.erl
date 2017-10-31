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

-module(couchdb_auth_tests).

-include("couch_eunit.hrl").
-include_lib("couchdb/couch_db.hrl").


start() ->
    couch_server_sup:start_link(?CONFIG_CHAIN),
    ok.

stop(_) ->
    couch_server_sup:stop(),
    ok.

setup() ->
    Addr = couch_config:get("httpd", "bind_address", "127.0.0.1"),
    Port = integer_to_list(mochiweb_socket_server:get(couch_httpd, port)),
    lists:concat(["http://", Addr, ":", Port, "/_session"]).

teardown(_) ->
    ok.


auth_test_() ->
    {
        "Auth tests",
        {
            setup,
            fun start/0, fun stop/1,
            {
                foreach,
                fun setup/0, fun teardown/1,
                [
                    fun should_not_return_username_on_post_to_session/1
                ]
            }
        }
    }.


should_not_return_username_on_post_to_session(Url) ->
    ?_assertEqual(<<"rocko">>,
        begin
            ok = couch_config:set("admins", "rocko", "artischocko", false),
            {ok, _, _, Body} = test_request:post(Url,
                [{"Content-Type", "application/json"}],
                "{\"name\":\"rocko\", \"password\":\"artischocko\"}"),
            {Json} = ?JSON_DECODE(Body),
            couch_util:get_value(<<"name">>, Json)
        end).
