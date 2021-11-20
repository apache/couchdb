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

-module(chttpd_external_test).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

setup_mock() ->
    ok = meck:new([config, couch], [passthrough]),
    ok = meck:expect(couch_db, is_clustered, 1, false),
    ok = meck:expect(couch_db, get_db_info, 1, {ok, [{name, <<"fake">>}]}),
    ok = meck:expect(couch_db, name, 1, <<"fake">>),
    ok = meck:expect(couch_db, get_user_ctx, 1, #user_ctx{}),
    ok = meck:expect(couch_db, get_security, 1, []),
    ok = meck:expect(couch_uuids, new, 0, <<"4">>),
    ok = meck:expect(config, get_integer, fun(_, _, N) -> N end).

teardown_mock(_) ->
    meck:unload().

setup_local_httpd_req() ->
    ok = meck:new(mochiweb, [passthrough]),
    ok = meck:expect(mochiweb_socket, peername, fun(_) ->
        {ok, {{127, 0, 0, 1}, 5984}}
    end),
    ok = meck:expect(mochiweb_request, recv_body, 2, {[{<<"a">>, 42}]}),
    Headers = mochiweb_headers:make([{"host", "example.com"}]),
    MochiReq = mochiweb_request:new(nil, 'GET', "/", {1, 1}, Headers),
    #httpd{
        mochi_req = MochiReq,
        method = 'GET',
        path_parts = [<<"/">>],
        requested_path_parts = [<<"/">>],
        user_ctx = #user_ctx{}
    }.

setup_remote_httpd_req() ->
    Headers = mochiweb_headers:make([{"host", "example.com"}]),
    MochiReq = mochiweb_request:new(nil, 'GET', "/", {1, 1}, Headers),
    #httpd{
        mochi_req = MochiReq,
        method = 'GET',
        path_parts = [<<"/">>],
        requested_path_parts = [<<"/">>],
        peer = "127.0.0.1",
        req_body = {[{<<"a">>, 42}]},
        user_ctx = #user_ctx{}
    }.

json_req_obj_local_httpd_req_test_() ->
    {
        "chttpd external local httpd_req tests",
        {
            setup,
            fun setup_mock/0,
            fun teardown_mock/1,
            {
                setup,
                fun setup_local_httpd_req/0,
                fun should_convert_req_to_json_obj/1
            }
        }
    }.

json_req_obj_remote_httpd_req_test_() ->
    {
        "chttpd external remote httpd_req tests",
        {
            setup,
            fun setup_mock/0,
            fun teardown_mock/1,
            {
                setup,
                fun setup_remote_httpd_req/0,
                fun should_convert_req_to_json_obj/1
            }
        }
    }.

should_convert_req_to_json_obj(HttpdReq) ->
    Expect = expect(),
    {Result} = chttpd_external:json_req_obj(HttpdReq, <<"fake">>),
    lists:map(
        fun({K, V}) ->
            {K, ?_assertEqual(couch_util:get_value(K, Expect), V)}
        end,
        Result
    ).

expect() ->
    [
        {<<"info">>, {[{name, <<"fake">>}]}},
        {<<"uuid">>, <<"4">>},
        {<<"id">>, null},
        {<<"method">>, 'GET'},
        {<<"requested_path">>, [<<"/">>]},
        {<<"path">>, [<<"/">>]},
        {<<"raw_path">>, <<"/">>},
        {<<"query">>, {[]}},
        {<<"headers">>, {[{<<"host">>, <<"example.com">>}]}},
        {<<"body">>, {[{<<"a">>, 42}]}},
        {<<"peer">>, <<"127.0.0.1">>},
        {<<"form">>, {[]}},
        {<<"cookie">>, {[]}},
        {<<"userCtx">>,
            {[
                {<<"db">>, <<"fake">>},
                {<<"name">>, null},
                {<<"roles">>, []}
            ]}},
        {<<"secObj">>, []}
    ].
