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

-module(chttpd_invalid_path_test).

-include_lib("couch/include/couch_eunit.hrl").

setup_fe() ->
    meck:expect(couch_stats, increment_counter, 1, ok),
    Addr = config:get("chttpd", "bind_address", "127.0.0.1"),
    Port = mochiweb_socket_server:get(chttpd, port),
    {Addr, Port}.

teardown_fe(_) ->
    meck:unload().

invalid_path_test_() ->
    {
        "chttpd invalid path test",
        {
            setup,
            fun chttpd_test_util:start_couch/0,
            fun chttpd_test_util:stop_couch/1,
            {
                foreach,
                fun setup_fe/0,
                fun teardown_fe/1,
                [
                    ?TDEF_FE(shutdown_on_invalid_path)
                ]
            }
        }
    }.

shutdown_on_invalid_path({Addr, Port}) ->
    {ok, Socket} = gen_tcp:connect(Addr, Port, [binary, {active, true}]),
    ok = gen_tcp:send(Socket, "GET badpath HTTP/1.0\n\n"),
    ok = gen_tcp:close(Socket),
    ok = meck:wait(couch_stats, increment_counter, '_', _TimeoutMillisec = 1000),
    ExpectArgs = [[couchdb, httpd, aborted_requests]],
    ?assert(meck:called(couch_stats, increment_counter, ExpectArgs)).
