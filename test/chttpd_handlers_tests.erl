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

-module(chttpd_handlers_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").


setup() ->
    Addr = config:get("chttpd", "bind_address", "127.0.0.1"),
    Port = mochiweb_socket_server:get(chttpd, port),
    BaseUrl = lists:concat(["http://", Addr, ":", Port]),
    BaseUrl.

teardown(_Url) ->
    ok.


replicate_test_() ->
    {
        "_replicate",
        {
            setup,
            fun chttpd_test_util:start_couch/0,
            fun chttpd_test_util:stop_couch/1,
            {
                foreach,
                fun setup/0, fun teardown/1,
                [
                    fun should_escape_dbname_on_replicate/1
                ]
            }
        }
    }.


should_escape_dbname_on_replicate(Url) ->
    ?_test(
        begin
            Request = couch_util:json_encode({[
                {<<"source">>, <<"foo/bar">>},
                {<<"target">>, <<"bar/baz">>},
                {<<"create_target">>, true}
            ]}),
            {ok, 200, _, Body} = request_replicate(Url ++ "/_replicate", Request),
            JSON = couch_util:json_decode(Body),

            Source = json_value(JSON, [<<"source">>, <<"url">>]),
            Target = json_value(JSON, [<<"target">>, <<"url">>]),
            UrlBin = ?l2b(Url),
            ?assertEqual(<<UrlBin/binary, "/foo%2Fbar">>, Source),
            ?assertEqual(<<UrlBin/binary, "/bar%2Fbaz">>, Target)
        end).


json_value(JSON, Keys) ->
    couch_util:get_nested_json_value(JSON, Keys).

request_replicate(Url, Body) ->
    Headers = [{"Content-Type", "application/json"}],
    Handler = {chttpd_misc, handle_replicate_req},
    request(post, Url, Headers, Body, Handler, fun(Req) ->
        chttpd:send_json(Req, 200, get(post_body))
    end).

request(Method, Url, Headers, Body, {M, F}, MockFun) ->
    meck:new(M, [passthrough, non_strict]),
    try
        meck:expect(M, F, MockFun),
        Result = test_request:Method(Url, Headers, Body),
        ?assert(meck:validate(M)),
        Result
    catch Kind:Reason ->
        {Kind, Reason}
    after
        meck:unload(M)
    end.
