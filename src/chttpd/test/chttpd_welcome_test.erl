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

-module(chttpd_welcome_test).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(USER, "chttpd_db_test_admin").
-define(PASS, "pass").
-define(AUTH, {basic_auth, {?USER, ?PASS}}).
-define(CONTENT_JSON, {"Content-Type", "application/json"}).


setup() ->
    ok = config:set("admins", ?USER, ?PASS, _Persist=false),
    Addr = config:get("chttpd", "bind_address", "127.0.0.1"),
    Port = mochiweb_socket_server:get(chttpd, port),
    Url = lists:concat(["http://", Addr, ":", Port, "/"]),
    Url.


teardown(_Url) ->
    ok = config:delete("admins", ?USER, _Persist=false).


welcome_test_() ->
    {
        "chttpd welcome endpoint tests",
        {
            setup,
            fun chttpd_test_util:start_couch/0, fun chttpd_test_util:stop_couch/1,
            {
                foreach,
                fun setup/0, fun teardown/1,
                [
                    fun should_have_version/1,
                    fun should_have_features/1
                ]
            }
        }
    }.


should_have_version(Url) ->
    ?_test(begin
        {ok, Status, _, Body} = test_request:get(Url, [?CONTENT_JSON, ?AUTH]),
        ?assertEqual(200, Status),
        {Json} = ?JSON_DECODE(Body),
        Version = couch_util:get_value(<<"version">>, Json, undefined),
        CouchDB = couch_util:get_value(<<"couchdb">>, Json, undefined),
        Features = couch_util:get_value(<<"features">>, Json, undefined),
        ?assertEqual(<<"Welcome">>, CouchDB),
        RealVersion = list_to_binary(couch_server:get_version()),
        ?assertEqual(RealVersion, Version),
        ?assert(is_list(Features))
    end).


should_have_features(Url) ->
    ?_test(begin
        config:enable_feature(snek),
        {ok, 200, _, Body1} = test_request:get(Url, [?CONTENT_JSON, ?AUTH]),
        {Json1} = ?JSON_DECODE(Body1),
        Features1 = couch_util:get_value(<<"features">>, Json1, undefined),
        ?assert(is_list(Features1)),
        ?assert(lists:member(<<"snek">>, Features1)),
        config:disable_feature(snek),
        {ok, 200, _, Body2} = test_request:get(Url, [?CONTENT_JSON, ?AUTH]),
        {Json2} = ?JSON_DECODE(Body2),
        Features2 = couch_util:get_value(<<"features">>, Json2, undefined),
        ?assert(is_list(Features2)),
        ?assertNot(lists:member(<<"snek">>, Features2))
    end).
