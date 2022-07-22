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

-include_lib("couch/include/couch_eunit.hrl").

setup(PortType) ->
    Hashed = couch_passwords:hash_admin_password("artischocko"),
    ok = config:set("admins", "rocko", binary_to_list(Hashed), _Persist = false),
    Addr = config:get("httpd", "bind_address", "127.0.0.1"),
    lists:concat(["http://", Addr, ":", port(PortType), "/_session"]).

setup_require_valid_user(PortType) ->
    ok = config:set("chttpd", "require_valid_user", "true", _Persist = false),
    setup(PortType).

teardown(_, _) ->
    ok.

teardown_require_valid_user(_, _) ->
    config:set("chttpd", "require_valid_user", "false", _Persist = false).

auth_test_() ->
    Tests = [
        fun should_return_username_on_post_to_session/2,
        fun should_not_return_authenticated_field/2,
        fun should_return_list_of_handlers/2
    ],
    RequireValidUserTests = [
        % See #1947 - this should work even with require_valid_user
        fun should_return_username_on_post_to_session/2
    ],
    {
        "Auth tests",
        {
            setup,
            fun() -> test_util:start_couch([chttpd]) end,
            fun test_util:stop_couch/1,
            [
                make_test_cases(clustered, Tests),
                make_test_cases(backdoor, Tests),
                make_require_valid_user_test_cases(clustered, RequireValidUserTests)
            ]
        }
    }.

make_test_cases(Mod, Funs) ->
    {
        lists:flatten(io_lib:format("~s", [Mod])),
        {foreachx, fun setup/1, fun teardown/2, [{Mod, Fun} || Fun <- Funs]}
    }.

make_require_valid_user_test_cases(Mod, Funs) ->
    {
        lists:flatten(io_lib:format("~s require_valid_user=true", [Mod])),
        {foreachx, fun setup_require_valid_user/1, fun teardown_require_valid_user/2, [
            {Mod, Fun}
         || Fun <- Funs
        ]}
    }.

should_return_username_on_post_to_session(_PortType, Url) ->
    ?_assertEqual(
        <<"rocko">>,
        begin
            Hashed = couch_passwords:hash_admin_password(<<"artischocko">>),
            ok = config:set("admins", "rocko", binary_to_list(Hashed), false),
            {ok, _, _, Body} = test_request:post(
                Url,
                [{"Content-Type", "application/json"}],
                "{\"name\":\"rocko\", \"password\":\"artischocko\"}"
            ),
            {Json} = jiffy:decode(Body),
            proplists:get_value(<<"name">>, Json)
        end
    ).

should_not_return_authenticated_field(_PortType, Url) ->
    ?_assertThrow(
        {not_found, _},
        begin
            couch_util:get_nested_json_value(session(Url), [
                <<"info">>, <<"authenticated">>
            ])
        end
    ).

should_return_list_of_handlers(backdoor, Url) ->
    ?_assertEqual(
        [<<"cookie">>, <<"default">>],
        begin
            couch_util:get_nested_json_value(session(Url), [
                <<"info">>, <<"authentication_handlers">>
            ])
        end
    );
should_return_list_of_handlers(clustered, Url) ->
    ?_assertEqual(
        [<<"cookie">>, <<"default">>],
        begin
            couch_util:get_nested_json_value(session(Url), [
                <<"info">>, <<"authentication_handlers">>
            ])
        end
    ).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

session(Url) ->
    {ok, _, _, Body} = test_request:get(
        Url,
        [{"Content-Type", "application/json"}],
        "{\"name\":\"rocko\", \"password\":\"artischocko\"}"
    ),
    jiffy:decode(Body).

port(clustered) ->
    integer_to_list(mochiweb_socket_server:get(chttpd, port));
port(backdoor) ->
    integer_to_list(mochiweb_socket_server:get(couch_httpd, port)).
