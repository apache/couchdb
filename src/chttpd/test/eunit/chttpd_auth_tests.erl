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

-module(chttpd_auth_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").


setup() ->
    Addr = config:get("chttpd", "bind_address", "127.0.0.1"),
    Port = mochiweb_socket_server:get(chttpd, port),
    BaseUrl = lists:concat(["http://", Addr, ":", Port]),
    BaseUrl.

teardown(_Url) ->
    ok.


require_valid_user_exception_test_() ->
    {
        "_up",
        {
            setup,
            fun chttpd_test_util:start_couch/0,
            fun chttpd_test_util:stop_couch/1,
            {
                foreach,
                fun setup/0, fun teardown/1,
                [
                    fun should_handle_require_valid_user_except_up_on_up_route/1,
                    fun should_handle_require_valid_user_except_up_on_non_up_routes/1
                ]
            }
        }
    }.

set_require_user_false() ->
  ok = config:set("chttpd", "require_valid_user", "false", _Persist=false).

set_require_user_true() ->
  ok = config:set("chttpd", "require_valid_user", "true", _Persist=false).

set_require_user_except_for_up_false() ->
  ok = config:set("chttpd", "require_valid_user_except_for_up", "false", _Persist=false).

set_require_user_except_for_up_true() ->
  ok = config:set("chttpd", "require_valid_user_except_for_up", "true", _Persist=false).

should_handle_require_valid_user_except_up_on_up_route(_Url) ->
  ?_test(begin
    %    require_valid_user |   require_valid_user_except_up | up needs auth
    % 1  F                  |   F                            | F
    % 2  F                  |   T                            | F
    % 3  T                  |   F                            | T
    % 4  T                  |   T                            | F

    UpRequest = #httpd{path_parts=[<<"_up">>]},
    % we use ?ADMIN_USER here because these tests run under admin party
    % so this is equivalent to an unauthenticated request
    ExpectAuth = {unauthorized, <<"Authentication required.">>},
    ExpectNoAuth = #httpd{user_ctx=?ADMIN_USER,path_parts=[<<"_up">>]},

    % 1
    set_require_user_false(),
    set_require_user_except_for_up_false(),
    Result1 = chttpd_auth:party_mode_handler(UpRequest),
    ?assertEqual(ExpectNoAuth, Result1),

    % 2
    set_require_user_false(),
    set_require_user_except_for_up_true(),
    Result2 = chttpd_auth:party_mode_handler(UpRequest),
    ?assertEqual(ExpectNoAuth, Result2),

    % 3
    set_require_user_true(),
    set_require_user_except_for_up_false(),
    ?assertThrow(ExpectAuth, chttpd_auth:party_mode_handler(UpRequest)),

    % 4
    set_require_user_true(),
    set_require_user_except_for_up_true(),
    Result4 = chttpd_auth:party_mode_handler(UpRequest),
    ?assertEqual(ExpectNoAuth, Result4)

  end).

should_handle_require_valid_user_except_up_on_non_up_routes(_Url) ->
  ?_test(begin
    %    require_valid_user |  require_valid_user_except_up | everything not _up requires auth
    % 5  F                  |  F                            | F
    % 6  F                  |  T                            | T
    % 7  T                  |  F                            | T
    % 8  T                  |  T                            | T

    NonUpRequest = #httpd{path_parts=[<<"/">>]},
    ExpectAuth = {unauthorized, <<"Authentication required.">>},
    ExpectNoAuth = #httpd{user_ctx=?ADMIN_USER,path_parts=[<<"/">>]},
    % 5
    set_require_user_false(),
    set_require_user_except_for_up_false(),
    Result5 = chttpd_auth:party_mode_handler(NonUpRequest),
    ?assertEqual(ExpectNoAuth, Result5),

    % 6
    set_require_user_false(),
    set_require_user_except_for_up_true(),
    ?assertThrow(ExpectAuth, chttpd_auth:party_mode_handler(NonUpRequest)),

    % 7
    set_require_user_true(),
    set_require_user_except_for_up_false(),
    ?assertThrow(ExpectAuth, chttpd_auth:party_mode_handler(NonUpRequest)),

    % 8
    set_require_user_true(),
    set_require_user_except_for_up_true(),
    ?assertThrow(ExpectAuth, chttpd_auth:party_mode_handler(NonUpRequest))
  end).
