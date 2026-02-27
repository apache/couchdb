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

-module(couch_secrets_tests).

-include_lib("couch/include/couch_eunit.hrl").

-define(DATA1, <<"data1">>).
-define(DATA2, <<"data2">>).
-define(BADMAC, <<"badmac">>).
-define(EXTRA1, <<"extra1">>).
-define(EXTRA2, <<"extra2">>).
-define(SECRET1, "secret1").
-define(SECRET2, "secret2").

couch_secrets_test_() ->
    {setup, fun test_util:start_couch/0, fun test_util:stop_couch/1,
        {with, [
            fun error_if_no_secret/1,
            fun verify_works/1,
            fun verify_extra_secret_works/1,
            fun verify_old_secret_works/1,
            fun verify_old_secret_stops_working/1
        ]}}.

error_if_no_secret(_Ctx) ->
    delete_secret(),
    ?assertThrow(
        {internal_server_error, <<"cookie auth secret is not set">>}, couch_secrets:sign(?DATA1)
    ).

verify_works(_Ctx) ->
    set_secret(?SECRET1),
    MAC = couch_secrets:sign(?DATA1),
    ?assert(couch_secrets:verify(?DATA1, MAC)),
    ?assertNot(couch_secrets:verify(?DATA1, ?BADMAC)),
    ?assertNot(couch_secrets:verify(?DATA2, MAC)).

verify_extra_secret_works(_Ctx) ->
    set_secret(?SECRET1),
    MAC = couch_secrets:sign(?DATA1, ?EXTRA1),
    ?assert(couch_secrets:verify(?DATA1, ?EXTRA1, MAC)),
    ?assertNot(couch_secrets:verify(?DATA1, ?EXTRA2, MAC)),
    ?assertNot(couch_secrets:verify(?DATA1, ?EXTRA1, ?BADMAC)),
    ?assertNot(couch_secrets:verify(?DATA2, ?EXTRA1, MAC)).

verify_old_secret_works(_Ctx) ->
    set_secret(?SECRET1),
    MAC1 = couch_secrets:sign(?DATA1),
    set_secret(?SECRET2),
    MAC2 = couch_secrets:sign(?DATA1),
    ?assert(couch_secrets:verify(?DATA1, MAC1)),
    ?assert(couch_secrets:verify(?DATA1, MAC2)).

verify_old_secret_stops_working(_Ctx) ->
    set_secret(?SECRET1),
    MAC1 = couch_secrets:sign(?DATA1),
    ?assert(couch_secrets:verify(?DATA1, MAC1)),
    set_secret(?SECRET2),
    MAC2 = couch_secrets:sign(?DATA1),
    ?assert(couch_secrets:verify(?DATA1, MAC2)),
    ?assert(gen_server:call(couch_secrets, flush_cache) > 0),
    ?assertNot(couch_secrets:verify(?DATA1, MAC1)),
    ?assert(couch_secrets:verify(?DATA1, MAC2)).

delete_secret() ->
    config:delete("chttpd_auth", "secret"),
    config:delete("couch_httpd_auth", "secret").

set_secret(Secret) ->
    config:set("chttpd_auth", "secret", Secret),
    timer:sleep(100).
