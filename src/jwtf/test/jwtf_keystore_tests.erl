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

-module(jwtf_keystore_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("public_key/include/public_key.hrl").

-define(HMAC_SECRET, "aGVsbG8=").
-define(RSA_SECRET, "-----BEGIN PUBLIC KEY-----\\nMIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAztanwQtIx0sms+x7m1SF\\nh7EHJHkM2biTJ41jR89FsDE2gd3MChpaqxemS5GpNvfFKRvuHa4PUZ3JtRCBG1KM\\n/7EWIVTy1JQDr2mb8couGlQNqz4uXN2vkNQ0XszgjU4Wn6ZpvYxmqPFbmkRe8QSn\\nAy2Wf8jQgjsbez8eaaX0G9S1hgFZUN3KFu7SVmUDQNvWpQdaJPP+ms5Z0CqF7JLa\\nvJmSdsU49nlYw9VH/XmwlUBMye6HgR4ZGCLQS85frqF0xLWvi7CsMdchcIjHudXH\\nQK1AumD/VVZVdi8Q5Qew7F6VXeXqnhbw9n6Px25cCuNuh6u5+E6GUzXRrMpqo9vO\\nqQIDAQAB\\n-----END PUBLIC KEY-----\\n").
-define(BAD_RSA_SECRET,"-----BEGIN PUBLIC KEY-----\\nMIIDAzCCAeugAwIBAgIJAL5YnwkF5jT6MA0GCSqGSIb3DQEBBQUAMBgxFjAUBgNV\\nBAMMDWZvby5hdXRoMC5jb20wHhcNMTQwMzE4MjAwNzUwWhcNMjcxMTI1MjAwNzUw\\nWjAYMRYwFAYDVQQDDA1mb28uYXV0aDAuY29tMIIBIjANBgkqhkiG9w0BAQEFAAOC\\nAQ8AMIIBCgKCAQEAtP6w43ppU0nkqGNHASojFJl60+k3isNVzYTO06f2vm/5tc3l\\nRhEA6ykyIuO8tHY3Ziqowc4h8XGaeDKqHw/BSS/b54F2rUVb/wACWyJICkM3bGtC\\ntWmM7kU8XZRCqXV04qIgQte+9GFSOax/TFyotS+FGFyFPUY+b57H7/6wNQ8ywGLi\\nWCbrWEx4wOJbGhnVNV+STmZXJgToLgz0R2kwsiGURhHMkNkUjcRl34nSv+lMYSMK\\nyywwzu0k3KBgqkxWibU3pa3jibWVRxc20f8ltfByp/wU/ICQ0MNGJ3/KaCiOtGQa\\noZOa7bMzb4W1x2L3cfgrshLrp978+FEeNzY9KQIDAQABo1AwTjAdBgNVHQ4EFgQU\\nOyDe79RE2SYTcCNPbniw3p4uZykwHwYDVR0jBBgwFoAUOyDe79RE2SYTcCNPbniw\\n3p4uZykwDAYDVR0TBAUwAwEB/zANBgkqhkiG9w0BAQUFAAOCAQEAW0mB5wR1sSHC\\n7iSmQo1uioH80X7txJY6zXH8hVjoCQOGUCi79x43L9wUTtyJg44Z8RhNozWOsCZM\\nf5LDSkeNx48QITrinDqWv5C/NA0klJ1g0Y/jN9X01r5T6vGdge8inIbQcO7ZrJ6v\\nVYDH+9HLvfPKFYd0uhYRFnw2aa3mKIRsanlWSEYHQr5Aoa+nboFLRiDtVWBuiAoV\\nZ1NoYm7uheU42CNGJqkv6SXxKHTea2TjmOxKRmaxYMvkjk/CsiPrSEQHUxDXqSSd\\nrIWU8o+9q9Hpdb3UuNJzMjlTzg2/UeHpzMBJAWxUlzTuXMqrrDFF9V/d4zO77Ts/\\n4mRBKB+GsQ==\\n-----END PUBLIC KEY-----\\n").

-define(EC_SECRET, "-----BEGIN PUBLIC KEY-----\\nMHYwEAYHKoZIzj0CAQYFK4EEACIDYgAEDsr0lz/Dg3luarb+Kua0Wcj9WrfR23os\\nwHzakglb8GhWRDn+oZT0Bt/26sX8uB4/ij9PEOLHPo+IHBtX4ELFFVr5GTzlqcJe\\nyctaTDd1OOAPXYuc67EWtGZ3pDAzztRs\\n-----END PUBLIC KEY-----\\n").

setup() ->
    test_util:start_applications([config, jwtf]),
    config:set("jwt_keys", "hmac:hmac", ?HMAC_SECRET),
    config:set("jwt_keys", "rsa:hmac", ?HMAC_SECRET),
    config:set("jwt_keys", "ec:hmac", ?HMAC_SECRET),

    config:set("jwt_keys", "hmac:rsa", ?RSA_SECRET),
    config:set("jwt_keys", "rsa:rsa", ?RSA_SECRET),
    config:set("jwt_keys", "ec:rsa", ?RSA_SECRET),

    config:set("jwt_keys", "hmac:ec", ?EC_SECRET),
    config:set("jwt_keys", "rsa:ec", ?EC_SECRET),
    config:set("jwt_keys", "ec:ec", ?EC_SECRET),

    config:set("jwt_keys", "rsa:badrsa", ?BAD_RSA_SECRET).


teardown(_) ->
    test_util:stop_applications([config, jwtf]).

jwtf_keystore_test_() ->
    {
     setup,
     fun setup/0,
     fun teardown/1,
     [
      ?_assertEqual(<<"hello">>,       jwtf_keystore:get(<<"HS256">>, <<"hmac">>)),
      ?_assertThrow({bad_request, _},  jwtf_keystore:get(<<"RS256">>, <<"hmac">>)),
      ?_assertThrow({bad_request, _},  jwtf_keystore:get(<<"ES256">>, <<"hmac">>)),

      ?_assertThrow({bad_request, _},  jwtf_keystore:get(<<"HS256">>, <<"rsa">>)),
      ?_assertMatch(#'RSAPublicKey'{}, jwtf_keystore:get(<<"RS256">>, <<"rsa">>)),
      ?_assertThrow({bad_request, _},  jwtf_keystore:get(<<"ES256">>, <<"rsa">>)),

      ?_assertThrow({bad_request, _},  jwtf_keystore:get(<<"HS256">>, <<"ec">>)),
      ?_assertThrow({bad_request, _},  jwtf_keystore:get(<<"RS256">>, <<"ec">>)),
      ?_assertMatch({#'ECPoint'{}, _}, jwtf_keystore:get(<<"ES256">>, <<"ec">>)),

      ?_assertThrow({bad_request, <<"Not a valid key">>}, jwtf_keystore:get(<<"RS256">>, <<"badrsa">>))
     ]
    }.
