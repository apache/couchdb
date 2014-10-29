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

-module(couch_totp_tests).

-include_lib("eunit/include/eunit.hrl").

totp_sha_test() ->
    Key = <<"12345678901234567890">>,
    ?assertEqual(94287082, couch_totp:generate(sha, Key, 59, 30, 8)),
    ?assertEqual(07081804, couch_totp:generate(sha, Key, 1111111109, 30, 8)),
    ?assertEqual(14050471, couch_totp:generate(sha, Key, 1111111111, 30, 8)),
    ?assertEqual(89005924, couch_totp:generate(sha, Key, 1234567890, 30, 8)),
    ?assertEqual(69279037, couch_totp:generate(sha, Key, 2000000000, 30, 8)),
    ?assertEqual(65353130, couch_totp:generate(sha, Key, 20000000000, 30, 8)).

totp_sha256_test() ->
    Key = <<"12345678901234567890123456789012">>,
    case sha_256_512_supported() of
        true ->
            ?assertEqual(46119246, couch_totp:generate(sha256, Key, 59, 30, 8)),
            ?assertEqual(68084774, couch_totp:generate(sha256, Key, 1111111109, 30, 8)),
            ?assertEqual(67062674, couch_totp:generate(sha256, Key, 1111111111, 30, 8)),
            ?assertEqual(91819424, couch_totp:generate(sha256, Key, 1234567890, 30, 8)),
            ?assertEqual(90698825, couch_totp:generate(sha256, Key, 2000000000, 30, 8)),
            ?assertEqual(77737706, couch_totp:generate(sha256, Key, 20000000000, 30, 8));
        false ->
            ?debugMsg("sha256 not supported, tests skipped")
    end.

totp_sha512_test() ->
    Key = <<"1234567890123456789012345678901234567890123456789012345678901234">>,
    case sha_256_512_supported() of
        true ->
            ?assertEqual(90693936, couch_totp:generate(sha512, Key, 59, 30, 8)),
            ?assertEqual(25091201, couch_totp:generate(sha512, Key, 1111111109, 30, 8)),
            ?assertEqual(99943326, couch_totp:generate(sha512, Key, 1111111111, 30, 8)),
            ?assertEqual(93441116, couch_totp:generate(sha512, Key, 1234567890, 30, 8)),
            ?assertEqual(38618901, couch_totp:generate(sha512, Key, 2000000000, 30, 8)),
            ?assertEqual(47863826, couch_totp:generate(sha512, Key, 20000000000, 30, 8));
        false ->
            ?debugMsg("sha512 not supported, tests skipped")
    end.

sha_256_512_supported() ->
    erlang:function_exported(crypto, hmac, 3).
