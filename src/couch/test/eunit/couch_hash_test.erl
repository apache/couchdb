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

-module(couch_hash_test).

-include_lib("couch/include/couch_eunit.hrl").

-define(XY_HASH, <<62, 68, 16, 113, 112, 165, 32, 88, 42, 222, 82, 47, 167, 60, 29, 21>>).

couch_hash_test_() ->
    {
        foreach,
        fun setup/0,
        fun teardown/1,
        [
            ?TDEF_FE(t_fips_disabled),
            ?TDEF_FE(t_fips_enabled)
        ]
    }.

setup() ->
    Ctx = test_util:start_couch([crypto]),
    config:disable_feature(fips),
    Ctx.

teardown(Ctx) ->
    config:disable_feature(fips),
    test_util:stop_couch(Ctx).

t_fips_disabled(_) ->
    ?assertEqual(?XY_HASH, couch_hash:md5_hash(<<"xy">>)),
    H = couch_hash:md5_hash_init(),
    H1 = couch_hash:md5_hash_update(H, <<"x">>),
    H2 = couch_hash:md5_hash_update(H1, <<"y">>),
    ?assertEqual(?XY_HASH, couch_hash:md5_hash_final(H2)).

t_fips_enabled(_) ->
    config:enable_feature(fips),
    ?assertEqual(?XY_HASH, couch_hash:md5_hash(<<"xy">>)),
    H = couch_hash:md5_hash_init(),
    H1 = couch_hash:md5_hash_update(H, <<"x">>),
    H2 = couch_hash:md5_hash_update(H1, <<"y">>),
    ?assertEqual(?XY_HASH, couch_hash:md5_hash_final(H2)).
