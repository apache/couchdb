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

-module(couch_uuids_tests).

-include_lib("couch/include/couch_eunit.hrl").

-define(TIMEOUT, 20).


setup_all() ->
    test_util:start_applications([config]),
    couch_uuids:start().


teardown_all(_) ->
    couch_uuids:stop(),
    test_util:stop_applications([config]).


uuids_test_() ->
    {
        setup,
        fun setup_all/0,
        fun teardown_all/1,
        [
            {timeout, ?TIMEOUT, fun default_algorithm/0},
            {timeout, ?TIMEOUT, fun sequential_algorithm/0},
            {timeout, ?TIMEOUT, fun utc_algorithm/0},
            {timeout, ?TIMEOUT, fun utc_id_suffix_algorithm/0}
        ]
    }.


default_algorithm() ->
    config:delete("uuids", "algorithm", false),
    check_unique().


sequential_algorithm() ->
    config:set("uuids", "algorithm", "sequential", false),
    check_unique(),
    check_increment_monotonically(),
    check_rollover().


utc_algorithm() ->
    config:set("uuids", "algorithm", "utc_random", false),
    check_unique(),
    check_increment_monotonically().


utc_id_suffix_algorithm() ->
    config:set("uuids", "algorithm", "utc_id", false),
    config:set("uuids", "utc_id_suffix", "bozo", false),
    check_unique(),
    check_increment_monotonically(),
    check_preserve_suffix().


check_unique() ->
    %% this one may really runs for too long on slow hosts
    ?assert(test_unique(10000, [couch_uuids:new()])).


check_increment_monotonically() ->
    ?assert(couch_uuids:new() < couch_uuids:new()).


check_rollover() ->
    UUID = binary_to_list(couch_uuids:new()),
    Prefix = element(1, lists:split(26, UUID)),
    N = gen_until_pref_change(Prefix, 0),
    ?assert(N >= 5000 andalso N =< 11000).


check_preserve_suffix() ->
    UUID = binary_to_list(couch_uuids:new()),
    Suffix = get_suffix(UUID),
    ?assert(test_same_suffix(10000, Suffix)).


test_unique(0, _) ->
    true;
test_unique(N, UUIDs) ->
    UUID = couch_uuids:new(),
    ?assertNot(lists:member(UUID, UUIDs)),
    test_unique(N - 1, [UUID| UUIDs]).


gen_until_pref_change(_, Count) when Count > 8251 ->
    Count;
gen_until_pref_change(Prefix, N) ->
    case get_prefix(couch_uuids:new()) of
        Prefix -> gen_until_pref_change(Prefix, N + 1);
        _ -> N
    end.


test_same_suffix(0, _) ->
    true;
test_same_suffix(N, Suffix) ->
    case get_suffix(couch_uuids:new()) of
        Suffix -> test_same_suffix(N - 1, Suffix);
        _ -> false
    end.


get_prefix(UUID) ->
    element(1, lists:split(26, binary_to_list(UUID))).


get_suffix(UUID) when is_binary(UUID) ->
    get_suffix(binary_to_list(UUID));
get_suffix(UUID) ->
    element(2, lists:split(14, UUID)).
