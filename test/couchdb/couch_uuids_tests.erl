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

-include("couch_eunit.hrl").

-define(TIMEOUT_S, 20).


setup() ->
    {ok, Pid} = couch_config:start_link(?CONFIG_CHAIN),
    erlang:monitor(process, Pid),
    couch_uuids:start(),
    Pid.

setup(Opts) ->
    Pid = setup(),
    lists:foreach(
        fun({Option, Value}) ->
            couch_config:set("uuids", Option, Value, false)
        end, Opts),
    Pid.

teardown(Pid) ->
    couch_uuids:stop(),
    couch_config:stop(),
    receive
        {'DOWN', _, _, Pid, _} -> ok
    after
        1000 -> throw({timeout_error, config_stop})
    end.

teardown(_, Pid) ->
    teardown(Pid).


default_test_() ->
    {
        "Default UUID algorithm",
        {
            setup,
            fun setup/0, fun teardown/1,
            fun should_be_unique/1
        }
    }.

sequential_test_() ->
    Opts = [{"algorithm", "sequential"}],
    Cases = [
        fun should_be_unique/2,
        fun should_increment_monotonically/2,
        fun should_rollover/2
    ],
    {
        "UUID algorithm: sequential",
        {
            foreachx,
            fun setup/1, fun teardown/2,
            [{Opts, Fun} || Fun <- Cases]
        }
    }.

utc_test_() ->
    Opts = [{"algorithm", "utc_random"}],
    Cases = [
        fun should_be_unique/2,
        fun should_increment_monotonically/2
    ],
    {
        "UUID algorithm: utc_random",
        {
            foreachx,
            fun setup/1, fun teardown/2,
            [{Opts, Fun} || Fun <- Cases]
        }
    }.

utc_id_suffix_test_() ->
    Opts = [{"algorithm", "utc_id"}, {"utc_id_suffix", "bozo"}],
    Cases = [
        fun should_be_unique/2,
        fun should_increment_monotonically/2,
        fun should_preserve_suffix/2
    ],
    {
        "UUID algorithm: utc_id",
        {
            foreachx,
            fun setup/1, fun teardown/2,
            [{Opts, Fun} || Fun <- Cases]
        }
    }.


should_be_unique() ->
    %% this one may really runs for too long on slow hosts
    {timeout, ?TIMEOUT_S, ?_assert(test_unique(10000, [couch_uuids:new()]))}.
should_be_unique(_) ->
    should_be_unique().
should_be_unique(_, _) ->
    should_be_unique().

should_increment_monotonically(_, _) ->
    ?_assert(couch_uuids:new() < couch_uuids:new()).

should_rollover(_, _) ->
    ?_test(begin
        UUID = binary_to_list(couch_uuids:new()),
        Prefix = element(1, lists:split(26, UUID)),
        N = gen_until_pref_change(Prefix, 0),
        ?assert(N >= 5000 andalso N =< 11000)
    end).

should_preserve_suffix(_, _) ->
    ?_test(begin
        UUID = binary_to_list(couch_uuids:new()),
        Suffix = get_suffix(UUID),
        ?assert(test_same_suffix(10000, Suffix))
    end).


test_unique(0, _) ->
    true;
test_unique(N, UUIDs) ->
    UUID = couch_uuids:new(),
    ?assertNot(lists:member(UUID, UUIDs)),
    test_unique(N - 1, [UUID| UUIDs]).

get_prefix(UUID) ->
    element(1, lists:split(26, binary_to_list(UUID))).

gen_until_pref_change(_, Count) when Count > 8251 ->
    Count;
gen_until_pref_change(Prefix, N) ->
    case get_prefix(couch_uuids:new()) of
        Prefix -> gen_until_pref_change(Prefix, N + 1);
        _ -> N
    end.

get_suffix(UUID) when is_binary(UUID) ->
    get_suffix(binary_to_list(UUID));
get_suffix(UUID) ->
    element(2, lists:split(14, UUID)).

test_same_suffix(0, _) ->
    true;
test_same_suffix(N, Suffix) ->
    case get_suffix(couch_uuids:new()) of
        Suffix -> test_same_suffix(N - 1, Suffix);
        _ -> false
    end.
