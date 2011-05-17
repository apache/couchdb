#!/usr/bin/env escript
%% -*- erlang -*-

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

default_config() ->
    test_util:build_file("etc/couchdb/default_dev.ini").

seq_alg_config() ->
    test_util:source_file("test/etap/041-uuid-gen-seq.ini").

utc_alg_config() ->
    test_util:source_file("test/etap/041-uuid-gen-utc.ini").

% Run tests and wait for the gen_servers to shutdown
run_test(IniFiles, Test) ->
    {ok, Pid} = couch_config:start_link(IniFiles),
    erlang:monitor(process, Pid),
    couch_uuids:start(),
    Test(),
    couch_uuids:stop(),
    couch_config:stop(),
    receive
        {'DOWN', _, _, Pid, _} -> ok;
        _Other -> etap:diag("OTHER: ~p~n", [_Other])
    after
        1000 -> throw({timeout_error, config_stop})
    end.

main(_) ->
    test_util:init_code_path(),
    application:start(crypto),
    etap:plan(6),

    case (catch test()) of
        ok ->
            etap:end_tests();
        Other ->
            etap:diag(io_lib:format("Test died abnormally: ~p", [Other])),
            etap:bail(Other)
    end,
    ok.

test() ->

    TestUnique = fun() ->
        etap:is(
            test_unique(10000, couch_uuids:new()),
            true,
            "Can generate 10K unique IDs"
        )
    end,
    run_test([default_config()], TestUnique),
    run_test([default_config(), seq_alg_config()], TestUnique),
    run_test([default_config(), utc_alg_config()], TestUnique),

    TestMonotonic = fun () ->
        etap:is(
            couch_uuids:new() < couch_uuids:new(),
            true,
            "should produce monotonically increasing ids"
        )
    end,
    run_test([default_config(), seq_alg_config()], TestMonotonic),
    run_test([default_config(), utc_alg_config()], TestMonotonic),

    % Pretty sure that the average of a uniform distribution is the
    % midpoint of the range. Thus, to exceed a threshold, we need
    % approximately Total / (Range/2 + RangeMin) samples.
    %
    % In our case this works out to be 8194. (0xFFF000 / 0x7FF)
    % These tests just fudge the limits for a good generator at 25%
    % in either direction. Technically it should be possible to generate
    % bounds that will show if your random number generator is not
    % sufficiently random but I hated statistics in school.
    TestRollOver = fun() ->
        UUID = binary_to_list(couch_uuids:new()),
        Prefix = element(1, lists:split(26, UUID)),
        N = gen_until_pref_change(Prefix,0),
        etap:diag("N is: ~p~n",[N]),                           
        etap:is(
            N >= 5000 andalso N =< 11000,
            true,
            "should roll over every so often."
        )
    end,
    run_test([default_config(), seq_alg_config()], TestRollOver).

test_unique(0, _) ->
    true;
test_unique(N, UUID) ->
    case couch_uuids:new() of
        UUID ->
            etap:diag("N: ~p~n", [N]),
            false;
        Else -> test_unique(N-1, Else)
    end.

get_prefix(UUID) ->
    element(1, lists:split(26, binary_to_list(UUID))).

gen_until_pref_change(_, Count) when Count > 8251 ->
    Count;
gen_until_pref_change(Prefix, N) ->
    case get_prefix(couch_uuids:new()) of
        Prefix -> gen_until_pref_change(Prefix, N+1);
        _ -> N
    end.
