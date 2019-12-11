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

-module(couch_expiring_cache_tests).


-include_lib("couch/include/couch_eunit.hrl").

-include_lib("couch_expiring_cache/include/couch_expiring_cache.hrl").


-define(CACHE_NAME, <<"test">>).


start_link() ->
    Opts = #{
        cache_name => ?CACHE_NAME,
        period => 20,
        max_jitter => 0
    },
    couch_expiring_cache_server:start_link(?MODULE, Opts).


couch_expiring_cache_basic_test_() ->
    {
        "Test expiring cache basics",
        {
            setup,
            fun setup_couch/0, fun teardown_couch/1,
            {
                foreach,
                fun setup/0, fun teardown/1,
                [
                    fun simple_lifecycle/1
                ]
            }
        }
    }.


setup_couch() ->
    test_util:start_couch([fabric, couch_jobs]).


teardown_couch(Ctx) ->
    test_util:stop_couch(Ctx).


setup() ->
    {ok, Pid} = start_link(),
    true = unlink(Pid),
    #{pid => Pid}.


teardown(#{pid := Pid}) ->
    exit(Pid, kill).


simple_lifecycle(_) ->
    ?_test(begin
        Now = erlang:system_time(?TIME_UNIT),
        StaleTS = Now + 100,
        ExpiresTS = Now + 200,
        Name = ?CACHE_NAME,
        Key = <<"key">>,
        Val = <<"val">>,

        ?assertEqual(ok, couch_expiring_cache_fdb:clear_all(Name)),
        ?assertEqual(not_found, couch_expiring_cache:lookup(Name, Key)),
        ?assertEqual(ok,
            couch_expiring_cache:insert(Name, Key, Val, StaleTS, ExpiresTS)),
        ?assertEqual({fresh, Val}, couch_expiring_cache:lookup(Name, Key)),
        ok = wait_lookup(Name, Key, {stale, Val}),
        ok = wait_lookup(Name, Key, expired),
        ok = wait_lookup(Name, Key, not_found),
        ?assertEqual(not_found, couch_expiring_cache:lookup(Name, Key))
    end).


wait_lookup(Name, Key, Expect) ->
    test_util:wait(fun() ->
        case couch_expiring_cache:lookup(Name, Key) of
            Expect -> ok;
            _ -> wait
        end
    end, _Timeout = 1000, _PollingInterval = 10).
