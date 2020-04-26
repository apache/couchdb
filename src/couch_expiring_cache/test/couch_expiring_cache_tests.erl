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


-define(CACHE_NAME, atom_to_binary(?MODULE, utf8)).

-define(FOREVER, 576460752303423488). % max int 64 bit


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
    Opts = #{
        cache_name => ?CACHE_NAME,
        period => 10,
        max_jitter => 0},
    {ok, Pid} = couch_expiring_cache_server:start_link(?MODULE, Opts),
    true = unlink(Pid),
    #{pid => Pid}.


teardown(#{pid := Pid}) ->
    exit(Pid, kill).


simple_lifecycle(_) ->
    % The entire test is racing against FDB being faster than timeout seconds
    {timeout, 20, ?_test(begin
        Start = couch_expiring_cache_server:now_ts(),
        % Race Alert!
        % We're betting on FDB returning a lookup faster than these:
        Stale = 500,
        Expires = 1000,
        Timeout = 5000,
        Interval = 5,

        StaleTS = Start + Stale,
        ExpiresTS = Start + Expires,
        Name = ?CACHE_NAME,
        Key = <<"key">>,
        Val = <<"val">>,

        ?assertEqual(ok, couch_expiring_cache_fdb:clear_all(Name)),
        ?assertEqual(not_found, couch_expiring_cache:lookup(Name, Key)),
        ?assertEqual([], entries(Name)),
        ?assertEqual(ok, couch_expiring_cache:insert(Name, Key, Val,
            StaleTS, ExpiresTS)),
        ok = attempt_fresh_and_stale_lookups(Name, Key, Timeout, Interval),

        % Refresh the existing key with updated timestamps
        Refresh = couch_expiring_cache_server:now_ts(),
        ?assertEqual(ok, couch_expiring_cache:insert(Name, Key, Val,
            Refresh + Stale, Refresh + Expires)),
        ok = attempt_fresh_and_stale_lookups(Name, Key, Timeout, Interval),
        ?assertEqual(1, length(entries(Name))),
        % These last 2 are also races, betting on FDB to be reasonably
        % fast on the home stretch
        ok = wait_lookup(Name, Key, expired, Timeout, Interval),
        ok = wait_lookup(Name, Key, not_found, Timeout, Interval),
        ?assertEqual([], entries(Name))
    end)}.


% In this race we're betting on FDB to take less than `Stale` and then
% `Expired` milliseconds to respond
attempt_fresh_and_stale_lookups(Name, Key, Timeout, Interval) ->
    case couch_expiring_cache:lookup(Name, Key) of
        {fresh, Val} ->
            % We won that race, let's bet on another!
            ok = wait_lookup(Name, Key, {stale, Val}, Timeout, Interval);
        _ ->
            % Unlucky! But don't fail the test just yet...
            ok
    end.


entries(Name) ->
    couch_expiring_cache_fdb:get_range_to(Name, ?FOREVER, _Limit=100).


% This lookup races against Timeout
wait_lookup(Name, Key, Expect, Timeout, Interval) ->
    wait(fun() ->
        case couch_expiring_cache:lookup(Name, Key) of
            Expect -> ok;
            _ -> wait
        end
    end, Timeout, Interval).


wait(Fun, Timeout, Delay) ->
    Now = couch_expiring_cache_server:now_ts(),
    wait(Fun, Timeout, Delay, Now, Now).


wait(_Fun, Timeout, _Delay, Started, Prev) when Prev - Started > Timeout ->
    timeout;

wait(Fun, Timeout, Delay, Started, _Prev) ->
    case Fun() of
        wait ->
            % http://erlang.org/doc/man/timer.html#sleep-1
            ok = timer:sleep(Delay), % always millisecond
            wait(Fun, Timeout, Delay, Started,
                couch_expiring_cache_server:now_ts());
        Else ->
            Else
    end.
