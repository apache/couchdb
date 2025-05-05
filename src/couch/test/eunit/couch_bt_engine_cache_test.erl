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

-module(couch_bt_engine_cache_test).

-include_lib("couch/include/couch_eunit.hrl").

couch_bt_engine_cache_test_() ->
    {
        foreach,
        fun setup/0,
        fun teardown/1,
        [
            ?TDEF_FE(t_created),
            ?TDEF_FE(t_insert_and_lookup),
            ?TDEF_FE(t_decay_and_removal_works, 10),
            ?TDEF_FE(t_pid_cleanup_works, 10)
        ]
    }.

setup() ->
    Ctx = test_util:start_applications([config]),
    couch_bt_engine_cache:create_tables(),
    #{shard_count := N} = couch_bt_engine_cache:info(),
    Pids = lists:map(
        fun(I) ->
            {ok, Pid} = couch_bt_engine_cache:start_link(I),
            Pid
        end,
        lists:seq(1, N)
    ),
    {Ctx, Pids}.

teardown({Ctx, [_ | _] = Pids}) ->
    lists:foreach(
        fun(Pid) ->
            catch unlink(Pid),
            exit(Pid, kill)
        end,
        Pids
    ),
    clear_tables(),
    config:delete("bt_engine_cache", "max_size", false),
    config:delete("bt_engine_cache", "leave_percent", false),
    test_util:stop_applications(Ctx).

clear_tables() ->
    lists:foreach(fun ets:delete/1, couch_bt_engine_cache:tables()).

t_created(_) ->
    Info = couch_bt_engine_cache:info(),
    #{size := Size, memory := Mem, shard_count := N} = Info,
    ?assert(N >= 16, "shard count is greater or equal to 16"),
    ?assertEqual(0, Size),
    ?assert(is_integer(Mem)),
    ?assert(Mem >= 0).

t_insert_and_lookup(_) ->
    ?assertError(function_clause, couch_bt_engine_cache:insert(not_a_pid, 1, foo)),
    ?assertError(function_clause, couch_bt_engine_cache:insert(self(), xyz, foo)),
    ?assertMatch(#{size := 0}, couch_bt_engine_cache:info()),
    ?assert(couch_bt_engine_cache:insert({pid, 42}, term)),
    ?assertMatch(#{size := 1}, couch_bt_engine_cache:info()),
    ?assertNot(couch_bt_engine_cache:insert({pid, 42}, term)),
    ?assertEqual(term, couch_bt_engine_cache:lookup({pid, 42})),
    ?assertEqual(undefined, couch_bt_engine_cache:lookup({pid, 43})).

t_decay_and_removal_works(_) ->
    config:set("bt_engine_cache", "leave_percent", "0", false),
    Term = [foo, bar, baz, lists:seq(1, 100)],
    [couch_bt_engine_cache:insert({pid, I}, Term) || I <- lists:seq(1, 10000)],
    WaitFun = fun() ->
        #{size := Size} = couch_bt_engine_cache:info(),
        case Size > 0 of
            true -> wait;
            false -> ok
        end
    end,
    test_util:wait(WaitFun, 7500),
    ?assertMatch(#{size := 0}, couch_bt_engine_cache:info()).

t_pid_cleanup_works(_) ->
    Pid = spawn(fun() -> timer:sleep(2000) end),
    [couch_bt_engine_cache:insert({Pid, I}, baz) || I <- lists:seq(1, 1000)],
    WaitFun = fun() ->
        #{size := Size} = couch_bt_engine_cache:info(),
        case Size > 0 of
            true -> wait;
            false -> ok
        end
    end,
    test_util:wait(WaitFun, 7500),
    ?assertMatch(#{size := 0}, couch_bt_engine_cache:info()).
