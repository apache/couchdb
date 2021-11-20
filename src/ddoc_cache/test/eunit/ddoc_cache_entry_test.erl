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

-module(ddoc_cache_entry_test).

-export([
    recover/1
]).

-include_lib("couch/include/couch_db.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("ddoc_cache_test.hrl").

recover(<<"foo">>) ->
    timer:sleep(30000);
recover(DbName) ->
    {ok, {DbName, such_custom}}.

start_couch() ->
    Ctx = ddoc_cache_tutil:start_couch(),
    meck:new(ddoc_cache_ev, [passthrough]),
    Ctx.

stop_couch(Ctx) ->
    meck:unload(),
    ddoc_cache_tutil:stop_couch(Ctx).

check_entry_test_() ->
    {
        setup,
        fun start_couch/0,
        fun stop_couch/1,
        ddoc_cache_tutil:with([
            {"cancel_and_replace_opener", fun cancel_and_replace_opener/1},
            {"condenses_access_messages", fun condenses_access_messages/1},
            {"kill_opener_on_terminate", fun kill_opener_on_terminate/1},
            {"evict_when_not_accessed", fun evict_when_not_accessed/1},
            {"open_dead_entry", fun open_dead_entry/1},
            {"handles_bad_messages", fun handles_bad_messages/1},
            {"handles_code_change", fun handles_code_change/1}
        ])
    }.

cancel_and_replace_opener(_) ->
    Key = {ddoc_cache_entry_custom, {<<"foo">>, ?MODULE}},
    true = ets:insert_new(?CACHE, #entry{key = Key}),
    {ok, Entry} = ddoc_cache_entry:start_link(Key, undefined),
    Opener1 = element(4, sys:get_state(Entry)),
    Ref1 = erlang:monitor(process, Opener1),
    gen_server:cast(Entry, force_refresh),
    receive
        {'DOWN', Ref1, _, _, _} -> ok
    end,
    Opener2 = element(4, sys:get_state(Entry)),
    ?assert(Opener2 /= Opener1),
    ?assert(is_process_alive(Opener2)),
    % Clean up after ourselves
    unlink(Entry),
    ddoc_cache_entry:shutdown(Entry).

condenses_access_messages({DbName, _}) ->
    meck:reset(ddoc_cache_ev),
    Key = {ddoc_cache_entry_custom, {DbName, ?MODULE}},
    true = ets:insert(?CACHE, #entry{key = Key}),
    {ok, Entry} = ddoc_cache_entry:start_link(Key, undefined),
    erlang:suspend_process(Entry),
    lists:foreach(
        fun(_) ->
            gen_server:cast(Entry, accessed)
        end,
        lists:seq(1, 100)
    ),
    erlang:resume_process(Entry),
    meck:wait(1, ddoc_cache_ev, event, [accessed, Key], 1000),
    ?assertError(
        timeout,
        meck:wait(2, ddoc_cache_ev, event, [accessed, Key], 100)
    ),
    unlink(Entry),
    ddoc_cache_entry:shutdown(Entry).

kill_opener_on_terminate(_) ->
    Pid = spawn(fun() ->
        receive
            _ -> ok
        end
    end),
    ?assert(is_process_alive(Pid)),
    St = {st, key, val, Pid, waiters, ts, accessed},
    ?assertEqual(ok, ddoc_cache_entry:terminate(normal, St)),
    ?assert(not is_process_alive(Pid)).

evict_when_not_accessed(_) ->
    meck:reset(ddoc_cache_ev),
    Key = {ddoc_cache_entry_custom, {<<"bar">>, ?MODULE}},
    true = ets:insert_new(?CACHE, #entry{key = Key}),
    {ok, Entry} = ddoc_cache_entry:start_link(Key, undefined),
    Ref = erlang:monitor(process, Entry),
    AccessCount1 = element(7, sys:get_state(Entry)),
    ?assertEqual(1, AccessCount1),
    ok = gen_server:cast(Entry, refresh),

    meck:wait(ddoc_cache_ev, event, [update_noop, Key], 1000),

    AccessCount2 = element(7, sys:get_state(Entry)),
    ?assertEqual(0, AccessCount2),
    ok = gen_server:cast(Entry, refresh),
    receive
        {'DOWN', Ref, _, _, Reason} -> Reason
    end,
    ?assertEqual(normal, Reason),
    ?assertEqual(0, ets:info(?CACHE, size)).

open_dead_entry({DbName, _}) ->
    Pid = spawn(fun() -> ok end),
    Key = {ddoc_cache_entry_custom, {DbName, ?MODULE}},
    ?assertEqual(recover(DbName), ddoc_cache_entry:open(Pid, Key)).

handles_bad_messages(_) ->
    CallExpect = {stop, {bad_call, foo}, {bad_call, foo}, baz},
    CastExpect = {stop, {bad_cast, foo}, bar},
    InfoExpect = {stop, {bad_info, foo}, bar},
    ?assertEqual(CallExpect, ddoc_cache_entry:handle_call(foo, bar, baz)),
    ?assertEqual(CastExpect, ddoc_cache_entry:handle_cast(foo, bar)),
    ?assertEqual(InfoExpect, ddoc_cache_entry:handle_info(foo, bar)).

handles_code_change(_) ->
    CCExpect = {ok, bar},
    ?assertEqual(CCExpect, ddoc_cache_entry:code_change(foo, bar, baz)).

handles_bad_shutdown_test_() ->
    {timeout, 10,
        ?_test(begin
            ErrorPid = spawn(fun() ->
                receive
                    _ -> exit(bad_shutdown)
                end
            end),
            ?assertExit(bad_shutdown, ddoc_cache_entry:shutdown(ErrorPid)),
            NotDeadYetPid = spawn(fun() ->
                timer:sleep(infinity)
            end),
            ?assertExit(
                {timeout, {entry_shutdown, NotDeadYetPid}},
                ddoc_cache_entry:shutdown(NotDeadYetPid)
            )
        end)}.
