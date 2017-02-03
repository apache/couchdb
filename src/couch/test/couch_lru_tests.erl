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

-module(couch_lru_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("couch/src/couch_server_int.hrl").


setup() ->
    ok = meck:new(couch_db, [passthrough]),
    ets:new(couch_dbs, [set, public, named_table, {keypos, #entry.name}]),
    ets:new(couch_dbs_pid_to_name, [set, public, named_table]),
    couch_lru:new().

teardown(_) ->
    ets:delete(couch_dbs),
    ets:delete(couch_dbs_pid_to_name),
    (catch meck:unload(couch_db)).

new_test_() ->
    {setup,
        fun() -> couch_lru:new() end,
        fun(Lru) ->
            ?_assertMatch({0, _, _}, Lru)
        end
    }.

insert_test_() ->
    {setup,
        fun() -> couch_lru:new() end,
        fun(Lru) ->
            Key = <<"test">>,
            {1, Updates, Dbs} = couch_lru:insert(Key, Lru),
            [
                ?_assertEqual(1, ets_size(Dbs)),
                ?_assert(ets:member(Dbs, Key)),
                ?_assertEqual(1, ets_size(Updates)),
                ?_assert(ets:member(Updates, {0, Key}))
            ]
        end
    }.

insert_same_test_() ->
    {setup,
        fun() -> couch_lru:new() end,
        fun(Lru) ->
            Key = <<"test">>,
            Lru1 = {1, Updates, Dbs} = couch_lru:insert(Key, Lru),
            {2, Updates, Dbs} = couch_lru:insert(Key, Lru1),
            [
                ?_assertEqual(1, ets_size(Dbs)),
                ?_assert(ets:member(Dbs, Key)),
                ?_assertEqual(1, ets_size(Updates)),
                ?_assert(ets:member(Updates, {1, Key}))
            ]
        end
    }.

update_test_() ->
    {setup,
        fun() -> couch_lru:new() end,
        fun(Lru) ->
            Key = <<"test">>,
            Lru1 = {1, Updates, Dbs} = couch_lru:update(Key, Lru),
            {2, Updates, Dbs} = couch_lru:update(Key, Lru1),
            [
                ?_assertEqual(1, ets_size(Dbs)),
                ?_assert(ets:member(Dbs, Key)),
                ?_assertEqual(1, ets_size(Updates)),
                ?_assert(ets:member(Updates, {1, Key}))
            ]
        end
    }.

close_test_() ->
    {setup,
        fun setup/0,
        fun teardown/1,
        fun(Lru) ->
            ok = meck:expect(couch_db, is_idle, 1, true),
            {ok, Lru1} = add_record(Lru, <<"test1">>, c:pid(0, 1001, 0)),
            {ok, Lru2} = add_record(Lru1, <<"test2">>, c:pid(0, 2001, 0)),
            {true, {2, Updates, Dbs}} = couch_lru:close(Lru2),
            [
                ?_assertEqual(1, ets_size(Dbs)),
                ?_assert(ets:member(Dbs, <<"test2">>)),
                ?_assertEqual(1, ets_size(Updates)),
                ?_assert(ets:member(Updates, {1, <<"test2">>}))
            ]
        end
    }.

add_record(Lru, Key, Pid) ->
    true = ets:insert(couch_dbs, #entry{name = Key, pid = Pid}),
    true = ets:insert(couch_dbs_pid_to_name, {Pid, Key}),
    {ok, couch_lru:insert(Key, Lru)}.

ets_size(Ets) ->
    proplists:get_value(size, ets:info(Ets)).
