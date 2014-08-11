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

-module(couch_ref_counter_tests).

-include("couch_eunit.hrl").
-include_lib("couchdb/couch_db.hrl").

-define(TIMEOUT, 1000).


setup() ->
    {ok, RefCtr} = couch_ref_counter:start([]),
    ChildPid = spawn(fun() -> loop() end),
    {RefCtr, ChildPid}.

teardown({_, ChildPid}) ->
    erlang:monitor(process, ChildPid),
    ChildPid ! close,
    wait().


couch_ref_counter_test_() ->
    {
        "CouchDB reference counter tests",
        {
            foreach,
            fun setup/0, fun teardown/1,
            [
                fun should_initialize_with_calling_process_as_referrer/1,
                fun should_ignore_unknown_pid/1,
                fun should_increment_counter_on_pid_add/1,
                fun should_not_increase_counter_on_readding_same_pid/1,
                fun should_drop_ref_for_double_added_pid/1,
                fun should_decrement_counter_on_pid_drop/1,
                fun should_add_after_drop/1,
                fun should_decrement_counter_on_process_exit/1

            ]
        }
    }.


should_initialize_with_calling_process_as_referrer({RefCtr, _}) ->
    ?_assertEqual(1, couch_ref_counter:count(RefCtr)).

should_ignore_unknown_pid({RefCtr, ChildPid}) ->
    ?_assertEqual(ok, couch_ref_counter:drop(RefCtr, ChildPid)).

should_increment_counter_on_pid_add({RefCtr, ChildPid}) ->
    couch_ref_counter:add(RefCtr, ChildPid),
    ?_assertEqual(2, couch_ref_counter:count(RefCtr)).

should_not_increase_counter_on_readding_same_pid({RefCtr, ChildPid}) ->
    couch_ref_counter:add(RefCtr, ChildPid),
    couch_ref_counter:add(RefCtr, ChildPid),
    ?_assertEqual(2, couch_ref_counter:count(RefCtr)).

should_drop_ref_for_double_added_pid({RefCtr, ChildPid}) ->
    couch_ref_counter:add(RefCtr, ChildPid),
    couch_ref_counter:add(RefCtr, ChildPid),
    couch_ref_counter:drop(RefCtr, ChildPid),
    ?_assertEqual(2, couch_ref_counter:count(RefCtr)).

should_decrement_counter_on_pid_drop({RefCtr, ChildPid}) ->
    couch_ref_counter:add(RefCtr, ChildPid),
    couch_ref_counter:drop(RefCtr, ChildPid),
    ?_assertEqual(1, couch_ref_counter:count(RefCtr)).

should_add_after_drop({RefCtr, ChildPid}) ->
    couch_ref_counter:add(RefCtr, ChildPid),
    couch_ref_counter:drop(RefCtr, ChildPid),
    couch_ref_counter:add(RefCtr, ChildPid),
    ?_assertEqual(2, couch_ref_counter:count(RefCtr)).

should_decrement_counter_on_process_exit({RefCtr, ChildPid}) ->
    ?_assertEqual(1,
        begin
            couch_ref_counter:add(RefCtr, ChildPid),
            erlang:monitor(process, ChildPid),
            ChildPid ! close,
            wait(),
            couch_ref_counter:count(RefCtr)
        end).


loop() ->
    receive
        close -> ok
    end.

wait() ->
    receive
        {'DOWN', _, _, _, _} ->
            ok
    after ?TIMEOUT ->
        throw(timeout_error)
    end.
