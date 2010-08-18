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

main(_) ->
    test_util:init_code_path(),
    etap:plan(8),
    case (catch test()) of
        ok ->
            etap:end_tests();
        Other ->
            etap:diag(io_lib:format("Test died abnormally: ~p", [Other])),
            etap:bail(Other)
    end,
    ok.

loop() ->
    receive
        close -> ok
    end.

wait() ->
    receive
        {'DOWN', _, _, _, _} -> ok
    after 1000 ->
        throw(timeout_error)
    end.

test() ->
    {ok, RefCtr} = couch_ref_counter:start([]),

    etap:is(
        couch_ref_counter:count(RefCtr),
        1,
        "A ref_counter is initialized with the calling process as a referer."
    ),

    ChildPid1 = spawn(fun() -> loop() end),

    % This is largely implicit in that nothing else breaks
    % as ok is just returned from gen_server:cast()
    etap:is(
        couch_ref_counter:drop(RefCtr, ChildPid1),
        ok,
        "Dropping an unknown Pid is ignored."
    ),

    couch_ref_counter:add(RefCtr, ChildPid1),
    etap:is(
        couch_ref_counter:count(RefCtr),
        2,
        "Adding a Pid to the ref_counter increases it's count."
    ),

    couch_ref_counter:add(RefCtr, ChildPid1),
    etap:is(
        couch_ref_counter:count(RefCtr),
        2,
        "Readding the same Pid maintains the count but increments it's refs."
    ),

    couch_ref_counter:drop(RefCtr, ChildPid1),
    etap:is(
        couch_ref_counter:count(RefCtr),
        2,
        "Droping the doubly added Pid only removes a ref, not a referer."
    ),

    couch_ref_counter:drop(RefCtr, ChildPid1),
    etap:is(
        couch_ref_counter:count(RefCtr),
        1,
        "Dropping the second ref drops the referer."
    ),

    couch_ref_counter:add(RefCtr, ChildPid1),
    etap:is(
        couch_ref_counter:count(RefCtr),
        2,
        "Sanity checking that the Pid was re-added."
    ),

    erlang:monitor(process, ChildPid1),
    ChildPid1 ! close,
    wait(),
    
    CheckFun = fun
        (Iter, nil) ->
            case couch_ref_counter:count(RefCtr) of
                1 -> Iter;
                _ -> nil
            end;
        (_, Acc) ->
            Acc
    end,
    Result = lists:foldl(CheckFun, nil, lists:seq(1, 10000)),
    etap:isnt(
        Result,
        nil,
        "The referer count was decremented automatically on process exit."
    ),

    ok.
