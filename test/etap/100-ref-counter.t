#!/usr/bin/env escript
%% -*- erlang -*-

main(_) ->
    code:add_pathz("src/couchdb"),
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
    {ping, From} ->
        From ! pong
    end.

wait() ->
    receive
        _ ->
            ok
    after
        1000 ->
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

    ChildPid1 ! {ping, self()},
    wait(),
    etap:is(
        couch_ref_counter:count(RefCtr),
        1,
        "The referer count was decremented automatically on process exit."
    ),

    ok.
