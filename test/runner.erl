-module(runner).

-export([run/0]).
-include("couch_config_test.erl").
-include("couch_config_writer_test.erl").

%% Test Runner
run() ->
    % mochiweb tests
    % case mochiweb:test() of
    %     ok ->
    %         io:format("Mochiweb Tests PASSED~n");
    %     _ ->
    %         io:format("Mochiweb Tests FAILED~n")
    % end,

    % CouchDB tests
    Tests = lists:flatten([
        couch_config_test(),
        couch_config_writer_test()
    ]),
    run_tests(Tests),

    % we're done, get out of here
    halt().

run_test(TestFun) ->
    io:format("  ~s        ", [proplists:get_value(
                                 name,
                                 erlang:fun_info(TestFun))]),
    try TestFun() of
        _ ->
            io:format("[PASSED]~n", []),
            passed
    catch
        _:{skipped, Reason} ->
            io:format("[SKIPPED]~n", []),
            io:format("      reason: ~s~n", [Reason]),
            skipped;
        _:X ->
            io:format("[FAILED]~n", []),
            io:format("ERROR: ~n======~n~p ~n======~n~n",
                      [{X, erlang:get_stacktrace()}]),
            failed
    end.

run_tests(List) ->
    io:format("Running ~p tests...~n", [lists:flatlength(List)]),

    Results = lists:map(fun run_test/1, List),

    Passed = lists:filter(
               fun (Result) -> Result =:= passed end,
               Results),

    Failed = lists:filter(
               fun (Result) -> Result =:= failed end,
               Results),

    Skipped = lists:filter(
                fun(Result) -> Result =:= skipped end,
                Results),

    io:format("PASSED: ~p, FAILED: ~p, SKIPPED: ~p ~n",
              [lists:flatlength(Passed),
               lists:flatlength(Failed),
               lists:flatlength(Skipped)]).
