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
