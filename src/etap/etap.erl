%% Copyright (c) 2008-2009 Nick Gerakines <nick@gerakines.net>
%%
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
%%
%% @author Nick Gerakines <nick@gerakines.net> [http://socklabs.com/]
%% @author Jeremy Wall <jeremy@marzhillstudios.com>
%% @version 0.3.4
%% @copyright 2007-2008 Jeremy Wall, 2008-2009 Nick Gerakines
%% @reference http://testanything.org/wiki/index.php/Main_Page
%% @reference http://en.wikipedia.org/wiki/Test_Anything_Protocol
%% @todo Finish implementing the skip directive.
%% @todo Document the messages handled by this receive loop.
%% @todo Explain in documentation why we use a process to handle test input.
%% @doc etap is a TAP testing module for Erlang components and applications.
%% This module allows developers to test their software using the TAP method.
%%
%% <blockquote cite="http://en.wikipedia.org/wiki/Test_Anything_Protocol"><p>
%% TAP, the Test Anything Protocol, is a simple text-based interface between
%% testing modules in a test harness. TAP started life as part of the test
%% harness for Perl but now has implementations in C/C++, Python, PHP, Perl
%% and probably others by the time you read this.
%% </p></blockquote>
%%
%% The testing process begins by defining a plan using etap:plan/1, running
%% a number of etap tests and then calling eta:end_tests/0. Please refer to
%% the Erlang modules in the t directory of this project for example tests.
-module(etap).
-export([
    ensure_test_server/0, start_etap_server/0, test_server/1,
    diag/1, diag/2, plan/1, end_tests/0, not_ok/2, ok/2, is/3, isnt/3,
    any/3, none/3, fun_is/3, is_greater/3, skip/1, skip/2,
    ensure_coverage_starts/0, ensure_coverage_ends/0, coverage_report/0,
    datetime/1, skip/3, bail/0, bail/1
]).
-record(test_state, {planned = 0, count = 0, pass = 0, fail = 0, skip = 0, skip_reason = ""}).
-vsn("0.3.4").

%% @spec plan(N) -> Result
%%       N = unknown | skip | {skip, string()} | integer()
%%       Result = ok
%% @doc Create a test plan and boot strap the test server.
plan(unknown) ->
    ensure_coverage_starts(),
    ensure_test_server(),
    etap_server ! {self(), plan, unknown},
    ok;
plan(skip) ->
    io:format("1..0 # skip~n");
plan({skip, Reason}) ->
    io:format("1..0 # skip ~s~n", [Reason]);
plan(N) when is_integer(N), N > 0 ->
    ensure_coverage_starts(),
    ensure_test_server(),
    etap_server ! {self(), plan, N},
    ok.

%% @spec end_tests() -> ok
%% @doc End the current test plan and output test results.
%% @todo This should probably be done in the test_server process.
end_tests() ->
    timer:sleep(100),
    ensure_coverage_ends(),
    etap_server ! {self(), state},
    State = receive X -> X end,
    if
        State#test_state.planned == -1 ->
            io:format("1..~p~n", [State#test_state.count]);
        true ->
            ok
    end,
    case whereis(etap_server) of
        undefined -> ok;
        _ -> etap_server ! done, ok
    end.

%% @private
ensure_coverage_starts() ->
    case os:getenv("COVER") of
        false -> ok;
        _ ->
            BeamDir = case os:getenv("COVER_BIN") of false -> "ebin"; X -> X end,
            cover:compile_beam_directory(BeamDir)
    end.

%% @private
%% @doc Attempts to write out any collected coverage data to the cover/
%% directory. This function should not be called externally, but it could be.
ensure_coverage_ends() ->
    case os:getenv("COVER") of
        false -> ok;
        _ ->
            filelib:ensure_dir("cover/"),
            Name = lists:flatten([
                io_lib:format("~.16b", [X]) || X <- binary_to_list(erlang:md5(
                     term_to_binary({make_ref(), now()})
                ))
            ]),
            cover:export("cover/" ++ Name ++ ".coverdata")
    end.

%% @spec coverage_report() -> ok
%% @doc Use the cover module's covreage report builder to create code coverage
%% reports from recently created coverdata files.
coverage_report() ->
    [cover:import(File) || File <- filelib:wildcard("cover/*.coverdata")],
    lists:foreach(
        fun(Mod) ->
            cover:analyse_to_file(Mod, atom_to_list(Mod) ++ "_coverage.txt", [])
        end,
        cover:imported_modules()
    ),
    ok.

bail() ->
    bail("").

bail(Reason) ->
    etap_server ! {self(), diag, "Bail out! " ++ Reason},
    ensure_coverage_ends(),
    etap_server ! done, ok,
    ok.


%% @spec diag(S) -> ok
%%       S = string()
%% @doc Print a debug/status message related to the test suite.
diag(S) -> etap_server ! {self(), diag, "# " ++ S}, ok.

%% @spec diag(Format, Data) -> ok
%%      Format = atom() | string() | binary()
%%      Data = [term()]
%%      UnicodeList = [Unicode]
%%      Unicode = int()
%% @doc Print a debug/status message related to the test suite.
%% Function arguments are passed through io_lib:format/2.
diag(Format, Data) -> diag(io_lib:format(Format, Data)).

%% @spec ok(Expr, Desc) -> Result
%%       Expr = true | false
%%       Desc = string()
%%       Result = true | false
%% @doc Assert that a statement is true.
ok(Expr, Desc) -> mk_tap(Expr == true, Desc).

%% @spec not_ok(Expr, Desc) -> Result
%%       Expr = true | false
%%       Desc = string()
%%       Result = true | false
%% @doc Assert that a statement is false.
not_ok(Expr, Desc) -> mk_tap(Expr == false, Desc).

%% @spec is(Got, Expected, Desc) -> Result
%%       Got = any()
%%       Expected = any()
%%       Desc = string()
%%       Result = true | false
%% @doc Assert that two values are the same.
is(Got, Expected, Desc) ->
    case mk_tap(Got == Expected, Desc) of
        false ->
            etap_server ! {self(), diag, "    ---"},
            etap_server ! {self(), diag, io_lib:format("    description: ~p", [Desc])},
            etap_server ! {self(), diag, io_lib:format("    found:       ~p", [Got])},
            etap_server ! {self(), diag, io_lib:format("    wanted:      ~p", [Expected])},
            etap_server ! {self(), diag, "    ..."},
            false;
        true -> true
    end.

%% @spec isnt(Got, Expected, Desc) -> Result
%%       Got = any()
%%       Expected = any()
%%       Desc = string()
%%       Result = true | false
%% @doc Assert that two values are not the same.
isnt(Got, Expected, Desc) -> mk_tap(Got /= Expected, Desc).

%% @spec is_greater(ValueA, ValueB, Desc) -> Result
%%       ValueA = number()
%%       ValueB = number()
%%       Desc = string()
%%       Result = true | false
%% @doc Assert that an integer is greater than another.
is_greater(ValueA, ValueB, Desc) when is_integer(ValueA), is_integer(ValueB) ->
    mk_tap(ValueA > ValueB, Desc).

%% @spec any(Got, Items, Desc) -> Result
%%       Got = any()
%%       Items = [any()]
%%       Desc = string()
%%       Result = true | false
%% @doc Assert that an item is in a list.
any(Got, Items, Desc) ->
    is(lists:member(Got, Items), true, Desc).

%% @spec none(Got, Items, Desc) -> Result
%%       Got = any()
%%       Items = [any()]
%%       Desc = string()
%%       Result = true | false
%% @doc Assert that an item is not in a list.
none(Got, Items, Desc) ->
    is(lists:member(Got, Items), false, Desc).

%% @spec fun_is(Fun, Expected, Desc) -> Result
%%       Fun = function()
%%       Expected = any()
%%       Desc = string()
%%       Result = true | false
%% @doc Use an anonymous function to assert a pattern match.
fun_is(Fun, Expected, Desc) when is_function(Fun) ->
    is(Fun(Expected), true, Desc).

%% @equiv skip(TestFun, "")
skip(TestFun) when is_function(TestFun) ->
    skip(TestFun, "").

%% @spec skip(TestFun, Reason) -> ok
%%       TestFun = function()
%%       Reason = string()
%% @doc Skip a test.
skip(TestFun, Reason) when is_function(TestFun), is_list(Reason) ->
    begin_skip(Reason),
    catch TestFun(),
    end_skip(),
    ok.

%% @spec skip(Q, TestFun, Reason) -> ok
%%       Q = true | false | function()
%%       TestFun = function()
%%       Reason = string()
%% @doc Skips a test conditionally. The first argument to this function can
%% either be the 'true' or 'false' atoms or a function that returns 'true' or
%% 'false'.
skip(QFun, TestFun, Reason) when is_function(QFun), is_function(TestFun), is_list(Reason) ->
    case QFun() of
        true -> begin_skip(Reason), TestFun(), end_skip();
        _ -> TestFun()
    end,
    ok;

skip(Q, TestFun, Reason) when is_function(TestFun), is_list(Reason), Q == true ->
    begin_skip(Reason),
    TestFun(),
    end_skip(),
    ok;

skip(_, TestFun, Reason) when is_function(TestFun), is_list(Reason) ->
    TestFun(),
    ok.

%% @private
begin_skip(Reason) ->
    etap_server ! {self(), begin_skip, Reason}.

%% @private
end_skip() ->
    etap_server ! {self(), end_skip}.

% ---
% Internal / Private functions

%% @private
%% @doc Start the etap_server process if it is not running already.
ensure_test_server() ->
    case whereis(etap_server) of
        undefined ->
            proc_lib:start(?MODULE, start_etap_server,[]);
        _ ->
            diag("The test server is already running.")
    end.

%% @private
%% @doc Start the etap_server loop and register itself as the etap_server
%% process.
start_etap_server() ->
    catch register(etap_server, self()),
    proc_lib:init_ack(ok),
    etap:test_server(#test_state{
        planned = 0,
        count = 0,
        pass = 0,
        fail = 0,
        skip = 0,
        skip_reason = ""
    }).


%% @private
%% @doc The main etap_server receive/run loop. The etap_server receive loop
%% responds to seven messages apperatining to failure or passing of tests.
%% It is also used to initiate the testing process with the {_, plan, _}
%% message that clears the current test state.
test_server(State) ->
    NewState = receive
        {_From, plan, unknown} ->
            io:format("# Current time local ~s~n", [datetime(erlang:localtime())]),
            io:format("# Using etap version ~p~n", [ proplists:get_value(vsn, proplists:get_value(attributes, etap:module_info())) ]),
            State#test_state{
                planned = -1,
                count = 0,
                pass = 0,
                fail = 0,
                skip = 0,
                skip_reason = ""
            };
        {_From, plan, N} ->
            io:format("# Current time local ~s~n", [datetime(erlang:localtime())]),
            io:format("# Using etap version ~p~n", [ proplists:get_value(vsn, proplists:get_value(attributes, etap:module_info())) ]),
            io:format("1..~p~n", [N]),
            State#test_state{
                planned = N,
                count = 0,
                pass = 0,
                fail = 0,
                skip = 0,
                skip_reason = ""
            };
        {_From, begin_skip, Reason} ->
            State#test_state{
                skip = 1,
                skip_reason = Reason
            };
        {_From, end_skip} ->
            State#test_state{
                skip = 0,
                skip_reason = ""
            };
        {_From, pass, Desc} ->
            FullMessage = skip_diag(
                " - " ++ Desc,
                State#test_state.skip,
                State#test_state.skip_reason
            ),
            io:format("ok ~p ~s~n", [State#test_state.count + 1, FullMessage]),
            State#test_state{
                count = State#test_state.count + 1,
                pass = State#test_state.pass + 1
            };

        {_From, fail, Desc} ->
            FullMessage = skip_diag(
                " - " ++ Desc,
                State#test_state.skip,
                State#test_state.skip_reason
            ),
            io:format("not ok ~p ~s~n", [State#test_state.count + 1, FullMessage]),
            State#test_state{
                count = State#test_state.count + 1,
                fail = State#test_state.fail + 1
            };
        {From, state} ->
            From ! State,
            State;
        {_From, diag, Message} ->
            io:format("~s~n", [Message]),
            State;
        {From, count} ->
            From ! State#test_state.count,
            State;
        {From, is_skip} ->
            From ! State#test_state.skip,
            State;
        done ->
            exit(normal)
    end,
    test_server(NewState).

%% @private
%% @doc Process the result of a test and send it to the etap_server process.
mk_tap(Result, Desc) ->
    IsSkip = lib:sendw(etap_server, is_skip),
    case [IsSkip, Result] of
        [_, true] ->
            etap_server ! {self(), pass, Desc},
            true;
        [1, _] ->
            etap_server ! {self(), pass, Desc},
            true;
        _ ->
            etap_server ! {self(), fail, Desc},
            false
    end.

%% @private
%% @doc Format a date/time string.
datetime(DateTime) ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = DateTime,
    io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B", [Year, Month, Day, Hour, Min, Sec]).

%% @private
%% @doc Craft an output message taking skip/todo into consideration.
skip_diag(Message, 0, _) ->
    Message;
skip_diag(_Message, 1, "") ->
    " # SKIP";
skip_diag(_Message, 1, Reason) ->
    " # SKIP : " ++ Reason.
