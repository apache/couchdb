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
-vsn("0.3.4").

-export([
    ensure_test_server/0,
    start_etap_server/0,
    test_server/1,
    msg/1, msg/2,
    diag/1, diag/2,
    expectation_mismatch_message/3,
    plan/1,
    end_tests/0,
    not_ok/2, ok/2, is_ok/2, is/3, isnt/3, any/3, none/3,
    fun_is/3, expect_fun/3, expect_fun/4,
    is_greater/3,
    skip/1, skip/2,
    datetime/1,
    skip/3,
    bail/0, bail/1,
    test_state/0, failure_count/0
]).

-export([
    contains_ok/3,
    is_before/4
]).

-export([
    is_pid/2,
    is_alive/2,
    is_mfa/3
]).

-export([
    loaded_ok/2,
    can_ok/2, can_ok/3,
    has_attrib/2, is_attrib/3,
    is_behaviour/2
]).

-export([
    dies_ok/2,
    lives_ok/2,
    throws_ok/3
]).


-record(test_state, {
    planned = 0,
    count = 0,
    pass = 0,
    fail = 0,
    skip = 0,
    skip_reason = ""
}).

%% @spec plan(N) -> Result
%%       N = unknown | skip | {skip, string()} | integer()
%%       Result = ok
%% @doc Create a test plan and boot strap the test server.
plan(unknown) ->
    ensure_test_server(),
    etap_server ! {self(), plan, unknown},
    ok;
plan(skip) ->
    io:format("1..0 # skip~n");
plan({skip, Reason}) ->
    io:format("1..0 # skip ~s~n", [Reason]);
plan(N) when is_integer(N), N > 0 ->
    ensure_test_server(),
    etap_server ! {self(), plan, N},
    ok.

%% @spec end_tests() -> ok
%% @doc End the current test plan and output test results.
%% @todo This should probably be done in the test_server process.
end_tests() ->
    case whereis(etap_server) of
        undefined -> self() ! true;
        _ -> etap_server ! {self(), state}
    end,
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

bail() ->
    bail("").

bail(Reason) ->
    etap_server ! {self(), diag, "Bail out! " ++ Reason},
    etap_server ! done, ok,
    ok.

%% @spec test_state() -> Return
%%       Return = test_state_record() | {error, string()}
%% @doc Return the current test state
test_state() ->
    etap_server ! {self(), state},
    receive
	X when is_record(X, test_state) -> X
    after
	1000 -> {error, "Timed out waiting for etap server reply.~n"}
    end.

%% @spec failure_count() -> Return
%%       Return = integer() | {error, string()}
%% @doc Return the current failure count
failure_count() ->
    case test_state() of
        #test_state{fail=FailureCount} -> FailureCount;
        X -> X
    end.

%% @spec msg(S) -> ok
%%       S = string()
%% @doc Print a message in the test output.
msg(S) -> etap_server ! {self(), diag, S}, ok.

%% @spec msg(Format, Data) -> ok
%%      Format = atom() | string() | binary()
%%      Data = [term()]
%%      UnicodeList = [Unicode]
%%      Unicode = int()
%% @doc Print a message in the test output.
%% Function arguments are passed through io_lib:format/2.
msg(Format, Data) -> msg(io_lib:format(Format, Data)).

%% @spec diag(S) -> ok
%%       S = string()
%% @doc Print a debug/status message related to the test suite.
diag(S) -> msg("# " ++ S).

%% @spec diag(Format, Data) -> ok
%%      Format = atom() | string() | binary()
%%      Data = [term()]
%%      UnicodeList = [Unicode]
%%      Unicode = int()
%% @doc Print a debug/status message related to the test suite.
%% Function arguments are passed through io_lib:format/2.
diag(Format, Data) -> diag(io_lib:format(Format, Data)).

%% @spec expectation_mismatch_message(Got, Expected, Desc) -> ok
%%       Got = any()
%%       Expected = any()
%%       Desc = string()
%% @doc Print an expectation mismatch message in the test output.
expectation_mismatch_message(Got, Expected, Desc) ->
    msg("    ---"),
    msg("    description: ~p", [Desc]),
    msg("    found:       ~p", [Got]),
    msg("    wanted:      ~p", [Expected]),
    msg("    ..."),
    ok.

% @spec evaluate(Pass, Got, Expected, Desc) -> Result
%%       Pass = true | false
%%       Got = any()
%%       Expected = any()
%%       Desc = string()
%%       Result = true | false
%% @doc Evaluate a test statement, printing an expectation mismatch message
%%       if the test failed.
evaluate(Pass, Got, Expected, Desc) ->
    case mk_tap(Pass, Desc) of
        false ->
            expectation_mismatch_message(Got, Expected, Desc),
            false;
        true ->
            true
    end.

%% @spec ok(Expr, Desc) -> Result
%%       Expr = true | false
%%       Desc = string()
%%       Result = true | false
%% @doc Assert that a statement is true.
ok(Expr, Desc) -> evaluate(Expr == true, Expr, true, Desc).

%% @spec not_ok(Expr, Desc) -> Result
%%       Expr = true | false
%%       Desc = string()
%%       Result = true | false
%% @doc Assert that a statement is false.
not_ok(Expr, Desc) -> evaluate(Expr == false, Expr, false, Desc).

%% @spec is_ok(Expr, Desc) -> Result
%%       Expr = any()
%%       Desc = string()
%%       Result = true | false
%% @doc Assert that two values are the same.
is_ok(Expr, Desc) -> evaluate(Expr == ok, Expr, ok, Desc).

%% @spec is(Got, Expected, Desc) -> Result
%%       Got = any()
%%       Expected = any()
%%       Desc = string()
%%       Result = true | false
%% @doc Assert that two values are the same.
is(Got, Expected, Desc) -> evaluate(Got == Expected, Got, Expected, Desc).

%% @spec isnt(Got, Expected, Desc) -> Result
%%       Got = any()
%%       Expected = any()
%%       Desc = string()
%%       Result = true | false
%% @doc Assert that two values are not the same.
isnt(Got, Expected, Desc) -> evaluate(Got /= Expected, Got, Expected, Desc).

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
any(Got, Items, Desc) when is_function(Got) ->
    is(lists:any(Got, Items), true, Desc);
any(Got, Items, Desc) ->
    is(lists:member(Got, Items), true, Desc).

%% @spec none(Got, Items, Desc) -> Result
%%       Got = any()
%%       Items = [any()]
%%       Desc = string()
%%       Result = true | false
%% @doc Assert that an item is not in a list.
none(Got, Items, Desc) when is_function(Got) ->
    is(lists:any(Got, Items), false, Desc);
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

%% @spec expect_fun(ExpectFun, Got, Desc) -> Result
%%       ExpectFun = function()
%%       Got = any()
%%       Desc = string()
%%       Result = true | false
%% @doc Use an anonymous function to assert a pattern match, using actual
%%       value as the argument to the function.
expect_fun(ExpectFun, Got, Desc) ->
    evaluate(ExpectFun(Got), Got, ExpectFun, Desc).

%% @spec expect_fun(ExpectFun, Got, Desc, ExpectStr) -> Result
%%       ExpectFun = function()
%%       Got = any()
%%       Desc = string()
%%       ExpectStr = string()
%%       Result = true | false
%% @doc Use an anonymous function to assert a pattern match, using actual
%%       value as the argument to the function.
expect_fun(ExpectFun, Got, Desc, ExpectStr) ->
    evaluate(ExpectFun(Got), Got, ExpectStr, Desc).

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

%% @spec contains_ok(string(), string(), string()) -> true | false
%% @doc Assert that a string is contained in another string.
contains_ok(Source, String, Desc) ->
    etap:isnt(
        string:str(Source, String),
        0,
        Desc
    ).

%% @spec is_before(string(), string(), string(), string()) -> true | false
%% @doc Assert that a string comes before another string within a larger body.
is_before(Source, StringA, StringB, Desc) ->
    etap:is_greater(
        string:str(Source, StringB),
        string:str(Source, StringA),
        Desc
    ).

%% @doc Assert that a given variable is a pid.
is_pid(Pid, Desc) when is_pid(Pid) -> etap:ok(true, Desc);
is_pid(_, Desc) -> etap:ok(false, Desc).

%% @doc Assert that a given process/pid is alive.
is_alive(Pid, Desc) ->
    etap:ok(erlang:is_process_alive(Pid), Desc).

%% @doc Assert that the current function of a pid is a given {M, F, A} tuple.
is_mfa(Pid, MFA, Desc) ->
    etap:is({current_function, MFA}, erlang:process_info(Pid, current_function), Desc).

%% @spec loaded_ok(atom(), string()) -> true | false
%% @doc Assert that a module has been loaded successfully.
loaded_ok(M, Desc) when is_atom(M) ->
    etap:fun_is(fun({module, _}) -> true; (_) -> false end, code:load_file(M), Desc).

%% @spec can_ok(atom(), atom()) -> true | false
%% @doc Assert that a module exports a given function.
can_ok(M, F) when is_atom(M), is_atom(F) ->
    Matches = [X || {X, _} <- M:module_info(exports), X == F],
    etap:ok(Matches > 0, lists:concat([M, " can ", F])).

%% @spec can_ok(atom(), atom(), integer()) -> true | false
%% @doc Assert that a module exports a given function with a given arity.
can_ok(M, F, A) when is_atom(M); is_atom(F), is_number(A) ->
    Matches = [X || X <- M:module_info(exports), X == {F, A}],
    etap:ok(Matches > 0, lists:concat([M, " can ", F, "/", A])).

%% @spec has_attrib(M, A) -> true | false
%%       M = atom()
%%       A = atom()
%% @doc Asserts that a module has a given attribute.
has_attrib(M, A) when is_atom(M), is_atom(A) ->
    etap:isnt(
        proplists:get_value(A, M:module_info(attributes), 'asdlkjasdlkads'),
        'asdlkjasdlkads',
        lists:concat([M, " has attribute ", A])
    ).

%% @spec has_attrib(M, A. V) -> true | false
%%       M = atom()
%%       A = atom()
%%       V = any()
%% @doc Asserts that a module has a given attribute with a given value.
is_attrib(M, A, V) when is_atom(M) andalso is_atom(A) ->
    etap:is(
        proplists:get_value(A, M:module_info(attributes)),
        [V],
        lists:concat([M, "'s ", A, " is ", V])
    ).

%% @spec is_behavior(M, B) -> true | false
%%       M = atom()
%%       B = atom()
%% @doc Asserts that a given module has a specific behavior.
is_behaviour(M, B) when is_atom(M) andalso is_atom(B) ->
    is_attrib(M, behaviour, B).

%% @doc Assert that an exception is raised when running a given function.
dies_ok(F, Desc) ->
    case (catch F()) of
        {'EXIT', _} -> etap:ok(true, Desc);
        _ -> etap:ok(false, Desc)
    end.

%% @doc Assert that an exception is not raised when running a given function.
lives_ok(F, Desc) ->
    etap:is(try_this(F), success, Desc).

%% @doc Assert that the exception thrown by a function matches the given exception.
throws_ok(F, Exception, Desc) ->
    try F() of
        _ -> etap:ok(nok, Desc)
    catch
        _:E ->
            etap:is(E, Exception, Desc)
    end.

%% @private
%% @doc Run a function and catch any exceptions.
try_this(F) when is_function(F, 0) ->
    try F() of
        _ -> success
    catch
        throw:E -> {throw, E};
        error:E -> {error, E};
        exit:E -> {exit, E}
    end.

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
