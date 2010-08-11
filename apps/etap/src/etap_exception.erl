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
%% @reference http://testanything.org/wiki/index.php/Main_Page
%% @reference http://en.wikipedia.org/wiki/Test_Anything_Protocol
%% @doc Adds exception based testing to the etap suite.
-module(etap_exception).

-export([dies_ok/2, lives_ok/2, throws_ok/3]).

% ---
% External / Public functions

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

% ---
% Internal / Private functions

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
