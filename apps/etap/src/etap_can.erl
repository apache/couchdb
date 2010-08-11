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
%% @doc Provide test functionality modules
-module(etap_can).

-export([
    loaded_ok/2, can_ok/2, can_ok/3,
    has_attrib/2, is_attrib/3, is_behaviour/2
]).

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
