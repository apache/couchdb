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
%% @copyright 2008 Nick Gerakines
%% @doc Provide testing functionality for strings.
-module(etap_string).

-export([contains_ok/3, is_before/4]).

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
