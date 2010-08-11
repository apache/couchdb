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
%% @doc Provides test functionality against a specific web request. Many of
%% the exported methods can be used to build your own more complex tests.
-module(etap_request, [Method, Url, InHeaders, InBody, Status, OutHeaders, OutBody]).

-export([status_is/2]).

-export([
    method/0, url/0, status/0, status_code/0, status_line/0, rheaders/0,
    has_rheader/1, rheader/1, rbody/0, header_is/3, body_is/2,
    body_has_string/2
]).

% ---
% Tests

%% @doc Assert that response status code is the given status code.
status_is(Code, Desc) ->
    etap:is(status_code(), Code, Desc).

header_is(Name, Value, Desc) ->
    etap:is(rheader(Name), Value, Desc).

body_is(Value, Desc) ->
    etap:is(rbody(), Value, Desc).

body_has_string(String, Desc) when is_list(OutBody), is_list(String) ->
    etap_string:contains_ok(OutBody, String, Desc).

% ---
% Accessor functions

%% @doc Access a request's method.
method() -> Method.

%% @doc Access a request's URL.
url() -> Url.

%% @doc Access a request's status.
status() -> Status.

%% @doc Access a request's status code.
status_code() ->
    {_, Code, _} = Status,
    Code.

%% @doc Access a request's status line.
status_line() ->
    {_, _, Line} = Status,
    Line.

%% @doc Access a request's headers.
rheaders() -> OutHeaders.

%% @doc Dertermine if a specific request header exists.
has_rheader(Key) ->
    lists:keymember(Key, 1, OutHeaders).

%% @doc Return a specific request header.
rheader(Key) ->
    case lists:keysearch(Key, 1, OutHeaders) of
        false -> undefined;
        {value, {Key, Value}} -> Value
    end.

%% @doc Access the request's body.
rbody() -> OutBody.
