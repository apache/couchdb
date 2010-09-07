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
%% @todo Support cookies.
%% @doc Provide testing functionality for web requests.
-module(etap_web).

-export([simple_200/2, simple_404/2, build_request/4]).

%% @doc Fetch a url and verify that it returned a 200 status.
simple_200(Url, Desc) ->
    Request = build_request(get, Url, [], []),
    Request:status_is(200, Desc).

%% @doc Fetch a url and verify that it returned a 404 status.
simple_404(Url, Desc) ->
    Request = build_request(get, Url, [], []),
    Request:status_is(404, Desc).

%% @doc Create and return a request structure.
build_request(Method, Url, Headers, Body) 
 when Method==options;Method==get;Method==head;Method==delete;Method==trace ->
     try http:request(Method, {Url, Headers}, [{autoredirect, false}], []) of
        {ok, {OutStatus, OutHeaders, OutBody}} ->
            etap_request:new(Method, Url, Headers, Body, OutStatus, OutHeaders, OutBody);
        _ -> error
    catch
        _:_ -> error
    end;

%% @doc Create and return a request structure.
build_request(Method, Url, Headers, Body) when Method == post; Method == put ->
    ContentType = case lists:keysearch("Content-Type", 1, Headers) of
        {value, {"Content-Type", X}} -> X;
        _ -> []
    end,
    try http:request(Method, {Url, Headers, ContentType, Body}, [{autoredirect, false}], []) of
        {ok, {OutStatus, OutHeaders, OutBody}} ->
            etap_request:new(Method, Url, Headers, Body, OutStatus, OutHeaders, OutBody);
        _ -> error
    catch
        _:_ -> error
    end.
