%% @author Bob Ippolito <bob@mochimedia.com>
%% @copyright 2007 Mochi Media, Inc.

%% @doc Response abstraction.

-module(mochiweb_response).
-author('bob@mochimedia.com').

-define(QUIP, "Any of you quaids got a smint?").

-export([new/3, get_header_value/2, get/2, dump/1]).
-export([send/2, write_chunk/2]).

%% @type response() = {atom(), [Request, Code, Headers]}

%% @spec new(Request, Code, Headers) -> response()
%% @doc Create a new mochiweb_response instance.
new(Request, Code, Headers) ->
    {?MODULE, [Request, Code, Headers]}.

%% @spec get_header_value(string() | atom() | binary(), response()) ->
%%           string() | undefined
%% @doc Get the value of the given response header.
get_header_value(K, {?MODULE, [_Request, _Code, Headers]}) ->
    mochiweb_headers:get_value(K, Headers).

%% @spec get(request | code | headers, response()) -> term()
%% @doc Return the internal representation of the given field.
get(request, {?MODULE, [Request, _Code, _Headers]}) ->
    Request;
get(code, {?MODULE, [_Request, Code, _Headers]}) ->
    Code;
get(headers, {?MODULE, [_Request, _Code, Headers]}) ->
    Headers.

%% @spec dump(response()) -> {mochiweb_request, [{atom(), term()}]}
%% @doc Dump the internal representation to a "human readable" set of terms
%%      for debugging/inspection purposes.
dump({?MODULE, [Request, Code, Headers]}) ->
    [{request, Request:dump()},
     {code, Code},
     {headers, mochiweb_headers:to_list(Headers)}].

%% @spec send(iodata(), response()) -> ok
%% @doc Send data over the socket if the method is not HEAD.
send(Data, {?MODULE, [Request, _Code, _Headers]}) ->
    case Request:get(method) of
        'HEAD' ->
            ok;
        _ ->
            Request:send(Data)
    end.

%% @spec write_chunk(iodata(), response()) -> ok
%% @doc Write a chunk of a HTTP chunked response. If Data is zero length,
%%      then the chunked response will be finished.
write_chunk(Data, {?MODULE, [Request, _Code, _Headers]}=THIS) ->
    case Request:get(version) of
        Version when Version >= {1, 1} ->
            Length = iolist_size(Data),
            send([io_lib:format("~.16b\r\n", [Length]), Data, <<"\r\n">>], THIS);
        _ ->
            send(Data, THIS)
    end.


%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
