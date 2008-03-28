%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id$

-module(httpc_response).

-include("http_internal.hrl").
-include("httpc_internal.hrl").

%% API
-export([parse/1, result/2, send/2, error/2, is_server_closing/1, 
	 stream_start/2]).

%% Callback API - used for example if the header/body is received a
%% little at a time on a socket. 
-export([parse_version/1, parse_status_code/1, parse_reason_phrase/1,
	 parse_headers/1, whole_body/1, whole_body/2]).

%%%=========================================================================
%%%  API
%%%=========================================================================

parse([Bin, MaxHeaderSize]) ->
    parse_version(Bin, [], MaxHeaderSize, []).

whole_body([Bin, Body, Length])  ->
    whole_body(<<Body/binary, Bin/binary>>, Length).

%% Functions that may be returned during the decoding process
%% if the input data is incompleate. 
parse_version([Bin, Version, MaxHeaderSize, Result]) ->
    parse_version(Bin, Version, MaxHeaderSize, Result).

parse_status_code([Bin, Code, MaxHeaderSize, Result]) ->
    parse_status_code(Bin, Code, MaxHeaderSize, Result).

parse_reason_phrase([Bin, Rest, Phrase, MaxHeaderSize, Result]) ->
    parse_reason_phrase(<<Rest/binary, Bin/binary>>, Phrase, 
			MaxHeaderSize, Result).

parse_headers([Bin, Rest,Header, Headers, MaxHeaderSize, Result]) ->
    parse_headers(<<Rest/binary, Bin/binary>>, Header, Headers, 
		  MaxHeaderSize, Result).
    
whole_body(Body, Length) ->
    case size(Body) of
	N when N < Length, N > 0  ->
	    {?MODULE, whole_body, [Body, Length]};
	%% OBS!  The Server may close the connection to indicate that the
	%% whole body is now sent instead of sending a lengh
	%% indicator.In this case the lengh indicator will be
	%% -1.
	N when N >= Length, Length >= 0 -> 
	    %% Potential trailing garbage will be thrown away in
	    %% format_response/1 Some servers may send a 100-continue
	    %% response without the client requesting it through an
	    %% expect header in this case the trailing bytes may be
	    %% part of the real response message.
	    {ok, Body};
	_ -> %% Length == -1
	    {?MODULE, whole_body, [Body, Length]} 
    end.

%%-------------------------------------------------------------------------
%% result(Response, Request) ->
%%   Response - {StatusLine, Headers, Body}
%%   Request - #request{}
%%   Session - #tcp_session{}
%%                                   
%% Description: Checks the status code ...
%%-------------------------------------------------------------------------
result(Response = {{_,200,_}, _, _}, 
       Request = #request{stream = Stream}) when Stream =/= none ->
    stream_end(Response, Request);

result(Response = {{_,100,_}, _, _}, Request) ->
    status_continue(Response, Request);

%% In redirect loop
result(Response = {{_, Code, _}, _, _}, Request =
       #request{redircount = Redirects,
		settings = #http_options{autoredirect = true}}) 
  when Code div 100 == 3, Redirects > ?HTTP_MAX_REDIRECTS ->
    transparent(Response, Request);

%% multiple choices 
result(Response = {{_, 300, _}, _, _}, 
       Request = #request{settings = 
			  #http_options{autoredirect = 
					true}}) ->
    redirect(Response, Request);

result(Response = {{_, Code, _}, _, _}, 
       Request = #request{settings = 
			  #http_options{autoredirect = true},
			  method = head}) when Code == 301;
					       Code == 302;
					       Code == 303;
					       Code == 307 ->
    redirect(Response, Request);
result(Response = {{_, Code, _}, _, _}, 
       Request = #request{settings = 
			  #http_options{autoredirect = true},
			  method = get}) when Code == 301;
					      Code == 302;
					      Code == 303;
					      Code == 307 ->
    redirect(Response, Request);


result(Response = {{_,503,_}, _, _}, Request) ->
    status_service_unavailable(Response, Request);
result(Response = {{_,Code,_}, _, _}, Request) when (Code div 100) == 5 ->
    status_server_error_50x(Response, Request);

result(Response, Request) -> 
    transparent(Response, Request).

send(To, Msg) -> 
    To ! {http, Msg}.

%%%========================================================================
%%% Internal functions
%%%========================================================================
parse_version(<<>>, Version, MaxHeaderSize, Result) ->
    {?MODULE, parse_version, [Version, MaxHeaderSize,Result]};
parse_version(<<?SP, Rest/binary>>, Version, MaxHeaderSize, Result) ->
    parse_status_code(Rest, [], MaxHeaderSize,
		      [lists:reverse(Version) | Result]);	
parse_version(<<Octet, Rest/binary>>, Version, MaxHeaderSize, Result) ->
    parse_version(Rest, [Octet | Version], MaxHeaderSize,Result).

parse_status_code(<<>>, StatusCodeStr, MaxHeaderSize, Result) -> 
    {?MODULE, parse_status_code, [StatusCodeStr, MaxHeaderSize, Result]};
parse_status_code(<<?SP, Rest/binary>>, StatusCodeStr, 
		  MaxHeaderSize, Result) ->
    parse_reason_phrase(Rest, [], MaxHeaderSize, 
			[list_to_integer(lists:reverse(StatusCodeStr)) | 
			 Result]);
parse_status_code(<<Octet, Rest/binary>>, StatusCodeStr, 
		  MaxHeaderSize,Result) ->
    parse_status_code(Rest, [Octet | StatusCodeStr], MaxHeaderSize, Result).

parse_reason_phrase(<<>>, Phrase, MaxHeaderSize, Result) ->
    {?MODULE, parse_reason_phrase, [<<>>, Phrase, MaxHeaderSize,Result]};
parse_reason_phrase(<<?CR, ?LF, Rest/binary>>, Phrase, 
		    MaxHeaderSize, Result) ->
    parse_headers(Rest, [], [], MaxHeaderSize,
		  [lists:reverse(Phrase) | Result]); 
parse_reason_phrase(<<?CR>> = Data, Phrase, MaxHeaderSize, Result) ->
    {?MODULE, parse_reason_phrase, [Data, Phrase, MaxHeaderSize,Result]};
parse_reason_phrase(<<Octet, Rest/binary>>, Phrase, MaxHeaderSize, Result) ->
    parse_reason_phrase(Rest, [Octet | Phrase], MaxHeaderSize, Result).

parse_headers(<<>>, Header, Headers, MaxHeaderSize, Result) -> 
    {?MODULE, parse_headers, [<<>>, Header, Headers, MaxHeaderSize, Result]};
parse_headers(<<?CR,?LF,?CR,?LF,Body/binary>>, Header, Headers,
	      MaxHeaderSize, Result) ->
    HTTPHeaders = [lists:reverse(Header) | Headers],
    Length = lists:foldl(fun(H, Acc) -> length(H) + Acc end,
			   0, HTTPHeaders),
    case ((Length =< MaxHeaderSize) or (MaxHeaderSize == nolimit)) of
 	true ->   
	    ResponseHeaderRcord = 
		http_response:headers(HTTPHeaders, #http_response_h{}),
	    {ok, list_to_tuple(
		   lists:reverse([Body, ResponseHeaderRcord | Result]))};
 	false ->
	    throw({error, {header_too_long, MaxHeaderSize, 
			   MaxHeaderSize-Length}})
    end;
parse_headers(<<?CR,?LF,?CR>> = Data, Header, Headers, 
	      MaxHeaderSize, Result) ->
    {?MODULE, parse_headers, [Data, Header, Headers, MaxHeaderSize, Result]};
parse_headers(<<?CR,?LF>> = Data, Header, Headers, 
	      MaxHeaderSize, Result) ->
    {?MODULE, parse_headers, [Data, Header, Headers, MaxHeaderSize, Result]};
parse_headers(<<?CR,?LF, Octet, Rest/binary>>, Header, Headers,
	      MaxHeaderSize, Result) ->
    parse_headers(Rest, [Octet], 
		  [lists:reverse(Header) | Headers], MaxHeaderSize, Result);
parse_headers(<<?CR>> = Data, Header, Headers, 
	      MaxHeaderSize, Result) ->
    {?MODULE, parse_headers, [Data, Header, Headers, MaxHeaderSize, Result]};
parse_headers(<<Octet, Rest/binary>>, Header, Headers,
	      MaxHeaderSize, Result) ->
    parse_headers(Rest, [Octet | Header], Headers, MaxHeaderSize, Result).


%% RFC2616, Section 10.1.1
%% Note:
%% - Only act on the 100 status if the request included the
%%   "Expect:100-continue" header, otherwise just ignore this response.
status_continue(_, #request{headers = 
			    #http_request_h{expect = "100-continue"}}) ->  
    continue;

status_continue({_,_, Data}, _) ->
    %% The data in the body in this case is actually part of the real
    %% response sent after the "fake" 100-continue.
    {ignore, Data}.

status_service_unavailable(Response = {_, Headers, _}, Request) ->
    case Headers#http_response_h.'retry-after' of 
	undefined ->
	    status_server_error_50x(Response, Request);
	Time when length(Time) < 3 -> % Wait only 99 s or less 
	    NewTime = list_to_integer(Time) * 100, % time in ms
	    {_, Data} =  format_response(Response),
	    {retry, {NewTime, Request}, Data};
	_ ->
	    status_server_error_50x(Response, Request)
    end.

status_server_error_50x(Response, Request) ->
    {Msg, _} =  format_response(Response),
    {stop, {Request#request.id, Msg}}.


redirect(Response = {StatusLine, Headers, Body}, Request) ->
    {_, Data} =  format_response(Response),
    case Headers#http_response_h.location of
	undefined ->
	    transparent(Response, Request);
	RedirUrl ->
	    case http_uri:parse(RedirUrl) of
		{error, no_scheme} when
		(Request#request.settings)#http_options.relaxed ->
		    NewLocation = fix_relative_uri(Request, RedirUrl),
		    redirect({StatusLine, Headers#http_response_h{
					    location = NewLocation},
			      Body}, Request);
		{error, Reason} ->
		    {ok, error(Request, Reason), Data};
		%% Automatic redirection
		{Scheme, _, Host, Port, Path,  Query} -> 
		    NewHeaders = 
			(Request#request.headers)#http_request_h{host = 
								 Host},
		    NewRequest = 
			Request#request{redircount = 
					Request#request.redircount+1,
					scheme = Scheme,
					headers = NewHeaders,
					address = {Host,Port},
					path = Path,
					pquery = Query,
					abs_uri =
					atom_to_list(Scheme) ++ "://" ++
                                        Host ++ ":" ++ 
					integer_to_list(Port) ++
					Path ++ Query},
		    {redirect, NewRequest, Data}
	    end
    end.

%%% Guessing that we received a relative URI, fix it to become an absoluteURI
fix_relative_uri(Request, RedirUrl) ->
    {Server, Port} = Request#request.address,
    Path = Request#request.path,
    atom_to_list(Request#request.scheme) ++ "://" ++ Server ++ ":" ++ Port
	++ Path ++ RedirUrl.
    
error(#request{id = Id}, Reason) ->
    {Id, {error, Reason}}.

transparent(Response, Request) ->    
    {Msg, Data} =  format_response(Response),
    {ok, {Request#request.id, Msg}, Data}.

stream_start(Headers, Request) ->
    {Request#request.id, stream_start,  http_response:header_list(Headers)}.

stream_end(Response, Request = #request{stream = self}) -> 
    {{_, Headers, _}, Data} =  format_response(Response),
    {ok, {Request#request.id, stream_end, Headers}, Data};
stream_end(Response, Request) ->
    {_, Data} =  format_response(Response),
    {ok, {Request#request.id, saved_to_file}, Data}.

is_server_closing(Headers) when record(Headers,http_response_h) ->
    case Headers#http_response_h.connection of
	"close" ->
	    true;
	_ ->
	    false
    end.

format_response({StatusLine, Headers, Body = <<>>}) ->
    {{StatusLine, http_response:header_list(Headers), Body}, <<>>};

format_response({StatusLine, Headers, Body}) ->
    Length = list_to_integer(Headers#http_response_h.'content-length'),
    {NewBody, Data} = 
	case Length of
	    0 ->
		{Body, <<>>};
	    -1 -> % When no lenght indicator is provided
		{Body, <<>>};
	    Length when Length =< size(Body) ->
		<<BodyThisReq:Length/binary, Next/binary>> = Body,
		{BodyThisReq, Next};
	    _ -> %% Connection prematurely ended. 
		{Body, <<>>}
	end,
    {{StatusLine, http_response:header_list(Headers), NewBody}, Data}.

