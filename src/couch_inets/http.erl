% ``The contents of this file are subject to the Erlang Public License,
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
%%
%% This module is very loosely based on code initially developed by 
%% Johan Blom at Mobile Arts AB
%% Description:
%%% This version of the HTTP/1.1 client supports:
%%%      - RFC 2616 HTTP 1.1 client part
%%%      - RFC 2818 HTTP Over TLS

-module(http).

%% API
-export([request/1, request/4, cancel_request/1, set_options/1, 
	 verify_cookies/2, cookie_header/1]).

-include("http_internal.hrl").
-include("httpc_internal.hrl").

%%%=========================================================================
%%%  API
%%%=========================================================================

%%--------------------------------------------------------------------------
%% request(Method, Request, HTTPOptions, Options) ->
%%           {ok, {StatusLine, Headers, Body}} | {ok, {Status, Body}} |
%%           {ok, RequestId} | {error,Reason} | {ok, {saved_as, FilePath}
%%
%%	Method - atom() = head | get | put | post | trace | options| delete 
%%	Request - {Url, Headers} | {Url, Headers, ContentType, Body} 
%%	Url - string() 
%%	HTTPOptions - [HttpOption]
%%	HTTPOption - {timeout, Time} | {ssl, SSLOptions} | 
%%                   {proxy_auth, {User, Password}}
%%	Ssloptions = [SSLOption]
%%	SSLOption =  {verify, code()} | {depth, depth()} | {certfile, path()} |
%%	{keyfile, path()} | {password, string()} | {cacertfile, path()} |
%%	{ciphers, string()} 
%%	Options - [Option]
%%	Option - {sync, Boolean} | {body_format, BodyFormat} | 
%%	{full_result, Boolean} | {stream, To} |
%%      {headers_as_is, Boolean}  
%%	StatusLine = {HTTPVersion, StatusCode, ReasonPhrase}</v>
%%	HTTPVersion = string()
%%	StatusCode = integer()
%%	ReasonPhrase = string()
%%	Headers = [Header]
%%      Header = {Field, Value}
%%	Field = string()
%%	Value = string()
%%	Body = string() | binary() - HTLM-code
%%
%% Description: Sends a HTTP-request. The function can be both
%% syncronus and asynchronous in the later case the function will
%% return {ok, RequestId} and later on a message will be sent to the
%% calling process on the format {http, {RequestId, {StatusLine,
%% Headers, Body}}} or {http, {RequestId, {error, Reason}}}
%% %%--------------------------------------------------------------------------
request(Url) ->
    request(get, {Url, []}, [], []).

request(Method, {Url, Headers}, HTTPOptions, Options) 
  when Method==options;Method==get;Method==head;Method==delete;Method==trace ->
    case http_uri:parse(Url) of
	{error,Reason} ->
	    {error,Reason};
	ParsedUrl ->
	    handle_request(Method, Url, {ParsedUrl, Headers, [], []}, 
			   HTTPOptions, Options)
    end;
     
request(Method, {Url,Headers,ContentType,Body}, HTTPOptions, Options) 
  when Method==post;Method==put ->
    case http_uri:parse(Url) of
	{error,Reason} ->
	    {error,Reason};
	ParsedUrl ->
	    handle_request(Method, Url, 
			   {ParsedUrl, Headers, ContentType, Body}, 
			   HTTPOptions, Options)
    end.

%%--------------------------------------------------------------------------
%% request(RequestId) -> ok
%%   RequestId - As returned by request/4  
%%                                 
%% Description: Cancels a HTTP-request.
%%-------------------------------------------------------------------------
cancel_request(RequestId) ->
    ok = httpc_manager:cancel_request(RequestId), 
    receive  
	%% If the request was allready fullfilled throw away the 
	%% answer as the request has been canceled.
	{http, {RequestId, _}} ->
	    ok 
    after 0 ->
	    ok
    end.

%%--------------------------------------------------------------------------
%% set_options(Options) ->
%%   Options - [Option]
%%   Option - {proxy, {Proxy, NoProxy}} | {max_sessions, MaxSessions} | 
%%            {max_pipeline_length, MaxPipeline} | 
%%            {pipeline_timeout, PipelineTimeout} | {cookies, CookieMode}
%%            | {ipv6, Ipv6Mode}
%%   Proxy - {Host, Port}
%%   NoProxy - [Domain | HostName | IPAddress]   
%%   MaxSessions, MaxPipeline, PipelineTimeout = integer()   
%%   CookieMode - enabled | disabled | verify
%%   Ipv6Mode - enabled | disabled
%% Description: Informs the httpc_manager of the new settings. 
%%-------------------------------------------------------------------------
set_options(Options) ->
    ensure_started(no_scheme),
    httpc_manager:set_options(Options).

verify_cookies(SetCookieHeaders, Url) ->
    {_, _, Host, Port, Path, _} = http_uri:parse(Url),
    Cookies = http_cookie:cookies(SetCookieHeaders, Path, Host),
    httpc_manager:store_cookies(Cookies, {Host, Port}),
    ok.

cookie_header(Url) ->
    httpc_manager:cookies(Url).

%%%========================================================================
%%% Internal functions
%%%========================================================================
handle_request(Method, Url, {{Scheme, UserInfo, Host, Port, Path, Query},
			Headers, ContentType, Body}, HTTPOptions, Options) ->
    HTTPRecordOptions = http_options(HTTPOptions, #http_options{}),
    
    Sync = http_util:key1search(Options, sync, true),
    NewHeaders = lists:map(fun({Key, Val}) -> 
				   {http_util:to_lower(Key), Val} end,
			   Headers),
    Stream = http_util:key1search(Options, stream, none),

    case {Sync, Stream} of
	{true, self} ->
	    {error, streaming_error};
	_ ->
	    RecordHeaders = header_record(NewHeaders, #http_request_h{}, Host),
	    Request = #request{from = self(),
			       scheme = Scheme, address = {Host,Port},
			       path = Path, pquery = Query, method = Method,
			       headers = RecordHeaders, 
			       content = {ContentType,Body},
			       settings = HTTPRecordOptions,
			       abs_uri = Url, userinfo = UserInfo, 
			       stream = Stream, 
			       headers_as_is = 
			       headers_as_is(Headers, Options)},
	    
	    ensure_started(Scheme),
	    
	    case httpc_manager:request(Request) of
		{ok, RequestId} ->
		    handle_answer(RequestId, Sync, Options);
		{error, Reason} ->
		    {error, Reason}
	    end
    end.

handle_answer(RequestId, false, _) ->
    {ok, RequestId};
handle_answer(RequestId, true, Options) ->
    receive
	{http, {RequestId, saved_to_file}} ->
	    {ok, saved_to_file};
	{http, {RequestId, Result = {_,_,_}}} ->
	    return_answer(Options, Result);
	{http, {RequestId, {error, Reason}}} ->
	    {error, Reason}
    end.
 
return_answer(Options, {StatusLine, Headers, BinBody}) ->
    Body = 
	case http_util:key1search(Options, body_format, string) of
	    string ->
		binary_to_list(BinBody);
	    _ ->
		BinBody
	end,
    case http_util:key1search(Options, full_result, true) of
	true ->
	    {ok, {StatusLine, Headers, Body}};
	false ->
	    {_, Status, _} = StatusLine,
	    {ok, {Status, Body}}
    end.


%% This options is a workaround for http servers that do not follow the 
%% http standard and have case sensative header parsing. Should only be
%% used if there is no other way to communicate with the server or for
%% testing purpose.
headers_as_is(Headers, Options) ->
     case http_util:key1search(Options, headers_as_is, false) of
	 false ->
	     [];
	 true  ->
	     Headers
     end.

http_options([], Acc) ->
    Acc;
http_options([{timeout, Val} | Settings], Acc) 
  when is_integer(Val), Val >= 0->
    http_options(Settings, Acc#http_options{timeout = Val});
http_options([{timeout, infinity} | Settings], Acc) ->
    http_options(Settings, Acc#http_options{timeout = infinity});
http_options([{autoredirect, Val} | Settings], Acc)   
  when Val == true; Val == false ->
    http_options(Settings, Acc#http_options{autoredirect = Val});
http_options([{ssl, Val} | Settings], Acc) ->
    http_options(Settings, Acc#http_options{ssl = Val});
http_options([{relaxed, Val} | Settings], Acc)
  when Val == true; Val == false ->
    http_options(Settings, Acc#http_options{relaxed = Val});
http_options([{proxy_auth, Val = {User, Passwd}} | Settings], Acc) 
  when is_list(User),
       is_list(Passwd) ->
    http_options(Settings, Acc#http_options{proxy_auth = Val});
http_options([Option | Settings], Acc) ->
    error_logger:info_report("Invalid option ignored ~p~n", [Option]),
    http_options(Settings, Acc).

header_record([], RequestHeaders, Host) ->
    validate_headers(RequestHeaders, Host);
header_record([{"cache-control", Val} | Rest], RequestHeaders, Host) ->
    header_record(Rest, RequestHeaders#http_request_h{'cache-control' = Val},
		  Host);  
header_record([{"connection", Val} | Rest], RequestHeaders, Host) ->
    header_record(Rest, RequestHeaders#http_request_h{connection = Val}, Host);
header_record([{"date", Val} | Rest], RequestHeaders, Host) ->
    header_record(Rest, RequestHeaders#http_request_h{date = Val}, Host);  
header_record([{"pragma", Val} | Rest], RequestHeaders, Host) ->
    header_record(Rest, RequestHeaders#http_request_h{pragma = Val}, Host);  
header_record([{"trailer", Val} | Rest], RequestHeaders, Host) ->
    header_record(Rest, RequestHeaders#http_request_h{trailer = Val}, Host);  
header_record([{"transfer-encoding", Val} | Rest], RequestHeaders, Host) ->
    header_record(Rest, 
		  RequestHeaders#http_request_h{'transfer-encoding' = Val},
		  Host);  
header_record([{"upgrade", Val} | Rest], RequestHeaders, Host) ->
    header_record(Rest, RequestHeaders#http_request_h{upgrade = Val}, Host);  
header_record([{"via", Val} | Rest], RequestHeaders, Host) ->
    header_record(Rest, RequestHeaders#http_request_h{via = Val}, Host);  
header_record([{"warning", Val} | Rest], RequestHeaders, Host) ->
    header_record(Rest, RequestHeaders#http_request_h{warning = Val}, Host);  
header_record([{"accept", Val} | Rest], RequestHeaders, Host) ->
    header_record(Rest, RequestHeaders#http_request_h{accept = Val}, Host);  
header_record([{"accept-charset", Val} | Rest], RequestHeaders, Host) ->
    header_record(Rest, RequestHeaders#http_request_h{'accept-charset' = Val}, 
		  Host);  
header_record([{"accept-encoding", Val} | Rest], RequestHeaders, Host) ->
    header_record(Rest, RequestHeaders#http_request_h{'accept-encoding' = Val},
		  Host);  
header_record([{"accept-language", Val} | Rest], RequestHeaders, Host) ->
    header_record(Rest, RequestHeaders#http_request_h{'accept-language' = Val},
		  Host);  
header_record([{"authorization", Val} | Rest], RequestHeaders, Host) ->
    header_record(Rest, RequestHeaders#http_request_h{authorization = Val}, 
		  Host);  
header_record([{"expect", Val} | Rest], RequestHeaders, Host) ->
    header_record(Rest, RequestHeaders#http_request_h{expect = Val}, Host);
header_record([{"from", Val} | Rest], RequestHeaders, Host) ->
    header_record(Rest, RequestHeaders#http_request_h{from = Val}, Host);  
header_record([{"host", Val} | Rest], RequestHeaders, Host) ->
    header_record(Rest, RequestHeaders#http_request_h{host = Val}, Host);
header_record([{"if-match", Val} | Rest], RequestHeaders, Host) ->
    header_record(Rest, RequestHeaders#http_request_h{'if-match' = Val},
		  Host);  
header_record([{"if-modified-since", Val} | Rest], RequestHeaders, Host) ->
    header_record(Rest, 
		  RequestHeaders#http_request_h{'if-modified-since' = Val},
		  Host);  
header_record([{"if-none-match", Val} | Rest], RequestHeaders, Host) ->
    header_record(Rest, RequestHeaders#http_request_h{'if-none-match' = Val}, 
		  Host);  
header_record([{"if-range", Val} | Rest], RequestHeaders, Host) ->
    header_record(Rest, RequestHeaders#http_request_h{'if-range' = Val}, 
		  Host);  

header_record([{"if-unmodified-since", Val} | Rest], RequestHeaders, Host) ->
    header_record(Rest, RequestHeaders#http_request_h{'if-unmodified-since' 
						      = Val}, Host);  
header_record([{"max-forwards", Val} | Rest], RequestHeaders, Host) ->
    header_record(Rest, RequestHeaders#http_request_h{'max-forwards' = Val}, 
		  Host);  
header_record([{"proxy-authorization", Val} | Rest], RequestHeaders, Host) ->
    header_record(Rest, RequestHeaders#http_request_h{'proxy-authorization' 
						      = Val}, Host);  
header_record([{"range", Val} | Rest], RequestHeaders, Host) ->
    header_record(Rest, RequestHeaders#http_request_h{range = Val}, Host);  
header_record([{"referer", Val} | Rest], RequestHeaders, Host) ->
    header_record(Rest, RequestHeaders#http_request_h{referer = Val}, Host);  
header_record([{"te", Val} | Rest], RequestHeaders, Host) ->
    header_record(Rest, RequestHeaders#http_request_h{te = Val}, Host);  
header_record([{"user-agent", Val} | Rest], RequestHeaders, Host) ->
    header_record(Rest, RequestHeaders#http_request_h{'user-agent' = Val}, 
		  Host);  
header_record([{"allow", Val} | Rest], RequestHeaders, Host) ->
    header_record(Rest, RequestHeaders#http_request_h{allow = Val}, Host);  
header_record([{"content-encoding", Val} | Rest], RequestHeaders, Host) ->
    header_record(Rest, 
		  RequestHeaders#http_request_h{'content-encoding' = Val},
		  Host);  
header_record([{"content-language", Val} | Rest], RequestHeaders, Host) ->
    header_record(Rest, 
		  RequestHeaders#http_request_h{'content-language' = Val}, 
		  Host);  
header_record([{"content-length", Val} | Rest], RequestHeaders, Host) ->
    header_record(Rest, RequestHeaders#http_request_h{'content-length' = Val},
		  Host);  
header_record([{"content-location", Val} | Rest], RequestHeaders, Host) ->
    header_record(Rest, 
		  RequestHeaders#http_request_h{'content-location' = Val},
		  Host);  
header_record([{"content-md5", Val} | Rest], RequestHeaders, Host) ->
    header_record(Rest, RequestHeaders#http_request_h{'content-md5' = Val}, 
		  Host);  
header_record([{"content-range", Val} | Rest], RequestHeaders, Host) ->
    header_record(Rest, RequestHeaders#http_request_h{'content-range' = Val},
		  Host);  
header_record([{"content-type", Val} | Rest], RequestHeaders, Host) ->
    header_record(Rest, RequestHeaders#http_request_h{'content-type' = Val}, 
		  Host);  
header_record([{"expires", Val} | Rest], RequestHeaders, Host) ->
    header_record(Rest, RequestHeaders#http_request_h{expires = Val}, Host);  
header_record([{"last-modified", Val} | Rest], RequestHeaders, Host) ->
    header_record(Rest, RequestHeaders#http_request_h{'last-modified' = Val},
		  Host);  
header_record([{Key, Val} | Rest], RequestHeaders, Host) ->
    header_record(Rest, RequestHeaders#http_request_h{
			  other = [{Key, Val} |
				   RequestHeaders#http_request_h.other]}, 
		  Host).

validate_headers(RequestHeaders = #http_request_h{te = undefined}, Host) ->
    validate_headers(RequestHeaders#http_request_h{te = ""}, Host);
validate_headers(RequestHeaders = #http_request_h{host = undefined}, Host) ->
    validate_headers(RequestHeaders#http_request_h{host = Host}, Host);
validate_headers(RequestHeaders, _) ->
    RequestHeaders.

ensure_started(Scheme) ->
    %% Start of the inets application should really be handled by the 
    %% application using inets. 
    case application:start(couch_inets) of
	{error,{already_started,couch_inets}} ->
	    ok;
	{error, {{already_started,_}, % Started as an included application
	 {inets_app, start, _}}} ->
	    ok;
	ok ->
	    error_logger:info_report("The inets application was not started."
				     " Has now been started as a temporary" 
				     " application.")
    end,
    
    case Scheme of
	https ->
	    %% Start of the ssl application should really be handled by the 
	    %% application using inets. 
	    case application:start(ssl) of
		{error,{already_started,ssl}} ->
		    ok;
		%% Started as an included application
		{error, {{already_started,_}, 
		 {ssl_app, start, _}}} ->
		    ok;
		ok ->
		    error_logger:info_report("The ssl application was not "
					     "started. Has now been started "
					     "as a temporary application.")
	    end;
	_ ->
	    ok
    end.
