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

-module(httpc_request).

-include("http_internal.hrl").
-include("httpc_internal.hrl").

%% We will not make the change to use base64 in stdlib in inets just yet.
%% it will be included in the next major release of inets. 
-compile({nowarn_deprecated_function, {http_base_64, encode, 1}}).

%%% Internal API
-export([send/3, is_idempotent/1, is_client_closing/1]).

%%%=========================================================================
%%%  Internal application API
%%%=========================================================================
%%-------------------------------------------------------------------------
%% send(MaybeProxy, Request) ->
%%      MaybeProxy - {Host, Port}
%%      Host = string()
%%      Port = integer()
%%	Request - #request{}
%%	Socket - socket()
%%      CookieSupport - enabled | disabled | verify
%%                                   
%% Description: Composes and sends a HTTP-request. 
%%-------------------------------------------------------------------------
send(SendAddr, #request{method = Method, scheme = Scheme,
			path = Path, pquery = Query, headers = Headers,
			content = Content, address = Address, 
			abs_uri = AbsUri, headers_as_is = HeadersAsIs,
			settings = HttpOptions, 
			userinfo = UserInfo},
     Socket) -> 
    
    TmpHeaders = handle_user_info(UserInfo, Headers),

    {TmpHeaders2, Body} = post_data(Method, TmpHeaders, Content, HeadersAsIs),
    
    {NewHeaders, Uri} = case Address of
			    SendAddr ->
				{TmpHeaders2, Path ++ Query};
			    _Proxy ->
				TmpHeaders3 =
				    handle_proxy(HttpOptions, TmpHeaders2),
				{TmpHeaders3, AbsUri}
			end,

    FinalHeaders = case NewHeaders of
		       HeaderList when is_list(HeaderList) ->
			   headers(HeaderList, []);
		       _  ->
			   http_request:http_headers(NewHeaders)
		   end,
    
    Message = 
	lists:append([method(Method), " ", Uri, " HTTP/1.1", ?CRLF, 
		      FinalHeaders, ?CRLF, Body]),
    
    http_transport:send(socket_type(Scheme), Socket, Message).

%%-------------------------------------------------------------------------
%% is_idempotent(Method) ->
%% Method = atom()
%%                                   
%% Description: Checks if Methode is considered idempotent.
%%-------------------------------------------------------------------------

%% In particular, the convention has been established that the GET and
%% HEAD methods SHOULD NOT have the significance of taking an action
%% other than retrieval. These methods ought to be considered "safe".
is_idempotent(head) -> 
    true;
is_idempotent(get) ->
    true;
%% Methods can also have the property of "idempotence" in that (aside
%% from error or expiration issues) the side-effects of N > 0
%% identical requests is the same as for a single request.
is_idempotent(put) -> 
    true;
is_idempotent(delete) ->
    true;
%% Also, the methods OPTIONS and TRACE SHOULD NOT have side effects,
%% and so are inherently idempotent.
is_idempotent(trace) ->
    true;
is_idempotent(options) ->
    true;
is_idempotent(_) ->
    false.

%%-------------------------------------------------------------------------
%% is_client_closing(Headers) ->
%% Headers = #http_request_h{}
%%                                   
%% Description: Checks if the client has supplied a "Connection: close" header.
%%-------------------------------------------------------------------------
is_client_closing(Headers) ->
    case Headers#http_request_h.connection of
	"close" ->
	    true;
	 _ ->
	    false
    end.

%%%========================================================================
%%% Internal functions
%%%========================================================================
post_data(Method, Headers, {ContentType, Body}, HeadersAsIs) 
  when Method == post; Method == put ->
    ContentLength = body_length(Body),	      
    NewBody = case Headers#http_request_h.expect of
		  "100-continue" ->
		      "";
		  _ ->
		      Body
	      end,
    
    NewHeaders = case HeadersAsIs of
		     [] ->
			 Headers#http_request_h{'content-type' = 
						ContentType, 
						'content-length' = 
						ContentLength};
		     _ ->
			 HeadersAsIs
		 end,
    
    {NewHeaders, NewBody};

post_data(_, Headers, _, []) ->
    {Headers, ""};
post_data(_, _, _, HeadersAsIs = [_|_]) ->
    {HeadersAsIs, ""}.

body_length(Body) when is_binary(Body) ->
   integer_to_list(size(Body));

body_length(Body) when is_list(Body) ->
  integer_to_list(length(Body)).

method(Method) ->
    http_util:to_upper(atom_to_list(Method)).

socket_type(http) ->
    ip_comm;
socket_type(https) ->
    {ssl, []}.

headers([], Headers) ->
    lists:flatten(Headers);
headers([{Key,Value} | Rest], Headers) ->
    Header = Key ++ ": " ++ Value ++ ?CRLF,
    headers(Rest, [Header | Headers]).

handle_proxy(_, Headers) when is_list(Headers) ->
    Headers; %% Headers as is option was specified
handle_proxy(HttpOptions, Headers) ->
    case HttpOptions#http_options.proxy_auth of
	undefined ->
	    Headers;
	{User, Password} ->
	    UserPasswd = http_base_64:encode(User ++ ":" ++ Password),
	    Headers#http_request_h{'proxy-authorization' = 
				   "Basic " ++ UserPasswd}
    end.

handle_user_info([], Headers) ->
    Headers;
handle_user_info(UserInfo, Headers) ->
    case string:tokens(UserInfo, ":") of
	[User, Passwd] ->
	    UserPasswd = http_base_64:encode(User ++ ":" ++ Passwd),
	    Headers#http_request_h{authorization = "Basic " ++ UserPasswd};
	_ ->
	    Headers
    end.
