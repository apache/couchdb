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
%%

-define(HTTP_REQUEST_TIMEOUT, infinity).
-define(HTTP_PIPELINE_TIMEOUT, 0).
-define(HTTP_PIPELINE_LENGTH, 2).
-define(HTTP_MAX_TCP_SESSIONS, 2).
-define(HTTP_MAX_REDIRECTS, 4).

%%% HTTP Client per request settings
-record(http_options,{
	  %% Milliseconds before a request times out
	  timeout = ?HTTP_REQUEST_TIMEOUT,  
	  %% bool() - True if automatic redirection on 30X responses.
	  autoredirect = true, 
	  ssl = [], % Ssl socket options
	  proxy_auth, % {User, Password} = {strring(), string()} 
	  relaxed = false % bool() true if not strictly standard compliant
	 }).

%%% HTTP Client per profile setting. Currently there is only one profile.
-record(options, {
	  proxy =  {undefined, []}, % {{ProxyHost, ProxyPort}, [NoProxy]},
	  pipeline_timeout = ?HTTP_PIPELINE_TIMEOUT,
	  max_pipeline_length = ?HTTP_PIPELINE_LENGTH,
	  max_sessions =  ?HTTP_MAX_TCP_SESSIONS,
	  cookies = disabled, % enabled | disabled | verify
	  ipv6 = enabled, % enabled | disabled
	  verbose = false
	 }).

%%% All data associated to a specific HTTP request
-record(request,{
	  id,            % ref() - Request Id
	  from,          % pid() - Caller
	  redircount = 0,% Number of redirects made for this request
	  scheme,        % http | https 
	  address,       % ({Host,Port}) Destination Host and Port
	  path,          % string() - Path of parsed URL
	  pquery,        % string() - Rest of parsed URL
	  method,        % atom() - HTTP request Method
	  headers,       % #http_request_h{}
	  content,       % {ContentType, Body} - Current HTTP request
	  settings,      % #http_options{} - User defined settings
	  abs_uri,       % string() ex: "http://www.erlang.org"
	  userinfo,      % string() - optinal "<userinfo>@<host>:<port>"
	  stream,	 % Boolean() - stream async reply?
	  headers_as_is  % Boolean() - workaround for servers that does
	  %% not honor the http standard, can also be used for testing purposes.
	 }).               

-record(tcp_session,{
	  id,           % {{Host, Port}, HandlerPid}
	  client_close, % true | false
	  scheme,       % http (HTTP/TCP) | https (HTTP/SSL/TCP)
	  socket,       % Open socket, used by connection
	  pipeline_length = 1 % Current length of pipeline 
	 }).

-record(http_cookie,{
	  domain,
	  domain_default = false,
	  name,
	  value,
	  comment,
	  max_age = session,
	  path, 
	  path_default = false,
	  secure = false,
	  version = "0" 
	 }).

