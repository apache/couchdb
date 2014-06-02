% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License.  You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
% License for the specific language governing permissions and limitations under
% the License.

-module(chttpd_auth).

-export([default_authentication_handler/1]).
-export([cookie_authentication_handler/1]).
-export([handle_session_req/1]).

default_authentication_handler(Req) ->
    couch_httpd_auth:default_authentication_handler(Req, chttpd_auth_cache).

cookie_authentication_handler(Req) ->
    couch_httpd_auth:cookie_authentication_handler(Req, chttpd_auth_cache).

handle_session_req(Req) ->
    couch_httpd_auth:handle_session_req(Req, chttpd_auth_cache).
