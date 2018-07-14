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

-export([authenticate/2]).
-export([authorize/2]).

-export([default_authentication_handler/1]).
-export([cookie_authentication_handler/1]).
-export([proxy_authentication_handler/1]).
-export([party_mode_handler/1]).

-export([handle_session_req/1]).

-include_lib("couch/include/couch_db.hrl").

-define(SERVICE_ID, chttpd_auth).


%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

authenticate(HttpReq, Default) ->
    maybe_handle(authenticate, [HttpReq], Default).

authorize(HttpReq, Default) ->
    maybe_handle(authorize, [HttpReq], Default).


%% ------------------------------------------------------------------
%% Default callbacks
%% ------------------------------------------------------------------

default_authentication_handler(Req) ->
    couch_httpd_auth:default_authentication_handler(Req, chttpd_auth_cache).

cookie_authentication_handler(Req) ->
    couch_httpd_auth:cookie_authentication_handler(Req, chttpd_auth_cache).

proxy_authentication_handler(Req) ->
    couch_httpd_auth:proxy_authentication_handler(Req).

party_mode_handler(Req) ->
    case config:get("chttpd", "require_valid_user", "false") of
    "true" ->
        throw({unauthorized, <<"Authentication required.">>});
    "false" ->
        case config:get("admins") of
        [] ->
            Req#httpd{user_ctx = ?ADMIN_USER};
        _ ->
            Req#httpd{user_ctx=#user_ctx{}}
        end
    end.

handle_session_req(Req) ->
    couch_httpd_auth:handle_session_req(Req, chttpd_auth_cache).


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

maybe_handle(Func, Args, Default) ->
    Handle = couch_epi:get_handle(?SERVICE_ID),
    case couch_epi:decide(Handle, ?SERVICE_ID, Func, Args, []) of
        no_decision when is_function(Default) ->
            apply(Default, Args);
        no_decision ->
            Default;
        {decided, Result} ->
            Result
    end.
