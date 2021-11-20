% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(chttpd_plugin).

-export([
    before_request/1,
    after_request/2,
    handle_error/1,
    before_response/4,
    before_serve_file/5
]).

-define(SERVICE_ID, chttpd).

-include_lib("couch/include/couch_db.hrl").

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

before_request(HttpReq) ->
    [HttpReq1] = with_pipe(before_request, [HttpReq]),
    {ok, HttpReq1}.

after_request(HttpReq, Result) ->
    [_, Result1] = with_pipe(after_request, [HttpReq, Result]),
    {ok, Result1}.

handle_error(Error) ->
    [Error1] = with_pipe(handle_error, [Error]),
    Error1.

before_response(HttpReq0, Code0, Headers0, Value0) ->
    [HttpReq, Code, Headers, Value] =
        with_pipe(before_response, [HttpReq0, Code0, Headers0, Value0]),
    {ok, {HttpReq, Code, Headers, Value}}.

before_serve_file(Req0, Code0, Headers0, RelativePath0, DocumentRoot0) ->
    [HttpReq, Code, Headers, RelativePath, DocumentRoot] =
        with_pipe(before_serve_file, [
            Req0, Code0, Headers0, RelativePath0, DocumentRoot0
        ]),
    {ok, {HttpReq, Code, Headers, RelativePath, DocumentRoot}}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

with_pipe(Func, Args) ->
    do_apply(Func, Args, [pipe]).

do_apply(Func, Args, Opts) ->
    Handle = couch_epi:get_handle(?SERVICE_ID),
    couch_epi:apply(Handle, ?SERVICE_ID, Func, Args, Opts).
