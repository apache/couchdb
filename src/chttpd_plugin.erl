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
    handle_error/1
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
    [Error1] = with_pipe(after_request, [Error]),
    Error1.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

with_pipe(Func, Args) ->
    do_apply(Func, Args, [pipe]).

do_apply(Func, Args, Opts) ->
    Handle = couch_epi:get_handle(?SERVICE_ID),
    couch_epi:apply(Handle, ?SERVICE_ID, Func, Args, Opts).
