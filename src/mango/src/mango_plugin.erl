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

-module(mango_plugin).

-export([
    before_find/1,
    after_find/3
]).

-define(SERVICE_ID, mango).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

before_find(HttpReq0) ->
    [HttpReq1] = with_pipe(before_find, [HttpReq0]),
    {ok, HttpReq1}.


after_find(HttpReq, HttpResp, Arg0) ->
    [_HttpReq, _HttpResp, Arg1] = with_pipe(after_find, [HttpReq, HttpResp, Arg0]),
    {ok, Arg1}.


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

with_pipe(Func, Args) ->
    do_apply(Func, Args, [pipe]).


do_apply(Func, Args, Opts) ->
    Handle = couch_epi:get_handle(?SERVICE_ID),
    couch_epi:apply(Handle, ?SERVICE_ID, Func, Args, Opts).
