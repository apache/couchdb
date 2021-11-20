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

-module(ddoc_cache_open_error_test).

-include_lib("couch/include/couch_db.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("ddoc_cache_test.hrl").

start_couch() ->
    Ctx = ddoc_cache_tutil:start_couch(),
    meck:expect(fabric, open_doc, fun(_, ?FOOBAR, _) ->
        erlang:error(test_kaboom)
    end),
    Ctx.

stop_couch(Ctx) ->
    meck:unload(),
    ddoc_cache_tutil:stop_couch(Ctx).

check_open_error_test_() ->
    {
        setup,
        fun start_couch/0,
        fun stop_couch/1,
        ddoc_cache_tutil:with([
            {"handle_open_error", fun handle_open_error/1}
        ])
    }.

handle_open_error({DbName, _}) ->
    ?assertError(test_kaboom, ddoc_cache:open_doc(DbName, ?FOOBAR)).
