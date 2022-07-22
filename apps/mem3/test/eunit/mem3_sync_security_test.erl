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

-module(mem3_sync_security_test).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").
-include("mem3.hrl").
-include_lib("eunit/include/eunit.hrl").

% seconds
-define(TIMEOUT, 5).

go_test_() ->
    {
        "security property sync test",
        {
            setup,
            fun start_couch/0,
            fun stop_couch/1,
            {
                foreach,
                fun setup/0,
                fun teardown/1,
                [
                    fun sync_security_ok/1
                ]
            }
        }
    }.

start_couch() ->
    test_util:start_couch([fabric, mem3]).

stop_couch(Ctx) ->
    test_util:stop_couch(Ctx).

setup() ->
    ok = meck:new(fabric, [passthrough]),
    meck:expect(fabric, all_dbs, fun() ->
        {ok, [<<"NoExistDb1">>, <<"NoExistDb2">>]}
    end).

teardown(_) ->
    meck:unload().

sync_security_ok(_) ->
    {timeout, ?TIMEOUT, ?_assertEqual(ok, mem3_sync_security:go())}.
