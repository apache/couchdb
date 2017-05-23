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

go_test() ->
    Ctx = test_util:start_couch([fabric, mem3]),
    ok = meck:new(fabric, [passthrough]),
    meck:expect(fabric, all_dbs, fun() ->
        {ok, [<<"NoExistDb1">>, <<"NoExistDb2">>]}
    end),
    Result = mem3_sync_security:go(),
    meck:unload(fabric),
    test_util:stop_couch(Ctx),
    ?assertEqual(ok, Result).
