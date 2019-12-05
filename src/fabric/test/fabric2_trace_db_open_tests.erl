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

-module(fabric2_trace_db_open_tests).


-include_lib("couch/include/couch_db.hrl").
-include_lib("couch/include/couch_eunit.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("fabric2_test.hrl").


trace_test_() ->
    {
        "Trace operation",
        {
            setup,
            fun setup/0,
            fun cleanup/1,
            with([
                ?TDEF(open_db)
            ])
        }
    }.


setup() ->
    put(erlfdb_trace, "starting fabric"),
    Ctx = test_util:start_couch([fabric]),
    {ok, Db} = fabric2_db:create(?tempdb(), [{user_ctx, ?ADMIN_USER}]),
    {Db, Ctx}.


cleanup({Db, Ctx}) ->
    ok = fabric2_db:delete(fabric2_db:name(Db), []),
    test_util:stop_couch(Ctx).


open_db({Db, _}) ->
    put(erlfdb_trace, <<"open db">>),
    fabric2_server:remove(fabric2_db:name(Db)),
    {ok, _Db} = fabric2_db:open(fabric2_db:name(Db), [{user_ctx, ?ADMIN_USER}]).
