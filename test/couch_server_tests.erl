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

-module(couch_server_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

start() ->
    Ctx = test_util:start_couch(),
    config:set("log", "include_sasl", "false", false),
    Ctx.

setup() ->
    DbName = ?tempdb(),
    {ok, Db} = couch_db:create(DbName, []),
    Db.

setup(rename) ->
    config:set("couchdb", "rename_on_delete", "true", false),
    setup();
setup(_) ->
    setup().

teardown(Db) ->
    (catch couch_db:close(Db)),
    (catch file:delete(Db#db.filepath)).

teardown(rename, Db) ->
    config:set("couchdb", "rename_on_delete", "false", false),
    teardown(Db);
teardown(_, Db) ->
    teardown(Db).


delete_db_test_() ->
    {
        "Test for proper deletion of db file",
        {
            setup,
            fun start/0, fun test_util:stop/1,
            [
                make_test_case(rename, [fun should_rename_on_delete/2]),
                make_test_case(delete, [fun should_delete/2])
            ]
        }
    }.

make_test_case(Mod, Funs) ->
    {
        lists:flatten(io_lib:format("~s", [Mod])),
        {foreachx, fun setup/1, fun teardown/2, [{Mod, Fun} || Fun <- Funs]}
    }.

should_rename_on_delete(_, #db{filepath = Origin, name = DbName}) ->
    Renamed = renamed_filename(Origin),
    ?_test(begin
        ?assert(filelib:is_regular(Origin)),
        ?assertMatch(ok, couch_server:delete(DbName, [])),
        ?assert(filelib:is_regular(Renamed)),
        ?assertNot(filelib:is_regular(Origin))
    end).

should_delete(_, #db{filepath = Origin, name = DbName}) ->
    PossiblyRenamed = renamed_filename(Origin),
    ?_test(begin
        ?assert(filelib:is_regular(Origin)),
        ?assertMatch(ok, couch_server:delete(DbName, [])),
        ?assertNot(filelib:is_regular(PossiblyRenamed)),
        ?assertNot(filelib:is_regular(Origin))
    end).


renamed_filename(Original) ->
    {{Y,Mon,D}, {H,Min,S}} = calendar:universal_time(),
    Suffix = lists:flatten(
        io_lib:format(".~w~2.10.0B~2.10.0B." ++
            "~2.10.0B~2.10.0B~2.10.0B.deleted.couch", [Y,Mon,D,H,Min,S])),
    filename:rootname(Original) ++ Suffix.
