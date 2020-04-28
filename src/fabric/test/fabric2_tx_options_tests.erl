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

-module(fabric2_tx_options_tests).


-include_lib("couch/include/couch_eunit.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("couch/include/couch_db.hrl").
-include("fabric2_test.hrl").
-include("fabric2.hrl").


fdb_tx_options_test_DISABLE() ->
    {
        "Test setting default transaction options",
        setup,
        fun() ->
            meck:new(erlfdb, [passthrough]),
            % erlfdb, rexi and mem3 are all dependent apps for fabric. We make
            % sure to start them so when fabric is started during the test it
            % already has its dependencies
            test_util:start_couch([erlfdb, rexi, mem3, ctrace, fabric])
        end,
        fun(Ctx) ->
            meck:unload(),

            config:delete("fdb_tx_options", "size_limit", false),
            config:delete("fdb_tx_options", "max_retry_delay", false),
            config:delete("fdb_tx_options", "machine_id", false),
            config:delete("fdb_tx_options", "datacenter_id", false),

            test_util:stop_couch(Ctx)
        end,
        with([
            ?TDEF(options_take_effect),
            ?TDEF(can_configure_options_at_runtime)
        ])
    }.


options_take_effect(_) ->
    ok = application:stop(fabric),

    % Try one of each type including some invalid values
    config:set("fdb_tx_options", "size_limit", "150000", false),
    config:set("fdb_tx_options", "max_retry_delay", "badness", false),
    config:set("fdb_tx_options", "machine_id", "123abc", false),
    TooLong = ["x" || _ <- lists:seq(1, 1000)],
    config:set("fdb_tx_options", "datacenter_id", TooLong, false),
    ok = application:start(fabric),

    DbName = ?tempdb(),
    {ok, Db} = fabric2_db:create(DbName, [?ADMIN_CTX]),
    ?assertError({erlfdb_error, ?TRANSACTION_TOO_LARGE},
        add_large_doc(Db, 200000)),
    ok = fabric2_db:delete(DbName, [?ADMIN_CTX]).


can_configure_options_at_runtime(_) ->
    meck:expect(erlfdb, set_option, fun(Fdb, Option, Val) ->
        meck:passthrough([Fdb, Option, Val])
    end),

    meck:reset(erlfdb),

    config:set("fdb_tx_options", "size_limit", "150000", false),
    meck:wait(erlfdb, set_option, ['_', size_limit, 150000], 4000),

    DbName = ?tempdb(),

    {ok, Db} = fabric2_db:create(DbName, [?ADMIN_CTX]),
    ?assertError({erlfdb_error, ?TRANSACTION_TOO_LARGE},
        add_large_doc(Db, 200000)),

    meck:reset(erlfdb),

    config:delete("fdb_tx_options", "size_limit", false),
    % Assert that we get a new handle and are setting our default values
    meck:wait(erlfdb, set_option, ['_', timeout, '_'], 4000),
    erase(?PDICT_DB_KEY),

    {ok, Db1} = fabric2_db:open(DbName, [?ADMIN_CTX]),
    ?assertMatch({ok, _}, add_large_doc(Db1, 200000)),

    ok = fabric2_db:delete(DbName, [?ADMIN_CTX]).


add_large_doc(Db, Size) ->
    Doc = #doc{
        id = fabric2_util:uuid(),
        body = {[{<<"x">>, crypto:strong_rand_bytes(Size)}]}
    },
    fabric2_db:update_doc(Db, Doc).
