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

-module(fabric2_index_tests).


-include_lib("couch/include/couch_eunit.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("couch/include/couch_db.hrl").
-include("fabric2_test.hrl").


% Should match fabric2_index define
-define(SHARDS, 32).


index_test_() ->
    {
        "Test fabric indexing module",
        {
            setup,
            fun setup/0,
            fun cleanup/1,
            with([
                ?TDEF(register_index_works),
                ?TDEF(single_update),
                ?TDEF(multiple_updates),
                ?TDEF(skip_db_if_no_ddocs),
                ?TDEF(ignore_deleted_dbs),
                ?TDEF(check_gen_server_messages)
            ])
        }
    }.


index_process_cleanup_test_() ->
    {
        "Test fabric process cleanup in indexing module",
        {
            foreach,
            fun setup/0,
            fun cleanup/1,
            [
                ?TDEF_FE(updater_processes_start),
                ?TDEF_FE(updater_processes_stop),
                ?TDEF_FE(indexing_can_be_disabled),
                ?TDEF_FE(handle_indexer_blowing_up)
            ]
        }
    }.


setup() ->
    meck:new(config, [passthrough]),
    meck:expect(config, get_integer, fun
        ("fabric", "index_updater_delay_msec", _) -> 200;
        ("fabric", "index_updater_resolution_msec", _) -> 100;

        (_, _, Default) -> Default
    end),
    meck:expect(config, get_boolean, fun
        ("fabric", "index_updater_enabled", _) -> true;
        (_, _, Default) -> Default
    end),

    Indices = application:get_env(fabric, indices, []),

    Ctx = test_util:start_couch([fabric]),

    % Db1 has a valid design doc, a deleted one and one with "autoupdate":false
    {ok, Db1} = fabric2_db:create(?tempdb(), [?ADMIN_CTX]),
    {_, _} = create_doc(Db1, <<"_design/doc1">>),

    DDocId2 = <<"_design/doc2">>,
    {DDocId2, {Pos, Rev}} = create_doc(Db1, DDocId2),
    Delete2 = #doc{id = DDocId2, revs = {Pos, [Rev]}, deleted = true},
    {ok, _} = fabric2_db:update_doc(Db1, Delete2),

    NoAutoUpdate = {[{<<"autoupdate">>, false}]},
    {_, _} = create_doc(Db1, <<"_design/doc3">>, NoAutoUpdate),

    % Db2 doesn't have any desig documents
    {ok, Db2} = fabric2_db:create(?tempdb(), [?ADMIN_CTX]),

    #{db1 => Db1, db2 => Db2, ctx => Ctx, indices => Indices}.


cleanup(#{db1 := Db1, db2 := Db2, ctx := Ctx, indices := Indices}) ->
    catch fabric2_db:delete(fabric2_db:name(Db1), []),
    catch fabric2_db:delete(fabric2_db:name(Db2), []),

    test_util:stop_couch(Ctx),
    application:set_env(fabric, indices, Indices),

    meck:unload().


register_index_works(_) ->
    reset_callbacks(),

    Mod1 = fabric2_test_callback1,
    fabric2_index:register_index(Mod1),
    Indices1 = application:get_env(fabric, indices, []),
    ?assertEqual([Mod1], Indices1),

    Mod2 = fabric2_test_callback2,
    fabric2_index:register_index(Mod2),
    Indices2 = application:get_env(fabric, indices, []),
    ?assertEqual(lists:sort([Mod1, Mod2]), lists:sort(Indices2)).


single_update(#{db1 := Db}) ->
    reset_callbacks(),

    Mod = fabric2_test_callback3,
    setup_callback(Mod),
    create_doc(Db),

    meck:wait(Mod, build_indices, 2, 2000),
    ?assertEqual(1, meck:num_calls(Mod, build_indices, 2)).


multiple_updates(#{db1 := Db}) ->
    reset_callbacks(),

    Mod = fabric2_test_callback4,
    setup_callback(Mod),
    create_docs(Db, 10),

    % should be called at least once
    meck:wait(Mod, build_indices, 2, 2000),

    % Maybe called another time or two at most
    timer:sleep(500),
    ?assert(meck:num_calls(Mod, build_indices, 2) =< 3).


skip_db_if_no_ddocs(#{db2 := Db}) ->
    reset_callbacks(),

    Mod = fabric2_test_callback5,
    setup_callback(Mod),
    create_doc(Db),

    timer:sleep(500),
    ?assertEqual(0, meck:num_calls(Mod, build_indices, 2)).


ignore_deleted_dbs(#{}) ->
    reset_callbacks(),

    Mod = fabric2_test_callback6,
    setup_callback(Mod),
    lists:foreach(fun(_) ->
        RandomDbName = fabric2_util:uuid(),
        fabric2_index:db_updated(RandomDbName)
    end, lists:seq(1, 10000)),

    test_util:wait(fun() ->
        case table_sizes() =:= 0 of
            true -> ok;
            false -> wait
        end
    end, 5000).


check_gen_server_messages(#{}) ->
    CallExpect = {stop, {bad_call, foo}, {bad_call, foo}, baz},
    CastExpect = {stop, {bad_cast, foo}, bar},
    InfoExpect = {stop, {bad_info, foo}, bar},
    ?assertEqual(CallExpect, fabric2_index:handle_call(foo, bar, baz)),
    ?assertEqual(CastExpect, fabric2_index:handle_cast(foo, bar)),
    ?assertEqual(InfoExpect, fabric2_index:handle_info(foo, bar)),
    ?assertEqual(ok, fabric2_index:terminate(shutdown, nil)),
    ?assertEqual({ok, nil}, fabric2_index:code_change(v0, nil, extra)).


updater_processes_start(#{}) ->
    Pid = whereis(fabric2_index),
    ?assert(is_process_alive(Pid)),
    lists:map(fun(N) ->
        ?assertEqual(tid(N), ets:info(tid(N), name))
    end, lists:seq(0, ?SHARDS - 1)).


updater_processes_stop(#{}) ->
    Refs = lists:map(fun(N) ->
        Pid = ets:info(tid(N), owner),
        ?assert(is_process_alive(Pid)),
        monitor(process, Pid)
    end, lists:seq(0, ?SHARDS - 1)),

    % We stop but don't restart fabric after this as we're running in a foreach
    % test list where app restart happens after each test.
    application:stop(fabric),

    lists:foreach(fun(Ref) ->
        receive
            {'DOWN', Ref, _, _, _} -> ok
        after 3000 ->
            ?assert(false)
        end
    end, Refs).


indexing_can_be_disabled(#{db1 := Db}) ->
    meck:expect(config, get_boolean, fun
        ("fabric", "index_updater_enabled", _) -> false;
        (_, _, Default) -> Default
    end),

    Mod = fabric2_test_callback7,
    setup_callback(Mod),

    create_doc(Db),
    timer:sleep(500),
    ?assertEqual(0, meck:num_calls(Mod, build_indices, 2)),

    meck:expect(config, get_boolean, fun
        ("fabric", "index_updater_enabled", _) -> true;
        (_, _, Default) -> Default
    end),

    create_doc(Db),
    meck:wait(Mod, build_indices, 2, 2000).


handle_indexer_blowing_up(#{db1 := Db}) ->
    Mod = fabric2_test_callback8,
    setup_callback(Mod),
    meck:expect(Mod, build_indices, fun(_, _) -> error(bad_index) end),

    MainPid = whereis(fabric2_index),
    WPids1 = [ets:info(tid(N), owner) || N <- lists:seq(0, ?SHARDS - 1)],

    create_doc(Db),
    meck:wait(Mod, build_indices, 2, 2000),

    ?assert(is_process_alive(MainPid)),

    WPids2 = [ets:info(tid(N), owner) || N <- lists:seq(0, ?SHARDS - 1)],
    ?assertEqual(lists:sort(WPids1), lists:sort(WPids2)),
    ?assert(lists:all(fun(Pid) -> is_process_alive(Pid) end, WPids2)).


% Utility functions

setup_callback(Mod) ->
    catch meck:unload(Mod),
    meck:new(Mod, [non_strict]),
    meck:expect(Mod, build_indices, 2, []),
    fabric2_index:register_index(Mod).


reset_callbacks() ->
    Mods = application:get_env(fabric, indices, []),
    application:set_env(fabric, indices, []),
    lists:foreach(fun(M) ->
        catch meck:reset(M),
        catch meck:unload(M)
    end, Mods).


tid(Id) when is_integer(Id) ->
    TableName = "fabric2_index_" ++ integer_to_list(Id),
    list_to_existing_atom(TableName).


table_sizes() ->
    Sizes = [ets:info(tid(N), size) || N <- lists:seq(0, ?SHARDS - 1)],
    lists:sum(Sizes).


create_docs(Db, Count) ->
    lists:map(fun(_) ->
        {DocId, _RevStr} = create_doc(Db),
        DocId
    end, lists:seq(1, Count)).


create_doc(Db) ->
    create_doc(Db, fabric2_util:uuid()).


create_doc(Db, DocId) ->
    create_doc(Db, DocId, {[]}).


create_doc(Db, DocId, Body) ->
    Doc = #doc{
        id = DocId,
        body = Body
    },
    {ok, {Pos, Rev}} = fabric2_db:update_doc(Db, Doc, []),
    {DocId, {Pos, Rev}}.
