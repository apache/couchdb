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

-module(ddoc_cache_remove_test).

-export([
    recover/1
]).

-include_lib("couch/include/couch_db.hrl").
-include_lib("mem3/include/mem3.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("ddoc_cache_test.hrl").

recover(DbName) ->
    {ok, #doc{body = {Body}}} = fabric:open_doc(DbName, ?CUSTOM, [?ADMIN_CTX]),
    case couch_util:get_value(<<"status">>, Body) of
        <<"ok">> ->
            {ok, yay};
        <<"not_ok">> ->
            {ruh, roh};
        <<"error">> ->
            erlang:error(thpppt)
    end.

start_couch() ->
    Ctx = ddoc_cache_tutil:start_couch(),
    meck:new(ddoc_cache_ev, [passthrough]),
    Ctx.

stop_couch(Ctx) ->
    meck:unload(),
    ddoc_cache_tutil:stop_couch(Ctx).

check_refresh_test_() ->
    {
        setup,
        fun start_couch/0,
        fun stop_couch/1,
        ddoc_cache_tutil:with([
            {"remove_ddoc", fun remove_ddoc/1},
            {"remove_ddoc_rev", fun remove_ddoc_rev/1},
            {"remove_ddoc_rev_only", fun remove_ddoc_rev_only/1},
            {"remove_custom_not_ok", fun remove_custom_not_ok/1},
            {"remove_custom_error", fun remove_custom_error/1}
        ])
    }.

remove_ddoc({DbName, _}) ->
    ddoc_cache_tutil:clear(),
    meck:reset(ddoc_cache_ev),
    ?assertEqual(0, ets:info(?CACHE, size)),
    {ok, _} = ddoc_cache:open_doc(DbName, ?FOOBAR),

    meck:wait(ddoc_cache_ev, event, [started, '_'], 1000),
    meck:wait(ddoc_cache_ev, event, [default_started, '_'], 1000),

    [#entry{val = DDoc}, #entry{val = DDoc}] = ets:tab2list(?CACHE),
    {Depth, [RevId | _]} = DDoc#doc.revs,
    NewDDoc = DDoc#doc{
        deleted = true,
        body = {[]}
    },
    {ok, _} = fabric:update_doc(DbName, NewDDoc, [?ADMIN_CTX]),

    DDocIdKey = {ddoc_cache_entry_ddocid, {DbName, ?FOOBAR}},
    Rev = {Depth, RevId},
    DDocIdRevKey = {ddoc_cache_entry_ddocid_rev, {DbName, ?FOOBAR, Rev}},
    meck:wait(ddoc_cache_ev, event, [removed, DDocIdKey], 1000),
    meck:wait(ddoc_cache_ev, event, [update_noop, DDocIdRevKey], 1000),

    ?assertMatch({not_found, deleted}, ddoc_cache:open_doc(DbName, ?FOOBAR)),
    ?assertEqual(1, ets:info(?CACHE, size)).

remove_ddoc_rev({DbName, _}) ->
    ddoc_cache_tutil:clear(),
    meck:reset(ddoc_cache_ev),
    Rev = ddoc_cache_tutil:get_rev(DbName, ?VDU),
    {ok, _} = ddoc_cache:open_doc(DbName, ?VDU, Rev),

    meck:wait(ddoc_cache_ev, event, [started, '_'], 1000),
    meck:wait(ddoc_cache_ev, event, [default_started, '_'], 1000),

    % Notice the sort so that we know we're getting the
    % revid version second.
    [_, #entry{key = Key, val = DDoc, pid = Pid}] =
        lists:sort(ets:tab2list(?CACHE)),

    NewDDoc = DDoc#doc{
        body = {[{<<"an">>, <<"update">>}]}
    },
    {ok, _} = fabric:update_doc(DbName, NewDDoc, [?ADMIN_CTX]),
    meck:wait(ddoc_cache_ev, event, [update_noop, Key], 1000),
    % Compact the database so that the old rev is removed
    lists:foreach(
        fun(Shard) ->
            do_compact(Shard#shard.name)
        end,
        mem3:local_shards(DbName)
    ),
    % Trigger a refresh rather than wait for the timeout
    ddoc_cache_entry:refresh(Pid),
    meck:wait(ddoc_cache_ev, event, [removed, Key], 1000),
    ?assertMatch(
        {{not_found, missing}, _},
        ddoc_cache:open_doc(DbName, ?VDU, Rev)
    ),
    ?assertEqual(1, ets:info(?CACHE, size)).

remove_ddoc_rev_only({DbName, _}) ->
    ddoc_cache_tutil:clear(),
    meck:reset(ddoc_cache_ev),
    Rev = ddoc_cache_tutil:get_rev(DbName, ?VDU),
    {ok, _} = ddoc_cache:open_doc(DbName, ?VDU),
    {ok, _} = ddoc_cache:open_doc(DbName, ?VDU, Rev),
    % Relying on the sort order of keys to keep
    % these lined up for testing
    [
        #entry{key = NoRevKey, val = DDoc, pid = NoRevPid},
        #entry{key = RevKey, val = DDoc, pid = RevPid}
    ] = lists:sort(ets:tab2list(?CACHE)),
    NewDDoc = DDoc#doc{
        body = {[{<<"new">>, <<"awesomeness">>}]}
    },
    {ok, _} = fabric:update_doc(DbName, NewDDoc, [?ADMIN_CTX]),
    meck:wait(ddoc_cache_ev, event, [updated, '_'], 1000),
    meck:wait(ddoc_cache_ev, event, [update_noop, RevKey], 1000),
    % Compact the database so that the old rev is removed
    lists:foreach(
        fun(Shard) ->
            do_compact(Shard#shard.name)
        end,
        mem3:local_shards(DbName)
    ),
    % Trigger a refresh rather than wait for the timeout
    ddoc_cache_entry:refresh(NoRevPid),
    ddoc_cache_entry:refresh(RevPid),
    meck:wait(ddoc_cache_ev, event, [update_noop, NoRevKey], 1000),
    meck:wait(ddoc_cache_ev, event, [removed, RevKey], 1000),
    ?assertMatch({ok, _}, ddoc_cache:open_doc(DbName, ?VDU)),
    ?assertMatch(
        {{not_found, missing}, _},
        ddoc_cache:open_doc(DbName, ?VDU, Rev)
    ),
    ?assertEqual(1, ets:info(?CACHE, size)).

remove_custom_not_ok({DbName, _}) ->
    ddoc_cache_tutil:clear(),
    meck:reset(ddoc_cache_ev),
    init_custom_ddoc(DbName),
    {ok, _} = ddoc_cache:open_custom(DbName, ?MODULE),
    [#entry{key = Key}] = ets:tab2list(?CACHE),
    {ok, DDoc} = fabric:open_doc(DbName, ?CUSTOM, [?ADMIN_CTX]),
    NewDDoc = DDoc#doc{
        body = {[{<<"status">>, <<"not_ok">>}]}
    },
    {ok, _} = fabric:update_doc(DbName, NewDDoc, [?ADMIN_CTX]),
    meck:wait(ddoc_cache_ev, event, [removed, Key], 1000),
    ?assertEqual({ruh, roh}, ddoc_cache:open_custom(DbName, ?MODULE)),
    ?assertEqual(0, ets:info(?CACHE, size)).

remove_custom_error({DbName, _}) ->
    ddoc_cache_tutil:clear(),
    meck:reset(ddoc_cache_ev),
    init_custom_ddoc(DbName),
    {ok, _} = ddoc_cache:open_custom(DbName, ?MODULE),
    [#entry{key = Key}] = ets:tab2list(?CACHE),
    {ok, DDoc} = fabric:open_doc(DbName, ?CUSTOM, [?ADMIN_CTX]),
    NewDDoc = DDoc#doc{
        body = {[{<<"status">>, <<"error">>}]}
    },
    {ok, _} = fabric:update_doc(DbName, NewDDoc, [?ADMIN_CTX]),
    meck:wait(ddoc_cache_ev, event, [removed, Key], 1000),
    ?assertError(thpppt, ddoc_cache:open_custom(DbName, ?MODULE)),
    ?assertEqual(0, ets:info(?CACHE, size)).

init_custom_ddoc(DbName) ->
    Body = {[{<<"status">>, <<"ok">>}]},
    {ok, Doc} = fabric:open_doc(DbName, ?CUSTOM, [?ADMIN_CTX]),
    NewDoc = Doc#doc{body = Body},
    {ok, _} = fabric:update_doc(DbName, NewDoc, [?ADMIN_CTX]).

do_compact(ShardName) ->
    {ok, Db} = couch_db:open_int(ShardName, []),
    try
        {ok, Pid} = couch_db:start_compact(Db),
        Ref = erlang:monitor(process, Pid),
        receive
            {'DOWN', Ref, _, _, _} ->
                ok
        end
    after
        couch_db:close(Db)
    end,
    wait_for_compaction(ShardName).

wait_for_compaction(ShardName) ->
    {ok, Db} = couch_db:open_int(ShardName, []),
    CompactRunning =
        try
            {ok, Info} = couch_db:get_db_info(Db),
            couch_util:get_value(compact_running, Info)
        after
            couch_db:close(Db)
        end,
    if
        not CompactRunning ->
            ok;
        true ->
            timer:sleep(100),
            wait_for_compaction(ShardName)
    end.
