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

-module(ddoc_cache_refresh_test).

-export([
    recover/1
]).

-include_lib("couch/include/couch_db.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("ddoc_cache_test.hrl").

recover(DbName) ->
    {ok, {DbName, rand_string()}}.

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
            {"refresh_ddoc", fun refresh_ddoc/1},
            {"refresh_ddoc_rev", fun refresh_ddoc_rev/1},
            {"refresh_vdu", fun refresh_vdu/1},
            {"refresh_custom", fun refresh_custom/1},
            {"refresh_multiple", fun refresh_multiple/1}
        ])
    }.

refresh_ddoc({DbName, _}) ->
    ddoc_cache_tutil:clear(),
    meck:reset(ddoc_cache_ev),
    {ok, _} = ddoc_cache:open_doc(DbName, ?FOOBAR),
    meck:wait(ddoc_cache_ev, event, [started, '_'], 1000),
    meck:wait(ddoc_cache_ev, event, [default_started, '_'], 1000),

    ?assertEqual(2, ets:info(?CACHE, size)),
    [#entry{key = Key, val = DDoc}, _] = lists:sort(ets:tab2list(?CACHE)),
    NewDDoc = DDoc#doc{
        body = {[{<<"foo">>, <<"baz">>}]}
    },
    {ok, {Depth, RevId}} = fabric:update_doc(DbName, NewDDoc, [?ADMIN_CTX]),
    Expect = NewDDoc#doc{
        revs = {Depth, [RevId | element(2, DDoc#doc.revs)]}
    },
    meck:wait(ddoc_cache_ev, event, [updated, {Key, Expect}], 1000),
    ?assertMatch({ok, Expect}, ddoc_cache:open_doc(DbName, ?FOOBAR)),
    ?assertEqual(2, ets:info(?CACHE, size)).

refresh_ddoc_rev({DbName, _}) ->
    ddoc_cache_tutil:clear(),
    meck:reset(ddoc_cache_ev),
    Rev = ddoc_cache_tutil:get_rev(DbName, ?FOOBAR),
    {ok, RevDDoc} = ddoc_cache:open_doc(DbName, ?FOOBAR, Rev),

    meck:wait(ddoc_cache_ev, event, [started, '_'], 1000),
    meck:wait(ddoc_cache_ev, event, [default_started, '_'], 1000),

    [_, #entry{key = Key, val = DDoc}] = lists:sort(ets:tab2list(?CACHE)),
    NewDDoc = DDoc#doc{
        body = {[{<<"foo">>, <<"kazam">>}]}
    },
    {ok, _} = fabric:update_doc(DbName, NewDDoc, [?ADMIN_CTX]),
    % We pass the rev explicitly so we assert that we're
    % getting the same original response from the cache
    meck:wait(ddoc_cache_ev, event, [update_noop, Key], 1000),
    ?assertMatch({ok, RevDDoc}, ddoc_cache:open_doc(DbName, ?FOOBAR, Rev)),
    ?assertEqual(2, ets:info(?CACHE, size)).

refresh_vdu({DbName, _}) ->
    ddoc_cache_tutil:clear(),
    meck:reset(ddoc_cache_ev),
    {ok, [_]} = ddoc_cache:open_validation_funs(DbName),
    [#entry{key = Key}] = ets:tab2list(?CACHE),
    {ok, DDoc} = fabric:open_doc(DbName, ?VDU, [?ADMIN_CTX]),
    {ok, _} = fabric:update_doc(DbName, DDoc#doc{body = {[]}}, [?ADMIN_CTX]),
    meck:wait(ddoc_cache_ev, event, [updated, {Key, []}], 1000),
    ?assertMatch({ok, []}, ddoc_cache:open_validation_funs(DbName)),
    ?assertEqual(1, ets:info(?CACHE, size)).

refresh_custom({DbName, _}) ->
    ddoc_cache_tutil:clear(),
    meck:reset(ddoc_cache_ev),
    {ok, Resp1} = ddoc_cache:open_custom(DbName, ?MODULE),
    {ok, DDoc} = fabric:open_doc(DbName, ?VDU, [?CUSTOM]),
    {ok, _} = fabric:update_doc(DbName, DDoc#doc{body = {[]}}, [?ADMIN_CTX]),
    meck:wait(ddoc_cache_ev, event, [updated, '_'], 1000),
    ?assertNotEqual({ok, Resp1}, ddoc_cache:open_custom(DbName, ?MODULE)),
    ?assertEqual(1, ets:info(?CACHE, size)).

refresh_multiple({DbName, _}) ->
    ddoc_cache_tutil:clear(),
    meck:reset(ddoc_cache_ev),
    Rev = ddoc_cache_tutil:get_rev(DbName, ?FOOBAR),
    {ok, DDoc} = ddoc_cache:open_doc(DbName, ?FOOBAR),
    {ok, DDoc} = ddoc_cache:open_doc(DbName, ?FOOBAR, Rev),
    ?assertEqual(2, ets:info(?CACHE, size)),
    % Relying on the sort order of entry keys to make
    % sure our entries line up for this test
    [
        #entry{key = NoRevKey, val = DDoc},
        #entry{key = RevKey, val = DDoc}
    ] = lists:sort(ets:tab2list(?CACHE)),
    NewDDoc = DDoc#doc{
        body = {[{<<"foo">>, <<"kalamazoo">>}]}
    },
    {ok, {Depth, RevId}} = fabric:update_doc(DbName, NewDDoc, [?ADMIN_CTX]),
    Updated = NewDDoc#doc{
        revs = {Depth, [RevId | element(2, DDoc#doc.revs)]}
    },
    meck:wait(ddoc_cache_ev, event, [update_noop, RevKey], 1000),
    meck:wait(ddoc_cache_ev, event, [updated, {NoRevKey, Updated}], 1000),
    % We pass the rev explicitly so we assert that we're
    % getting the same original response from the cache
    ?assertEqual({ok, Updated}, ddoc_cache:open_doc(DbName, ?FOOBAR)),
    ?assertEqual({ok, DDoc}, ddoc_cache:open_doc(DbName, ?FOOBAR, Rev)),
    ?assertEqual(2, ets:info(?CACHE, size)).

rand_string() ->
    Bin = crypto:strong_rand_bytes(8),
    to_hex(Bin, []).

to_hex(<<>>, Acc) ->
    list_to_binary(lists:reverse(Acc));
to_hex(<<C1:4, C2:4, Rest/binary>>, Acc) ->
    to_hex(Rest, [hexdig(C1), hexdig(C2) | Acc]).

hexdig(C) when C >= 0, C =< 9 ->
    C + $0;
hexdig(C) when C >= 10, C =< 15 ->
    C + $A - 10.
