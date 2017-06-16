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

-module(ddoc_cache_disabled_test).


-include_lib("couch/include/couch_db.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("ddoc_cache_test.hrl").


start_couch() ->
    Ctx = ddoc_cache_tutil:start_couch(),
    config:set("ddoc_cache", "max_size", "0", false),
    Ctx.


check_disabled_test_() ->
    {
        setup,
        fun start_couch/0,
        fun ddoc_cache_tutil:stop_couch/1,
        {with, [
            fun resp_ok/1,
            fun resp_not_found/1,
            fun check_effectively_disabled/1
        ]}
    }.


resp_ok({DbName, _}) ->
    ddoc_cache_tutil:clear(),
    Resp = ddoc_cache:open_doc(DbName, ?FOOBAR),
    ?assertMatch({ok, #doc{id = ?FOOBAR}}, Resp),
    ?assertEqual(0, ets:info(?CACHE, size)),
    ?assertEqual(0, ets:info(?LRU, size)).


resp_not_found({DbName, _}) ->
    ddoc_cache_tutil:clear(),
    Resp = ddoc_cache:open_doc(DbName, <<"_design/not_found">>),
    ?assertEqual({not_found, missing}, Resp),
    ?assertEqual(0, ets:info(?CACHE, size)),
    ?assertEqual(0, ets:info(?LRU, size)).


check_effectively_disabled({DbName, _}) ->
    config:set("ddoc_cache", "max_size", "1", false),
    ddoc_cache_tutil:clear(),
    Resp = ddoc_cache:open_doc(DbName, ?FOOBAR),
    ?assertMatch({ok, #doc{id = ?FOOBAR}}, Resp),
    ?assertEqual(0, ets:info(?CACHE, size)),
    ?assertEqual(0, ets:info(?LRU, size)).
