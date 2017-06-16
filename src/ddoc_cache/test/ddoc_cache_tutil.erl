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

-module(ddoc_cache_tutil).


-compile(export_all).


-include_lib("couch/include/couch_db.hrl").
-include_lib("couch/include/couch_eunit.hrl").


start_couch() ->
    start_couch([{write_ddocs, true}]).


start_couch(Options) ->
    WriteDDocs = couch_util:get_value(write_ddocs, Options, true),
    purge_modules(),
    Ctx = test_util:start_couch(?CONFIG_CHAIN, [chttpd, ddoc_cache]),
    TmpDb = ?tempdb(),
    ok = fabric:create_db(TmpDb, [{q, "1"}, {n, "1"}]),
    if not WriteDDocs -> ok; true ->
        {ok, _} = fabric:update_docs(TmpDb, ddocs(), [?ADMIN_CTX])
    end,
    {TmpDb, Ctx}.


stop_couch({_TmpDb, Ctx}) ->
    test_util:stop_couch(Ctx).


clear() ->
    application:stop(ddoc_cache),
    application:start(ddoc_cache).


get_rev(DbName, DDocId) ->
    {_, Ref} = erlang:spawn_monitor(fun() ->
        {ok, #doc{revs = Revs}} = fabric:open_doc(DbName, DDocId, [?ADMIN_CTX]),
        {Depth, [RevId | _]} = Revs,
        exit({Depth, RevId})
    end),
    receive
        {'DOWN', Ref, _, _, Rev} -> Rev
    end.


ddocs() ->
    FooBar = #doc{
        id = <<"_design/foobar">>,
        body = {[
            {<<"foo">>, <<"bar">>}
        ]}
    },
    VDU = #doc{
        id = <<"_design/vdu">>,
        body = {[
            {<<"validate_doc_update">>, <<"function(doc) {return;}">>}
        ]}
    },
    Custom = #doc{
        id = <<"_design/custom">>,
        body = {[
            {<<"status">>, <<"ok">>},
            {<<"custom">>, <<"hotrod">>}
        ]}
    },
    [FooBar, VDU, Custom].


purge_modules() ->
    case application:get_key(ddoc_cache, modules) of
        {ok, Mods} ->
            lists:foreach(fun(Mod) ->
                case code:which(Mod) of
                    cover_compiled ->
                        ok;
                    _ ->
                        code:delete(Mod),
                        code:purge(Mod)
                end
            end, Mods);
        undefined ->
            ok
    end.
