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

-module(fabric2_trace_doc_create_tests).


-include_lib("couch/include/couch_db.hrl").
-include_lib("couch/include/couch_eunit.hrl").
-include_lib("eunit/include/eunit.hrl").


doc_crud_test_() ->
    {
        "Test document CRUD operations",
        {
            setup,
            fun setup/0,
            fun cleanup/1,
            {with, [
                fun create_new_doc/1,
                fun create_two_docs/1,
                fun create_50_docs/1
            ]}
        }
    }.


setup() ->
    Ctx = test_util:start_couch([fabric]),
    {ok, Db} = fabric2_db:create(?tempdb(), [{user_ctx, ?ADMIN_USER}]),
    {Db, Ctx}.


cleanup({Db, Ctx}) ->
    ok = fabric2_db:delete(fabric2_db:name(Db), []),
    test_util:stop_couch(Ctx).


create_new_doc({Db, _}) ->
    put(erlfdb_trace, <<"one doc">>),
    Doc = #doc{
        id = fabric2_util:uuid(),
        body = {[{<<"foo">>, <<"bar">>}]}
    },
    {ok, _} = fabric2_db:update_doc(Db, Doc).


create_two_docs({Db, _}) ->
    put(erlfdb_trace, <<"two docs">>),
    Doc1 = #doc{
        id = fabric2_util:uuid(),
        body = {[{<<"bam">>, <<"baz">>}]}
    },
    Doc2 = #doc{
        id = fabric2_util:uuid(),
        body = {[{<<"bang">>, <<"bargle">>}]}
    },
    {ok, _} = fabric2_db:update_docs(Db, [Doc1, Doc2]).


create_50_docs({Db, _}) ->
    lists:foreach(fun(_) ->
        spawn_monitor(fun() ->
            Name = io_lib:format("50 docs : ~w", [self()]),
            put(erlfdb_trace, iolist_to_binary(Name)),
            Docs = lists:map(fun(Val) ->
                #doc{
                    id = fabric2_util:uuid(),
                    body = {[{<<"value">>, Val}]}
                }
            end, lists:seq(1, 50)),
            {ok, _} = fabric2_db:update_docs(Db, Docs)
        end)
    end, lists:seq(1, 5)),
    lists:foreach(fun(_) ->
        receive {'DOWN', _, _, _, _} -> ok end
    end, lists:seq(1, 5)).
