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

-module(couch_mrview_test_util).

-compile(export_all).

-include("couch_db.hrl").
-define(ADMIN, #user_ctx{roles=[<<"_admin">>]}).


init_db(Name, Type) ->
    init_db(Name, Type, 10).


init_db(Name, Type, Count) ->
    {ok, Db} = new_db(Name, Type),
    Docs = make_docs(Count),
    save_docs(Db, Docs).


new_db(Name, Type) ->
    couch_server:delete(Name, [{user_ctx, ?ADMIN}]),
    {ok, Db} = couch_db:create(Name, [{user_ctx, ?ADMIN}]),
    save_docs(Db, [ddoc(Type)]).


save_docs(Db, Docs) ->
    {ok, _} = couch_db:update_docs(Db, Docs, []),
    couch_db:reopen(Db).


make_docs(Count) ->
    make_docs(Count, []).

make_docs(Count, Acc) when Count =< 0 ->
    Acc;
make_docs(Count, Acc) ->
    make_docs(Count-1, [doc(Count) | Acc]).


ddoc(map) ->
    couch_doc:from_json_obj({[
        {<<"_id">>, <<"_design/bar">>},
        {<<"views">>, {[
            {<<"baz">>, {[
                {<<"map">>, <<"function(doc) {emit(doc.val, doc.val);}">>}
            ]}},
            {<<"bing">>, {[
                {<<"map">>, <<"function(doc) {}">>}
            ]}},
            {<<"zing">>, {[
                {<<"map">>, <<
                    "function(doc) {\n"
                    "  if(doc.foo !== undefined)\n"
                    "    emit(doc.foo, 0);\n"
                    "}"
                >>}
            ]}}
        ]}}
    ]});
ddoc(red) ->
    couch_doc:from_json_obj({[
        {<<"_id">>, <<"_design/bar">>},
        {<<"views">>, {[
            {<<"baz">>, {[
                {<<"map">>, <<
                    "function(doc) {\n"
                    "  emit([doc.val % 2, doc.val], doc.val);\n"
                    "}\n"
                >>},
                {<<"reduce">>, <<"function(keys, vals) {return sum(vals);}">>}
            ]}}
        ]}}
    ]}).


doc(Id) ->
    couch_doc:from_json_obj({[
        {<<"_id">>, list_to_binary(integer_to_list(Id))},
        {<<"val">>, Id}
    ]}).
