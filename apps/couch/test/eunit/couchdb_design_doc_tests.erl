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

-module(couchdb_design_doc_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

setup() ->
    DbName = ?tempdb(),
    {ok, Db} = couch_db:create(DbName, [?ADMIN_CTX]),
    ok = couch_db:close(Db),
    create_design_doc(DbName, <<"_design/foo">>),
    Addr = config:get("httpd", "bind_address", "127.0.0.1"),
    Port = integer_to_list(mochiweb_socket_server:get(couch_httpd, port)),
    BaseUrl = "http://" ++ Addr ++ ":" ++ Port,
    {?b2l(DbName), BaseUrl}.

teardown({DbName, _}) ->
    couch_server:delete(?l2b(DbName), [?ADMIN_CTX]),
    ok.

design_list_test_() ->
    {
        "Check _list functionality",
        {
            setup,
            fun test_util:start_couch/0,
            fun test_util:stop_couch/1,
            {
                foreach,
                fun setup/0,
                fun teardown/1,
                [
                    fun should_return_empty_when_plain_return/1,
                    fun should_return_empty_when_no_docs/1
                ]
            }
        }
    }.

should_return_empty_when_plain_return({DbName, BaseUrl}) ->
    ?_test(begin
        ?assertEqual(
            <<>>,
            query_text(BaseUrl, DbName, "foo", "_list/plain_return/simple_view")
        )
    end).

should_return_empty_when_no_docs({DbName, BaseUrl}) ->
    ?_test(begin
        ?assertEqual(
            <<>>,
            query_text(BaseUrl, DbName, "foo", "_list/simple_render/simple_view")
        )
    end).

create_design_doc(DbName, DDName) ->
    {ok, Db} = couch_db:open(DbName, [?ADMIN_CTX]),
    DDoc = couch_doc:from_json_obj(
        {[
            {<<"_id">>, DDName},
            {<<"language">>, <<"javascript">>},
            {<<"views">>,
                {[
                    {<<"simple_view">>,
                        {[
                            {<<"map">>, <<"function(doc) {emit(doc._id, doc)}">>},
                            {<<"reduce">>,
                                <<"function (key, values, rereduce) {return sum(values);}">>}
                        ]}}
                ]}},
            {<<"lists">>,
                {[
                    {<<"plain_return">>, <<"function(head, req) {return;}">>},
                    {<<"simple_render">>,
                        <<"function(head, req) {var row; while(row=getRow()) {send(JSON.stringify(row)); }}">>}
                ]}}
        ]}
    ),
    {ok, Rev} = couch_db:update_doc(Db, DDoc, []),
    couch_db:close(Db),
    Rev.

query_text(BaseUrl, DbName, DDoc, Path) ->
    {ok, Code, _Headers, Body} = test_request:get(
        BaseUrl ++ "/" ++ DbName ++ "/_design/" ++ DDoc ++ "/" ++ Path
    ),
    ?assertEqual(200, Code),
    Body.
