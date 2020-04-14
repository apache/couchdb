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

-module(couchdb_regression_2756_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

setup() ->
    DbName = ?tempdb(),
    {ok, Db} = couch_db:create(DbName, [?ADMIN_CTX]),
    ok = couch_db:close(Db),
    Addr = config:get("httpd", "bind_address", "127.0.0.1"),
    Port = integer_to_list(mochiweb_socket_server:get(couch_httpd, port)),
    BaseUrl = "http://" ++ Addr ++ ":" ++ Port,
    {?b2l(DbName), BaseUrl}.


teardown({DbName, _}) ->
    couch_server:delete(?l2b(DbName), [?ADMIN_CTX]),
    ok.

should_lower_case_single_umlaut_test() ->
    Con = setup(),
    {DbName, _} = Con,
    create_design_doc(DbName, <<"_design/foo">>),
    create_simple_doc(DbName, "one"),
    ?assertEqual(<<"ä">>,
        query_text(Con, "foo", "_view/single_umlaut")),
    teardown(Con).

create_simple_doc(DbName, DName) ->
    {ok, Db} = couch_db:open(DbName, [?ADMIN_CTX]),
    Doc = couch_doc:from_json_obj({[
        {<<"_id">>, DName}
    ]}),
    {ok, Rev} = couch_db:update_doc(Db, Doc, []),
    couch_db:close(Db),
    Rev.

create_design_doc(DbName, DDName) ->
    {ok, Db} = couch_db:open(DbName, [?ADMIN_CTX]),
    DDoc = couch_doc:from_json_obj({[
        {<<"_id">>, DDName},
        {<<"language">>, <<"javascript">>},
        {<<"views">>, {[
            {<<"simple_view">>, {[
                {<<"map">>, <<"function(doc) {const k = 'Ä'.toLowerCase(); emit(doc._id, k)}">> }
            ]}}
        ]}}
    ]}),
    {ok, Rev} = couch_db:update_doc(Db, DDoc, []),
    couch_db:close(Db),
    Rev.

query_text({DbName, BaseUrl}, DDoc, Path) ->
    {ok, Code, _Headers, Body} = test_request:get(
        BaseUrl ++ "/" ++ DbName ++ "/_design/" ++ DDoc ++ "/" ++ Path),
    ?assertEqual(200, Code),
    Body.
