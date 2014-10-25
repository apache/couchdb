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

-module(couchdb_location_header_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(TIMEOUT, 1000).


setup() ->
    DbName = ?tempdb(),
    {ok, Db} = couch_db:create(DbName, [?ADMIN_USER]),
    couch_db:close(Db),

    Addr = config:get("httpd", "bind_address", "127.0.0.1"),
    Port = integer_to_list(mochiweb_socket_server:get(couch_httpd, port)),
    Host = "http://" ++ Addr ++ ":" ++ Port,
    {Host, ?b2l(DbName)}.

teardown({_, DbName}) ->
    ok = couch_server:delete(?l2b(DbName), [?ADMIN_USER]),
    ok.


header_test_() ->
    {
        "CouchDB Location Header Tests",
        {
            setup,
            fun test_util:start_couch/0, fun test_util:stop_couch/1,
            {
                foreach,
                fun setup/0, fun teardown/1,
                [
                    fun should_work_with_newlines_in_docs/1,
                    fun should_work_with_newlines_in_attachments/1
                ]
            }
        }
    }.

should_work_with_newlines_in_docs({Host, DbName}) ->
    Url = Host ++ "/" ++ DbName ++ "/docid%0A",
    {"COUCHDB-708",
        ?_assertEqual(
            Url,
            begin
                {ok, _, Headers, _} = test_request:put(Url,
                    [{"Content-Type", "application/json"}], "{}"),
                proplists:get_value("Location", Headers)
            end)}.

should_work_with_newlines_in_attachments({Host, DbName}) ->
    Url = Host ++ "/" ++ DbName,
    AttUrl = Url ++ "/docid%0A/readme.txt",
    {"COUCHDB-708",
        ?_assertEqual(
            AttUrl,
            begin
                Body = "We all live in a yellow submarine!",
                Headers0 = [
                    {"Content-Length", "34"},
                    {"Content-Type", "text/plain"}
                ],
                {ok, _, Headers, _} = test_request:put(AttUrl, Headers0, Body),
                proplists:get_value("Location", Headers)
            end)}.
