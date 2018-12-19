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

-module(couch_db_doc_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

start() ->
    test_util:start_couch([ioq]).


setup() ->
    DbName = ?tempdb(),
    config:set("couchdb", "stem_interactive_updates", "false", false),
    {ok, Db} = couch_db:create(DbName, [?ADMIN_CTX]),
    couch_db:close(Db),
    DbName.


teardown(DbName) ->
    ok = couch_server:delete(DbName, [?ADMIN_CTX]),
    ok.


couch_db_doc_test_() ->
    {
        "CouchDB doc tests",
        {
            setup,
            fun start/0, fun test_util:stop_couch/1,
            {
                foreach,
                fun setup/0, fun teardown/1,
                [
                    fun should_truncate_number_of_revisions/1
                ]
            }
        }
    }.


should_truncate_number_of_revisions(DbName) ->
    DocId = <<"foo">>,
    Db = open_db(DbName),
    couch_db:set_revs_limit(Db, 5),
    Rev = create_doc(Db, DocId),
    Rev10 = add_revisions(Db, DocId, Rev, 10),
    {ok, [{ok, #doc{revs = {11, Revs}}}]} = open_doc_rev(Db, DocId, Rev10),
    ?_assertEqual(5, length(Revs)).


open_db(DbName) ->
    {ok, Db} = couch_db:open_int(DbName, [?ADMIN_CTX]),
    Db.


create_doc(Db, DocId) ->
    add_revision(Db, DocId, undefined).


open_doc_rev(Db0, DocId, Rev) ->
    {ok, Db} = couch_db:reopen(Db0),
    couch_db:open_doc_revs(Db, DocId, [couch_doc:parse_rev(Rev)], []).


add_revision(Db, DocId, undefined) ->
    add_revision(Db, DocId, []);
add_revision(Db, DocId, Rev) when is_binary(Rev) ->
    add_revision(Db, DocId, [{<<"_rev">>, Rev}]);
add_revision(Db0, DocId, Rev) ->
    {ok, Db} = couch_db:reopen(Db0),
    Doc = couch_doc:from_json_obj({[
        {<<"_id">>, DocId},
        {<<"value">>, DocId}
    ] ++ Rev}),
    {ok, NewRev} = couch_db:update_doc(Db, Doc, []),
    {ok, _} = couch_db:ensure_full_commit(Db),
    couch_doc:rev_to_str(NewRev).


add_revisions(Db, DocId, Rev, N) ->
    lists:foldl(fun(_, OldRev) ->
        add_revision(Db, DocId, OldRev)
    end, Rev, lists:seq(1, N)).
