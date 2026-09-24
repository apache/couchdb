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

-module(couch_db_recreate_att_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(DOC_ID, ~"doc").
-define(ATT_NAME, ~"att").
-define(ATT_TYPE, ~"application/octet-stream").
-define(ADATA, ~"aaaaa").
-define(BDATA, ~"bbbbbb").

setup() ->
    Src = ?tempdb(),
    Tgt = ?tempdb(),
    {ok, SrcDb} = couch_db:create(Src, [?ADMIN_CTX]),
    {ok, TgtDb} = couch_db:create(Tgt, [?ADMIN_CTX]),
    ok = couch_db:close(SrcDb),
    ok = couch_db:close(TgtDb),
    {Src, Tgt}.

teardown({Src, Tgt}) ->
    ok = couch_server:delete(Src, [?ADMIN_CTX]),
    ok = couch_server:delete(Tgt, [?ADMIN_CTX]).

recreate_att_test_() ->
    {
        "Recreating doc attachments",
        {
            setup,
            fun() -> test_util:start_couch([ioq]) end,
            fun test_util:stop_couch/1,
            {
                foreach,
                fun setup/0,
                fun teardown/1,
                [
                    ?TDEF_FE(t_recreate_revpos),
                    ?TDEF_FE(t_recreate_multiple_deletes),
                    ?TDEF_FE(t_atts_since_works),
                    ?TDEF_FE(t_recreate_replicates),
                    ?TDEF_FE(t_recreate_no_atts),
                    ?TDEF_FE(t_revs_depend_on_att_content),
                    ?TDEF_FE(t_rev_is_deterministic)
                ]
            }
        }
    }.

t_recreate_revpos({Src, _}) ->
    {1, _} = Rev1 = create(Src, ?ADATA),
    {2, _} = delete(Src, Rev1),
    {3, _} = Rev3 = create(Src, ?BDATA),
    Att = open_att(Src, Rev3, []),
    ?assertEqual(3, couch_att:fetch(revpos, Att)),
    ?assertEqual(?BDATA, couch_att:to_binary(Att)).

t_recreate_multiple_deletes({Src, _}) ->
    Rev1 = create(Src, ?ADATA),
    Rev2 = delete(Src, Rev1),
    Rev3 = create(Src, ?BDATA),
    {4, _} = delete(Src, Rev3),
    {5, _} = Rev5 = create(Src, ?ADATA),
    ?assertNotEqual(Rev2, Rev5),
    Att = open_att(Src, Rev5, []),
    ?assertEqual(5, couch_att:fetch(revpos, Att)),
    % a's then b's then a's again
    ?assertEqual(?ADATA, couch_att:to_binary(Att)).

t_atts_since_works({Src, _}) ->
    Rev1 = create(Src, ?ADATA),
    delete(Src, Rev1),
    Rev3 = create(Src, ?BDATA),
    Att = open_att(Src, Rev3, [{atts_since, [Rev1]}]),
    ?assertNotEqual(stub, couch_att:fetch(data, Att)),
    ?assertEqual(?BDATA, couch_att:to_binary(Att)).

t_recreate_replicates({Src, Tgt}) ->
    % Replicate a's
    Rev1 = create(Src, ?ADATA),
    replicate(Src, Tgt, Rev1, []),
    ?assertEqual(?ADATA, couch_att:to_binary(open_att(Tgt, Rev1, []))),
    % Delete, recreate and re-replicate
    delete(Src, Rev1),
    Rev3 = create(Src, ?BDATA),
    replicate(Src, Tgt, Rev3, [Rev1]),
    % Expect b's now
    ?assertEqual(?BDATA, couch_att:to_binary(open_att(Tgt, Rev3, []))).

t_recreate_no_atts({Src, _}) ->
    Rev1 = create(Src, ?ADATA),
    delete(Src, Rev1),
    {ok, Db} = couch_db:open_int(Src, []),
    Doc = #doc{id = ?DOC_ID, body = {[{~"foo", 1}]}},
    {ok, {3, _}} = couch_db:update_doc(Db, Doc, []),
    ok = couch_db:close(Db),
    {ok, Db1} = couch_db:open_int(Src, []),
    {ok, #doc{atts = Atts, body = Body}} = couch_db:open_doc(Db1, ?DOC_ID, []),
    ok = couch_db:close(Db1),
    ?assertEqual([], Atts),
    ?assertEqual({[{~"foo", 1}]}, Body).

t_revs_depend_on_att_content({Db1, Db2}) ->
    % Recreate with different attachment content in each db. We should be
    % getting different revisions then, too.
    do_recreate(Db1, Db2),
    {3, _} = Rev1 = create(Db1, ?ADATA),
    {3, _} = Rev2 = create(Db2, ?BDATA),
    ?assertNotEqual(Rev1, Rev2).

t_rev_is_deterministic({Db1, Db2}) ->
    % Write some random attachment to get a different file position in db2 and
    % we should still get the same revision for same content. The idea is we
    % can generate the data independently on multuple servers but their revs
    % should match (as opposed to creating conflicts)
    create(Db2, ~"junk", [{?ATT_NAME, ?BDATA}]),
    do_recreate(Db1, Db2),
    {3, _} = Rev = create(Db1, ?BDATA),
    ?assertEqual(Rev, create(Db2, ?BDATA)).

% The revisions should match, so a recreated doc will branch off the same
% deleted revision in both dbs.
do_recreate(Db1, Db2) ->
    Rev1 = create(Db1, ?ADATA),
    ?assertEqual(Rev1, create(Db2, ?ADATA)),
    Rev2 = delete(Db1, Rev1),
    ?assertEqual(Rev2, delete(Db2, Rev1)).

create(DbName, Data) ->
    create(DbName, ?DOC_ID, [{?ATT_NAME, Data}]).

create(DbName, DocId, Atts) ->
    AttsJson = [
        {Name, {[{~"content_type", ?ATT_TYPE}, {~"data", base64:encode(Data)}]}}
     || {Name, Data} <- Atts
    ],
    Doc = couch_doc:from_json_obj(
        {[
            {~"_id", DocId},
            {~"_attachments", {AttsJson}}
        ]}
    ),
    {ok, Db} = couch_db:open_int(DbName, []),
    {ok, Rev} = couch_db:update_doc(Db, Doc, []),
    ok = couch_db:close(Db),
    Rev.

delete(DbName, {Pos, RevId}) ->
    {ok, Db} = couch_db:open_int(DbName, []),
    Doc = #doc{id = ?DOC_ID, revs = {Pos, [RevId]}, deleted = true},
    {ok, Rev} = couch_db:update_doc(Db, Doc, []),
    ok = couch_db:close(Db),
    Rev.

open_att(DbName, Rev, Opts) ->
    {ok, Db} = couch_db:open_int(DbName, []),
    {ok, [{ok, #doc{atts = [Att]}}]} = couch_db:open_doc_revs(Db, ?DOC_ID, [Rev], Opts),
    ok = couch_db:close(Db),
    Att.

% Pretend we're replicating. Fetch revs with atts_since that target has. Write
% to target with new_edits=false. Attachments which the source thinks the
% target already has are sent as stubs.
replicate(Src, Tgt, Rev, AttsSince) ->
    {ok, SDb} = couch_db:open_int(Src, []),
    Opts = [revs, {atts_since, AttsSince}],
    {ok, [{ok, Doc}]} = couch_db:open_doc_revs(SDb, ?DOC_ID, [Rev], Opts),
    ok = couch_db:close(SDb),
    Json = couch_doc:to_json_obj(Doc, [revs, attachments]),
    Doc1 = couch_doc:from_json_obj(Json),
    {ok, TDb} = couch_db:open_int(Tgt, []),
    {ok, []} = couch_db:update_docs(TDb, [Doc1], [], ?REPLICATED_CHANGES),
    ok = couch_db:close(TDb).
