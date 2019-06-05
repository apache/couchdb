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

-module(fabric2_changes_fold_tests).


-include_lib("couch/include/couch_db.hrl").
-include_lib("couch/include/couch_eunit.hrl").
-include_lib("eunit/include/eunit.hrl").


-define(DOC_COUNT, 25).


changes_fold_test_() ->
    {
        "Test changes fold operations",
        {
            setup,
            fun setup/0,
            fun cleanup/1,
            {with, [
                fun fold_changes_basic/1,
                fun fold_changes_since_now/1,
                fun fold_changes_since_seq/1,
                fun fold_changes_basic_rev/1,
                fun fold_changes_since_now_rev/1,
                fun fold_changes_since_seq_rev/1
            ]}
        }
    }.


setup() ->
    Ctx = test_util:start_couch([fabric]),
    {ok, Db} = fabric2_db:create(?tempdb(), [{user_ctx, ?ADMIN_USER}]),
    Rows = lists:map(fun(Val) ->
        DocId = fabric2_util:uuid(),
        Doc = #doc{
            id = DocId,
            body = {[{<<"value">>, Val}]}
        },
        {ok, RevId} = fabric2_db:update_doc(Db, Doc, []),
        UpdateSeq = fabric2_db:get_update_seq(Db),
        #{
            id => DocId,
            sequence => UpdateSeq,
            deleted => false,
            rev_id => RevId
        }
    end, lists:seq(1, ?DOC_COUNT)),
    {Db, Rows, Ctx}.


cleanup({Db, _DocIdRevs, Ctx}) ->
    ok = fabric2_db:delete(fabric2_db:name(Db), []),
    test_util:stop_couch(Ctx).


fold_changes_basic({Db, DocRows, _}) ->
    {ok, Rows} = fabric2_db:fold_changes(Db, 0, fun fold_fun/2, []),
    ?assertEqual(lists:reverse(DocRows), Rows).


fold_changes_since_now({Db, _, _}) ->
    {ok, Rows} = fabric2_db:fold_changes(Db, now, fun fold_fun/2, []),
    ?assertEqual([], Rows).


fold_changes_since_seq({_, [], _}) ->
    ok;

fold_changes_since_seq({Db, [Row | RestRows], _}) ->
    #{sequence := Since} = Row,
    {ok, Rows} = fabric2_db:fold_changes(Db, Since, fun fold_fun/2, []),
    ?assertEqual(lists:reverse(RestRows), Rows),
    fold_changes_since_seq({Db, RestRows, nil}).


fold_changes_basic_rev({Db, _, _}) ->
    Opts = [{dir, rev}],
    {ok, Rows} = fabric2_db:fold_changes(Db, 0, fun fold_fun/2, [], Opts),
    ?assertEqual([], Rows).


fold_changes_since_now_rev({Db, DocRows, _}) ->
    Opts = [{dir, rev}],
    {ok, Rows} = fabric2_db:fold_changes(Db, now, fun fold_fun/2, [], Opts),
    ?assertEqual(DocRows, Rows).


fold_changes_since_seq_rev({_, [], _}) ->
    ok;

fold_changes_since_seq_rev({Db, DocRows, _}) ->
    #{sequence := Since} = lists:last(DocRows),
    Opts = [{dir, rev}],
    {ok, Rows} = fabric2_db:fold_changes(Db, Since, fun fold_fun/2, [], Opts),
    ?assertEqual(DocRows, Rows),
    RestRows = lists:sublist(DocRows, length(DocRows) - 1),
    fold_changes_since_seq_rev({Db, RestRows, nil}).


fold_fun(#{} = Change, Acc) ->
    {ok, [Change | Acc]}.
