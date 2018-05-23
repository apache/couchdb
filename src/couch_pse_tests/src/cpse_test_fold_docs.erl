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

-module(cpse_test_fold_docs).
-compile(export_all).


-include_lib("eunit/include/eunit.hrl").
-include_lib("couch/include/couch_db.hrl").


-define(NUM_DOCS, 100).


setup_each() ->
    cpse_util:dbname().


teardown_each(DbName) ->
    ok = couch_server:delete(DbName, []).


cpse_fold_all(DbName) ->
    fold_all(DbName, fold_docs, fun docid/1).


cpse_fold_all_local(DbName) ->
    fold_all(DbName, fold_local_docs, fun local_docid/1).


cpse_fold_start_key(DbName) ->
    fold_start_key(DbName, fold_docs, fun docid/1).


cpse_fold_start_key_local(DbName) ->
    fold_start_key(DbName, fold_local_docs, fun local_docid/1).


cpse_fold_end_key(DbName) ->
    fold_end_key(DbName, fold_docs, fun docid/1).


cpse_fold_end_key_local(DbName) ->
    fold_end_key(DbName, fold_local_docs, fun local_docid/1).


cpse_fold_end_key_gt(DbName) ->
    fold_end_key_gt(DbName, fold_docs, fun docid/1).


cpse_fold_end_key_gt_local(DbName) ->
    fold_end_key_gt(DbName, fold_local_docs, fun local_docid/1).


cpse_fold_range(DbName) ->
    fold_range(DbName, fold_docs, fun docid/1).


cpse_fold_range_local(DbName) ->
    fold_range(DbName, fold_local_docs, fun local_docid/1).


cpse_fold_stop(DbName) ->
    fold_user_fun_stop(DbName, fold_docs, fun docid/1).


cpse_fold_stop_local(DbName) ->
    fold_user_fun_stop(DbName, fold_local_docs, fun local_docid/1).


% This is a loose test but we have to have this until
% I figure out what to do about the total_rows/offset
% meta data included in _all_docs
cpse_fold_include_reductions(DbName) ->
    {ok, Db} = init_db(DbName, fun docid/1),
    FoldFun = fun(_, _, nil) -> {ok, nil} end,
    Opts = [include_reductions],
    {ok, Count, nil} = couch_db_engine:fold_docs(Db, FoldFun, nil, Opts),
    ?assert(is_integer(Count)),
    ?assert(Count >= 0).


fold_all(DbName, FoldFun, DocIdFun) ->
    DocIds = [DocIdFun(I) || I <- lists:seq(1, ?NUM_DOCS)],
    {ok, Db} = init_db(DbName, DocIdFun),

    {ok, DocIdAccFwd} = couch_db_engine:FoldFun(Db, fun fold_fun/2, [], []),
    ?assertEqual(?NUM_DOCS, length(DocIdAccFwd)),
    ?assertEqual(DocIds, lists:reverse(DocIdAccFwd)),

    Opts = [{dir, rev}],
    {ok, DocIdAccRev} = couch_db_engine:FoldFun(Db, fun fold_fun/2, [], Opts),
    ?assertEqual(?NUM_DOCS, length(DocIdAccRev)),
    ?assertEqual(DocIds, DocIdAccRev).


fold_start_key(DbName, FoldFun, DocIdFun) ->
    {ok, Db} = init_db(DbName, DocIdFun),

    StartKeyNum = ?NUM_DOCS div 4,
    StartKey = DocIdFun(StartKeyNum),

    AllDocIds = [DocIdFun(I) || I <- lists:seq(1, ?NUM_DOCS)],
    DocIdsFwd = [DocIdFun(I) || I <- lists:seq(StartKeyNum, ?NUM_DOCS)],
    DocIdsRev = [DocIdFun(I) || I <- lists:seq(1, StartKeyNum)],

    ?assertEqual({ok, []}, couch_db_engine:FoldFun(Db, fun fold_fun/2, [], [
            {start_key, <<255>>}
        ])),

    ?assertEqual({ok, []}, couch_db_engine:FoldFun(Db, fun fold_fun/2, [], [
            {dir, rev},
            {start_key, <<"">>}
        ])),

    {ok, AllDocIdAccFwd} = couch_db_engine:FoldFun(Db, fun fold_fun/2, [], [
            {start_key, <<"">>}
        ]),
    ?assertEqual(length(AllDocIds), length(AllDocIdAccFwd)),
    ?assertEqual(AllDocIds, lists:reverse(AllDocIdAccFwd)),

    {ok, AllDocIdAccRev} = couch_db_engine:FoldFun(Db, fun fold_fun/2, [], [
            {dir, rev},
            {start_key, <<255>>}
        ]),
    ?assertEqual(length(AllDocIds), length(AllDocIdAccRev)),
    ?assertEqual(AllDocIds, AllDocIdAccRev),

    {ok, DocIdAccFwd} = couch_db_engine:FoldFun(Db, fun fold_fun/2, [], [
            {start_key, StartKey}
        ]),
    ?assertEqual(length(DocIdsFwd), length(DocIdAccFwd)),
    ?assertEqual(DocIdsFwd, lists:reverse(DocIdAccFwd)),

    {ok, DocIdAccRev} = couch_db_engine:FoldFun(Db, fun fold_fun/2, [], [
            {dir, rev},
            {start_key, StartKey}
        ]),
    ?assertEqual(length(DocIdsRev), length(DocIdAccRev)),
    ?assertEqual(DocIdsRev, DocIdAccRev).


fold_end_key(DbName, FoldFun, DocIdFun) ->
    {ok, Db} = init_db(DbName, DocIdFun),

    EndKeyNum = ?NUM_DOCS div 4,
    EndKey = DocIdFun(EndKeyNum),

    ?assertEqual({ok, []}, couch_db_engine:FoldFun(Db, fun fold_fun/2, [], [
            {end_key, <<"">>}
        ])),

    ?assertEqual({ok, []}, couch_db_engine:FoldFun(Db, fun fold_fun/2, [], [
            {dir, rev},
            {end_key, <<255>>}
        ])),

    AllDocIds = [DocIdFun(I) || I <- lists:seq(1, ?NUM_DOCS)],

    {ok, AllDocIdAccFwd} = couch_db_engine:FoldFun(Db, fun fold_fun/2, [], [
            {end_key, <<255>>}
        ]),
    ?assertEqual(length(AllDocIds), length(AllDocIdAccFwd)),
    ?assertEqual(AllDocIds, lists:reverse(AllDocIdAccFwd)),

    {ok, AllDocIdAccRev} = couch_db_engine:FoldFun(Db, fun fold_fun/2, [], [
            {dir, rev},
            {end_key, <<"">>}
        ]),
    ?assertEqual(length(AllDocIds), length(AllDocIdAccFwd)),
    ?assertEqual(AllDocIds, AllDocIdAccRev),

    DocIdsFwd = [DocIdFun(I) || I <- lists:seq(1, EndKeyNum)],

    {ok, DocIdAccFwd} = couch_db_engine:FoldFun(Db, fun fold_fun/2, [], [
            {end_key, EndKey}
        ]),
    ?assertEqual(length(DocIdsFwd), length(DocIdAccFwd)),
    ?assertEqual(DocIdsFwd, lists:reverse(DocIdAccFwd)),

    DocIdsRev = [DocIdFun(I) || I <- lists:seq(EndKeyNum, ?NUM_DOCS)],

    {ok, DocIdAccRev} = couch_db_engine:FoldFun(Db, fun fold_fun/2, [], [
            {dir, rev},
            {end_key, EndKey}
        ]),
    ?assertEqual(length(DocIdsRev), length(DocIdAccRev)),
    ?assertEqual(DocIdsRev, DocIdAccRev).


fold_end_key_gt(DbName, FoldFun, DocIdFun) ->
    {ok, Db} = init_db(DbName, DocIdFun),

    EndKeyNum = ?NUM_DOCS div 4,
    EndKey = DocIdFun(EndKeyNum),

    ?assertEqual({ok, []}, couch_db_engine:FoldFun(Db, fun fold_fun/2, [], [
            {end_key_gt, <<"">>}
        ])),

    ?assertEqual({ok, []}, couch_db_engine:FoldFun(Db, fun fold_fun/2, [], [
            {dir, rev},
            {end_key_gt, <<255>>}
        ])),

    AllDocIds = [DocIdFun(I) || I <- lists:seq(1, ?NUM_DOCS)],

    {ok, AllDocIdAccFwd} = couch_db_engine:FoldFun(Db, fun fold_fun/2, [], [
            {end_key_gt, <<255>>}
        ]),
    ?assertEqual(length(AllDocIds), length(AllDocIdAccFwd)),
    ?assertEqual(AllDocIds, lists:reverse(AllDocIdAccFwd)),

    {ok, AllDocIdAccRev} = couch_db_engine:FoldFun(Db, fun fold_fun/2, [], [
            {dir, rev},
            {end_key_gt, <<"">>}
        ]),
    ?assertEqual(length(AllDocIds), length(AllDocIdAccFwd)),
    ?assertEqual(AllDocIds, AllDocIdAccRev),

    DocIdsFwd = [DocIdFun(I) || I <- lists:seq(1, EndKeyNum - 1)],

    {ok, DocIdAccFwd} = couch_db_engine:FoldFun(Db, fun fold_fun/2, [], [
            {end_key_gt, EndKey}
        ]),
    ?assertEqual(length(DocIdsFwd), length(DocIdAccFwd)),
    ?assertEqual(DocIdsFwd, lists:reverse(DocIdAccFwd)),

    DocIdsRev = [DocIdFun(I) || I <- lists:seq(EndKeyNum + 1, ?NUM_DOCS)],

    {ok, DocIdAccRev} = couch_db_engine:FoldFun(Db, fun fold_fun/2, [], [
            {dir, rev},
            {end_key_gt, EndKey}
        ]),
    ?assertEqual(length(DocIdsRev), length(DocIdAccRev)),
    ?assertEqual(DocIdsRev, DocIdAccRev).


fold_range(DbName, FoldFun, DocIdFun) ->
    {ok, Db} = init_db(DbName, DocIdFun),

    StartKeyNum = ?NUM_DOCS div 4,
    EndKeyNum = StartKeyNum * 3,

    StartKey = DocIdFun(StartKeyNum),
    EndKey = DocIdFun(EndKeyNum),

    ?assertEqual({ok, []}, couch_db_engine:FoldFun(Db, fun fold_fun/2, [], [
            {start_key, <<"">>},
            {end_key, <<"">>}
        ])),

    ?assertEqual({ok, []}, couch_db_engine:FoldFun(Db, fun fold_fun/2, [], [
            {dir, rev},
            {start_key, <<"">>},
            {end_key, <<255>>}
        ])),

    AllDocIds = [DocIdFun(I) || I <- lists:seq(1, ?NUM_DOCS)],

    {ok, AllDocIdAccFwd} = couch_db_engine:FoldFun(Db, fun fold_fun/2, [], [
            {start_key, <<"">>},
            {end_key, <<255>>}
        ]),
    ?assertEqual(length(AllDocIds), length(AllDocIdAccFwd)),
    ?assertEqual(AllDocIds, lists:reverse(AllDocIdAccFwd)),

    {ok, AllDocIdAccRev} = couch_db_engine:FoldFun(Db, fun fold_fun/2, [], [
            {dir, rev},
            {start_key, <<255>>},
            {end_key_gt, <<"">>}
        ]),
    ?assertEqual(length(AllDocIds), length(AllDocIdAccFwd)),
    ?assertEqual(AllDocIds, AllDocIdAccRev),

    DocIdsFwd = [DocIdFun(I) || I <- lists:seq(StartKeyNum, EndKeyNum)],

    {ok, DocIdAccFwd} = couch_db_engine:FoldFun(Db, fun fold_fun/2, [], [
            {start_key, StartKey},
            {end_key, EndKey}
        ]),
    ?assertEqual(length(DocIdsFwd), length(DocIdAccFwd)),
    ?assertEqual(DocIdsFwd, lists:reverse(DocIdAccFwd)),

    DocIdsRev = [DocIdFun(I) || I <- lists:seq(StartKeyNum, EndKeyNum)],

    ?assertEqual({ok, []}, couch_db_engine:FoldFun(Db, fun fold_fun/2, [], [
            {dir, rev},
            {start_key, StartKey},
            {end_key, EndKey}
        ])),

    {ok, DocIdAccRev} = couch_db_engine:FoldFun(Db, fun fold_fun/2, [], [
            {dir, rev},
            {start_key, EndKey},
            {end_key, StartKey}
        ]),
    ?assertEqual(length(DocIdsRev), length(DocIdAccRev)),
    ?assertEqual(DocIdsRev, DocIdAccRev).


fold_user_fun_stop(DbName, FoldFun, DocIdFun) ->
    {ok, Db} = init_db(DbName, DocIdFun),

    StartKeyNum = ?NUM_DOCS div 4,
    StartKey = DocIdFun(StartKeyNum),

    ?assertEqual({ok, []}, couch_db_engine:FoldFun(Db, fun fold_stop/2, [], [
            {start_key, <<255>>}
        ])),

    ?assertEqual({ok, []}, couch_db_engine:FoldFun(Db, fun fold_stop/2, [], [
            {dir, rev},
            {start_key, <<"">>}
        ])),

    SuffixDocIds = [DocIdFun(I) || I <- lists:seq(?NUM_DOCS - 3, ?NUM_DOCS)],

    {ok, SuffixDocIdAcc} = couch_db_engine:FoldFun(Db, fun fold_stop/2, [], [
            {start_key, DocIdFun(?NUM_DOCS - 3)}
        ]),
    ?assertEqual(length(SuffixDocIds), length(SuffixDocIdAcc)),
    ?assertEqual(SuffixDocIds, lists:reverse(SuffixDocIdAcc)),

    PrefixDocIds = [DocIdFun(I) || I <- lists:seq(1, 3)],

    {ok, PrefixDocIdAcc} = couch_db_engine:FoldFun(Db, fun fold_stop/2, [], [
            {dir, rev},
            {start_key, DocIdFun(3)}
        ]),
    ?assertEqual(3, length(PrefixDocIdAcc)),
    ?assertEqual(PrefixDocIds, PrefixDocIdAcc),

    FiveDocIdsFwd = [DocIdFun(I)
            || I <- lists:seq(StartKeyNum, StartKeyNum + 5)],

    {ok, FiveDocIdAccFwd} = couch_db_engine:FoldFun(Db, fun fold_stop/2, [], [
            {start_key, StartKey}
        ]),
    ?assertEqual(length(FiveDocIdsFwd), length(FiveDocIdAccFwd)),
    ?assertEqual(FiveDocIdsFwd, lists:reverse(FiveDocIdAccFwd)),

    FiveDocIdsRev = [DocIdFun(I)
            || I <- lists:seq(StartKeyNum - 5, StartKeyNum)],

    {ok, FiveDocIdAccRev} = couch_db_engine:FoldFun(Db, fun fold_stop/2, [], [
            {dir, rev},
            {start_key, StartKey}
        ]),
    ?assertEqual(length(FiveDocIdsRev), length(FiveDocIdAccRev)),
    ?assertEqual(FiveDocIdsRev, FiveDocIdAccRev).


init_db(DbName, DocIdFun) ->
    {ok, Db1} = cpse_util:create_db(DbName),
    Actions = lists:map(fun(Id) ->
        {create, {DocIdFun(Id), {[{<<"int">>, Id}]}}}
    end, lists:seq(1, ?NUM_DOCS)),
    cpse_util:apply_actions(Db1, [{batch, Actions}]).


fold_fun(Doc, Acc) ->
    Id = case Doc of
        #doc{id = Id0} -> Id0;
        #full_doc_info{id = Id0} -> Id0
    end,
    {ok, [Id | Acc]}.


fold_stop(Doc, Acc) ->
    Id = case Doc of
        #doc{id = Id0} -> Id0;
        #full_doc_info{id = Id0} -> Id0
    end,
    case length(Acc) of
        N when N =< 4 ->
            {ok, [Id | Acc]};
        _ ->
            {stop, [Id | Acc]}
    end.


docid(I) ->
    Str = io_lib:format("~4..0b", [I]),
    iolist_to_binary(Str).


local_docid(I) ->
    Str = io_lib:format("_local/~4..0b", [I]),
    iolist_to_binary(Str).
