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

-module(test_engine_fold_docs).
-compile(export_all).


-include_lib("eunit/include/eunit.hrl").
-include_lib("couch/include/couch_db.hrl").


-define(NUM_DOCS, 100).


cet_fold_all() ->
    fold_all(fold_docs, fun docid/1).


cet_fold_all_local() ->
    fold_all(fold_local_docs, fun local_docid/1).


cet_fold_start_key() ->
    fold_start_key(fold_docs, fun docid/1).


cet_fold_start_key_local() ->
    fold_start_key(fold_local_docs, fun local_docid/1).


cet_fold_end_key() ->
    fold_end_key(fold_docs, fun docid/1).


cet_fold_end_key_local() ->
    fold_end_key(fold_local_docs, fun local_docid/1).


cet_fold_end_key_gt() ->
    fold_end_key_gt(fold_docs, fun docid/1).


cet_fold_end_key_gt_local() ->
    fold_end_key_gt(fold_local_docs, fun local_docid/1).


cet_fold_range() ->
    fold_range(fold_docs, fun docid/1).


cet_fold_range_local() ->
    fold_range(fold_local_docs, fun local_docid/1).


cet_fold_stop() ->
    fold_user_fun_stop(fold_docs, fun docid/1).


cet_fold_stop_local() ->
    fold_user_fun_stop(fold_local_docs, fun local_docid/1).


% This is a loose test but we have to have this until
% I figure out what to do about the total_rows/offset
% meta data included in _all_docs
cet_fold_include_reductions() ->
    {ok, Db} = init_st(fun docid/1),
    FoldFun = fun(_, _, nil) -> {ok, nil} end,
    Opts = [include_reductions],
    {ok, Count, nil} = couch_db_engine:fold_docs(Db, FoldFun, nil, Opts),
    ?assert(is_integer(Count)),
    ?assert(Count >= 0).


fold_all(FoldFun, DocIdFun) ->
    DocIds = [DocIdFun(I) || I <- lists:seq(1, ?NUM_DOCS)],
    {ok, Db} = init_st(DocIdFun),

    {ok, DocIdAccFwd} = couch_db_engine:FoldFun(Db, fun fold_fun/2, [], []),
    ?assertEqual(?NUM_DOCS, length(DocIdAccFwd)),
    ?assertEqual(DocIds, lists:reverse(DocIdAccFwd)),

    Opts = [{dir, rev}],
    {ok, DocIdAccRev} = couch_db_engine:FoldFun(Db, fun fold_fun/2, [], Opts),
    ?assertEqual(?NUM_DOCS, length(DocIdAccRev)),
    ?assertEqual(DocIds, DocIdAccRev).


fold_start_key(FoldFun, DocIdFun) ->
    {ok, Db} = init_st(DocIdFun),

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


fold_end_key(FoldFun, DocIdFun) ->
    {ok, Db} = init_st(DocIdFun),

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


fold_end_key_gt(FoldFun, DocIdFun) ->
    {ok, Db} = init_st(DocIdFun),

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


fold_range(FoldFun, DocIdFun) ->
    {ok, Db} = init_st(DocIdFun),

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


fold_user_fun_stop(FoldFun, DocIdFun) ->
    {ok, Db} = init_st(DocIdFun),

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


init_st(DocIdFun) ->
    {ok, Db1} = test_engine_util:create_db(),
    Actions = lists:map(fun(Id) ->
        {create, {DocIdFun(Id), {[{<<"int">>, Id}]}}}
    end, lists:seq(1, ?NUM_DOCS)),
    test_engine_util:apply_batch(Db1, Actions).


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
