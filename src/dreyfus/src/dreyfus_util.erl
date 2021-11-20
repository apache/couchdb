% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
% http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-

-module(dreyfus_util).

-include("dreyfus.hrl").
-include_lib("mem3/include/mem3.hrl").
-include_lib("couch/include/couch_db.hrl").

-export([get_shards/2, get_ring_opts/2, sort/2, upgrade/1, export/1, time/2]).
-export([in_black_list/1, in_black_list/3, maybe_deny_index/3]).
-export([get_design_docid/1]).
-export([
    ensure_local_purge_docs/2,
    get_value_from_options/2,
    get_local_purge_doc_id/1,
    get_local_purge_doc_body/4,
    maybe_create_local_purge_doc/2,
    maybe_create_local_purge_doc/3,
    get_signature_from_idxdir/1,
    verify_index_exists/2
]).

get_shards(DbName, #index_query_args{partition = nil} = Args) ->
    case use_ushards(Args) of
        true ->
            mem3:ushards(DbName);
        false ->
            mem3:shards(DbName)
    end;
get_shards(DbName, #index_query_args{partition = Partition} = Args) ->
    PartitionId = couch_partition:shard_key(Partition),
    case use_ushards(Args) of
        true ->
            mem3:ushards(DbName, PartitionId);
        false ->
            mem3:shards(DbName, PartitionId)
    end;
get_shards(DbName, Args) ->
    get_shards(DbName, upgrade(Args)).

use_ushards(#index_query_args{stale = ok}) ->
    true;
use_ushards(#index_query_args{stable = true}) ->
    true;
use_ushards(#index_query_args{}) ->
    false.

get_ring_opts(#index_query_args{partition = nil}, _Shards) ->
    [];
get_ring_opts(#index_query_args{}, Shards) ->
    Shards1 = lists:map(
        fun(#shard{} = S) ->
            S#shard{ref = undefined}
        end,
        Shards
    ),
    [{any, Shards1}].

-spec sort(Order :: relevance | [any()], [#sortable{}]) -> [#sortable{}].
sort(Sort, List0) ->
    {List1, Stash} = stash_items(List0),
    List2 = lists:sort(fun(A, B) -> sort(Sort, A, B) end, List1),
    unstash_items(List2, Stash).

stash_items(List) ->
    lists:unzip([stash_item(Item) || Item <- List]).

stash_item(Item) ->
    Ref = make_ref(),
    {Item#sortable{item = Ref}, {Ref, Item#sortable.item}}.

unstash_items(List, Stash) ->
    [unstash_item(Item, Stash) || Item <- List].

unstash_item(Stashed, Stash) ->
    {_, Item} = lists:keyfind(Stashed#sortable.item, 1, Stash),
    Stashed#sortable{item = Item}.

-spec sort(Order :: relevance | [any()], #sortable{}, #sortable{}) -> boolean().
sort(relevance, #sortable{} = A, #sortable{} = B) ->
    sort2(pad([<<"-">>], <<"">>, length(A#sortable.order)), A, B);
sort(Sort, #sortable{} = A, #sortable{} = B) when is_binary(Sort) ->
    sort2(pad([Sort], <<"">>, length(A#sortable.order)), A, B);
sort(Sort, #sortable{} = A, #sortable{} = B) when is_list(Sort) ->
    sort2(pad(Sort, <<"">>, length(A#sortable.order)), A, B).

-spec sort2([any()], #sortable{}, #sortable{}) -> boolean().
sort2([<<"-", _/binary>> | _], #sortable{order = [A | _]}, #sortable{order = [B | _]}) when
    A =/= B
->
    A > B;
sort2([_ | _], #sortable{order = [A | _]}, #sortable{order = [B | _]}) when A =/= B ->
    A < B;
sort2([], #sortable{shard = #shard{range = A}}, #sortable{shard = #shard{range = B}}) ->
    % arbitrary tie-breaker
    A =< B;
sort2(
    [_ | Rest],
    #sortable{order = [_ | RestA]} = SortableA,
    #sortable{order = [_ | RestB]} = SortableB
) ->
    sort2(Rest, SortableA#sortable{order = RestA}, SortableB#sortable{order = RestB}).

pad(List, _Padding, Length) when length(List) >= Length ->
    List;
pad(List, Padding, Length) ->
    pad(List ++ [Padding], Padding, Length).

upgrade(#index_query_args{} = Args) ->
    Args;
upgrade({index_query_args, Query, Limit, Stale, IncludeDocs, Bookmark, Sort, Grouping, Stable}) ->
    #index_query_args{
        q = Query,
        limit = Limit,
        stale = Stale,
        include_docs = IncludeDocs,
        bookmark = Bookmark,
        sort = Sort,
        grouping = Grouping,
        stable = Stable
    };
upgrade(
    {index_query_args, Query, Limit, Stale, IncludeDocs, Bookmark, Sort, Grouping, Stable, Counts,
        Ranges, Drilldown}
) ->
    #index_query_args{
        q = Query,
        limit = Limit,
        stale = Stale,
        include_docs = IncludeDocs,
        bookmark = Bookmark,
        sort = Sort,
        grouping = Grouping,
        stable = Stable,
        counts = Counts,
        ranges = Ranges,
        drilldown = Drilldown
    };
upgrade(
    {index_query_args, Query, Limit, Stale, IncludeDocs, Bookmark, Sort, Grouping, Stable, Counts,
        Ranges, Drilldown, IncludeFields, HighlightFields, HighlightPreTag, HighlightPostTag,
        HighlightNumber, HighlightSize}
) ->
    #index_query_args{
        q = Query,
        limit = Limit,
        stale = Stale,
        include_docs = IncludeDocs,
        bookmark = Bookmark,
        sort = Sort,
        grouping = Grouping,
        stable = Stable,
        counts = Counts,
        ranges = Ranges,
        drilldown = Drilldown,
        include_fields = IncludeFields,
        highlight_fields = HighlightFields,
        highlight_pre_tag = HighlightPreTag,
        highlight_post_tag = HighlightPostTag,
        highlight_number = HighlightNumber,
        highlight_size = HighlightSize
    };
upgrade(
    {index_query_args, Query, Limit, Stale, IncludeDocs, Bookmark, Sort, Grouping, Stable, Counts,
        Ranges, Drilldown, IncludeFields, HighlightFields, HighlightPreTag, HighlightPostTag,
        HighlightNumber, HighlightSize, RawBookmark}
) ->
    #index_query_args{
        q = Query,
        limit = Limit,
        stale = Stale,
        include_docs = IncludeDocs,
        bookmark = Bookmark,
        sort = Sort,
        grouping = Grouping,
        stable = Stable,
        counts = Counts,
        ranges = Ranges,
        drilldown = Drilldown,
        include_fields = IncludeFields,
        highlight_fields = HighlightFields,
        highlight_pre_tag = HighlightPreTag,
        highlight_post_tag = HighlightPostTag,
        highlight_number = HighlightNumber,
        highlight_size = HighlightSize,
        raw_bookmark = RawBookmark
    }.

export(
    #index_query_args{
        partition = nil,
        counts = nil,
        ranges = nil,
        drilldown = [],
        include_fields = nil,
        highlight_fields = nil
    } = Args
) ->
    % Ensure existing searches work during the upgrade by creating an
    % #index_query_args record in the old format
    {index_query_args, Args#index_query_args.q, Args#index_query_args.limit,
        Args#index_query_args.stale, Args#index_query_args.include_docs,
        Args#index_query_args.bookmark, Args#index_query_args.sort, Args#index_query_args.grouping,
        Args#index_query_args.stable};
export(
    #index_query_args{
        partition = nil,
        include_fields = nil,
        highlight_fields = nil
    } = Args
) ->
    {index_query_args, Args#index_query_args.q, Args#index_query_args.limit,
        Args#index_query_args.stale, Args#index_query_args.include_docs,
        Args#index_query_args.bookmark, Args#index_query_args.sort, Args#index_query_args.grouping,
        Args#index_query_args.stable, Args#index_query_args.counts, Args#index_query_args.ranges,
        Args#index_query_args.drilldown};
export(#index_query_args{partition = nil} = Args) ->
    {index_query_args, Args#index_query_args.q, Args#index_query_args.limit,
        Args#index_query_args.stale, Args#index_query_args.include_docs,
        Args#index_query_args.bookmark, Args#index_query_args.sort, Args#index_query_args.grouping,
        Args#index_query_args.stable, Args#index_query_args.counts, Args#index_query_args.ranges,
        Args#index_query_args.drilldown, Args#index_query_args.include_fields,
        Args#index_query_args.highlight_fields, Args#index_query_args.highlight_pre_tag,
        Args#index_query_args.highlight_post_tag, Args#index_query_args.highlight_number,
        Args#index_query_args.highlight_size, Args#index_query_args.raw_bookmark};
export(QueryArgs) ->
    QueryArgs.

time(Metric, {M, F, A}) when is_list(Metric) ->
    Start = os:timestamp(),
    try
        erlang:apply(M, F, A)
    after
        Length = timer:now_diff(os:timestamp(), Start) / 1000,
        couch_stats:update_histogram([dreyfus | Metric], Length)
    end.

in_black_list(DbName, GroupId, IndexName) when
    is_binary(DbName),
    is_binary(GroupId),
    is_binary(IndexName)
->
    in_black_list(?b2l(DbName), ?b2l(GroupId), ?b2l(IndexName));
in_black_list(DbName, GroupId, IndexName) when
    is_list(DbName),
    is_list(GroupId),
    is_list(IndexName)
->
    in_black_list(lists:flatten([DbName, ".", GroupId, ".", IndexName]));
in_black_list(_DbName, _GroupId, _IndexName) ->
    false.

in_black_list(IndexEntry) when is_list(IndexEntry) ->
    case dreyfus_config:get(IndexEntry) of
        undefined -> false;
        _ -> true
    end;
in_black_list(_IndexEntry) ->
    false.

maybe_deny_index(DbName, GroupId, IndexName) ->
    case in_black_list(DbName, GroupId, IndexName) of
        true ->
            Reason = ?l2b(
                io_lib:format(
                    "Index <~s, ~s, ~s>, is BlackListed",
                    [?b2l(DbName), ?b2l(GroupId), ?b2l(IndexName)]
                )
            ),
            throw({bad_request, Reason});
        _ ->
            ok
    end.

get_design_docid(#doc{id = <<"_design/", DesignName/binary>>}) ->
    DesignName.

get_value_from_options(Key, Options) ->
    case couch_util:get_value(Key, Options) of
        undefined ->
            Reason = binary_to_list(Key) ++ " must exist in Options.",
            throw({bad_request, Reason});
        Value ->
            Value
    end.

ensure_local_purge_docs(DbName, DDocs) ->
    couch_util:with_db(DbName, fun(Db) ->
        lists:foreach(
            fun(DDoc) ->
                #doc{body = {Props}} = DDoc,
                case couch_util:get_value(<<"indexes">>, Props) of
                    undefined ->
                        false;
                    _ ->
                        try dreyfus_index:design_doc_to_indexes(DDoc) of
                            SIndexes -> ensure_local_purge_doc(Db, SIndexes)
                        catch
                            _:_ ->
                                ok
                        end
                end
            end,
            DDocs
        )
    end).

ensure_local_purge_doc(Db, SIndexes) ->
    if
        SIndexes =/= [] ->
            lists:map(
                fun(SIndex) ->
                    maybe_create_local_purge_doc(Db, SIndex)
                end,
                SIndexes
            );
        true ->
            ok
    end.

maybe_create_local_purge_doc(Db, Index) ->
    DocId = dreyfus_util:get_local_purge_doc_id(Index#index.sig),
    case couch_db:open_doc(Db, DocId) of
        {not_found, _} ->
            DbPurgeSeq = couch_db:get_purge_seq(Db),
            DocContent = dreyfus_util:get_local_purge_doc_body(
                Db, DocId, DbPurgeSeq, Index
            ),
            couch_db:update_doc(Db, DocContent, []);
        _ ->
            ok
    end.

maybe_create_local_purge_doc(Db, IndexPid, Index) ->
    DocId = dreyfus_util:get_local_purge_doc_id(Index#index.sig),
    case couch_db:open_doc(Db, DocId) of
        {not_found, _} ->
            DbPurgeSeq = couch_db:get_purge_seq(Db),
            clouseau_rpc:set_purge_seq(IndexPid, DbPurgeSeq),
            DocContent = dreyfus_util:get_local_purge_doc_body(
                Db, DocId, DbPurgeSeq, Index
            ),
            couch_db:update_doc(Db, DocContent, []);
        _ ->
            ok
    end.

get_local_purge_doc_id(Sig) ->
    ?l2b(?LOCAL_DOC_PREFIX ++ "purge-" ++ "dreyfus-" ++ Sig).

get_signature_from_idxdir(IdxDir) ->
    IdxDirList = filename:split(IdxDir),
    Sig = lists:last(IdxDirList),
    Sig2 =
        if
            not is_binary(Sig) -> Sig;
            true -> binary_to_list(Sig)
        end,
    case
        [
            Ch
         || Ch <- Sig2,
            not (((Ch >= $0) and (Ch =< $9)) orelse
                ((Ch >= $a) and (Ch =< $f)) orelse
                ((Ch >= $A) and (Ch =< $F)))
        ] == []
    of
        true -> Sig;
        false -> undefined
    end.

get_local_purge_doc_body(_, LocalDocId, PurgeSeq, Index) ->
    #index{
        name = IdxName,
        ddoc_id = DDocId,
        sig = Sig
    } = Index,
    {Mega, Secs, _} = os:timestamp(),
    NowSecs = Mega * 1000000 + Secs,
    JsonList =
        {[
            {<<"_id">>, LocalDocId},
            {<<"purge_seq">>, PurgeSeq},
            {<<"updated_on">>, NowSecs},
            {<<"indexname">>, IdxName},
            {<<"ddoc_id">>, DDocId},
            {<<"signature">>, Sig},
            {<<"type">>, <<"dreyfus">>}
        ]},
    couch_doc:from_json_obj(JsonList).

verify_index_exists(DbName, Props) ->
    try
        Type = couch_util:get_value(<<"type">>, Props),
        if
            Type =/= <<"dreyfus">> ->
                false;
            true ->
                DDocId = couch_util:get_value(<<"ddoc_id">>, Props),
                IndexName = couch_util:get_value(<<"indexname">>, Props),
                Sig = couch_util:get_value(<<"signature">>, Props),
                couch_util:with_db(DbName, fun(Db) ->
                    case couch_db:get_design_doc(Db, DDocId) of
                        {ok, #doc{} = DDoc} ->
                            {ok, IdxState} = dreyfus_index:design_doc_to_index(
                                DDoc, IndexName
                            ),
                            IdxState#index.sig == Sig;
                        {not_found, _} ->
                            false
                    end
                end)
        end
    catch
        _:_ ->
            false
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(SORT(T, L), lists:sort(fun(A, B) -> sort(T, A, B) end, L)).
-define(ASC, <<"">>).
-define(DESC, <<"-">>).

%% use proper for this...

empty_test() ->
    ?assertEqual([], ?SORT([], [])).

primary_asc_test() ->
    ?assertMatch(
        [#sortable{order = [1]}, #sortable{order = [2]}],
        ?SORT([?ASC], [#sortable{order = [2]}, #sortable{order = [1]}])
    ).

primary_desc_test() ->
    ?assertMatch(
        [#sortable{order = [2]}, #sortable{order = [1]}],
        ?SORT([?DESC], [#sortable{order = [1]}, #sortable{order = [2]}])
    ).

secondary_asc_test() ->
    ?assertMatch(
        [#sortable{order = [1, 1]}, #sortable{order = [1, 2]}],
        ?SORT([?ASC, ?ASC], [#sortable{order = [1, 2]}, #sortable{order = [1, 1]}])
    ).

secondary_desc_test() ->
    ?assertMatch(
        [#sortable{order = [1, 2]}, #sortable{order = [1, 1]}],
        ?SORT([?DESC, ?DESC], [#sortable{order = [1, 1]}, #sortable{order = [1, 2]}])
    ).

stash_test() ->
    {Stashed, Stash} = stash_items([#sortable{order = foo, item = bar}]),
    First = hd(Stashed),
    ?assert(is_reference(First#sortable.item)),
    Unstashed = hd(unstash_items(Stashed, Stash)),
    ?assertEqual(Unstashed#sortable.item, bar).

ring_opts_test() ->
    Shards = [#shard{name = foo, ref = make_ref()}],

    QArgs1 = #index_query_args{partition = nil},
    ?assertEqual([], get_ring_opts(QArgs1, Shards)),

    QArgs2 = #index_query_args{partition = <<"x">>},
    ?assertMatch(
        [{any, [#shard{name = foo, ref = undefined}]}],
        get_ring_opts(QArgs2, Shards)
    ).

-endif.
