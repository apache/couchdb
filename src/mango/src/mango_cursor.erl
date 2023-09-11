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

-module(mango_cursor).

-export([
    create/4,
    explain/1,
    execute/3,
    maybe_filter_indexes_by_ddoc/2,
    remove_indexes_with_partial_filter_selector/1,
    maybe_add_warning/4,
    maybe_noop_range/2
]).

-include_lib("couch/include/couch_db.hrl").
-include("mango.hrl").
-include("mango_cursor.hrl").

-ifdef(HAVE_DREYFUS).
-define(CURSOR_MODULES, [
    mango_cursor_view,
    mango_cursor_text,
    mango_cursor_nouveau,
    mango_cursor_special
]).
-else.
-define(CURSOR_MODULES, [
    mango_cursor_view,
    mango_cursor_nouveau,
    mango_cursor_special
]).
-endif.

-define(SUPERVISOR, mango_cursor_sup).

-spec create(Db, Selector, Options, Kind) -> {ok, #cursor{}} when
    Db :: database(),
    Selector :: selector(),
    Options :: cursor_options(),
    Kind :: cursor_kind().
create(Db, Selector0, Opts, Kind) ->
    Selector = mango_selector:normalize(Selector0),
    {UsableIndexes, Trace} = mango_idx:get_usable_indexes(Db, Selector, Opts, Kind),
    case maybe_filter_indexes_by_ddoc(UsableIndexes, Opts) of
        [] ->
            % use_index doesn't match a valid index - fall back to a valid one
            create_cursor(Db, {UsableIndexes, Trace}, Selector, Opts);
        UserSpecifiedIndex ->
            create_cursor(Db, {UserSpecifiedIndex, Trace}, Selector, Opts)
    end.

-spec enhance_candidates(Candidates, [{#idx{}, properties()}]) -> Candidates when
    Candidates :: #{#idx{} => properties()}.
enhance_candidates(Entries, Inputs) ->
    Combiner =
        fun(Key, Value1, Value2) ->
            case Key of
                usable -> Value1 andalso Value2;
                reason -> lists:append(Value1, Value2);
                ranking -> Value1 + Value2
            end
        end,
    lists:foldr(
        fun({Index, Delta}, Map) ->
            Updater = fun(Value) -> maps:merge_with(Combiner, Value, Delta) end,
            maps:update_with(Index, Updater, Map)
        end,
        Entries,
        Inputs
    ).

-type ranking() :: pos_integer().
-type properties() ::
    #{
        usable := boolean(),
        ranking := ranking(),
        reason := [reason()]
    }.

-spec tag_elems(properties(), sets:set(#idx{})) -> [{#idx{}, properties()}].
tag_elems(Properties, Set) ->
    sets:fold(fun(Index, Acc) -> [{Index, Properties} | Acc] end, [], Set).

-type reason_description() ::
    {name, reason()}.
-type analysis_attribute() ::
    {usable, boolean()}
    | {reasons, [reason_description()]}
    | {ranking, ranking()}
    | {covering, boolean()}.
-type analysis() ::
    {[analysis_attribute()]}.
-type candidate_index_attribute() ::
    {index, #idx{}} | {analysis, analysis()}.
-type candidate_index() ::
    {[candidate_index_attribute()]}.

-spec extract_candidate_indexes(#cursor{}) -> [candidate_index()].
extract_candidate_indexes(Cursor) ->
    #cursor{trace = Trace, index = Winner, fields = Fields} = Cursor,
    #{
        all_indexes := AllIndexes,
        global_indexes := GlobalIndexes,
        partition_indexes := PartitionIndexes,
        usable_indexes := UsableIndexes,
        usability_map := UsabilityMap,
        filtered_indexes := FilteredIndexes,
        indexes_of_type := IndexesOfType
    } = Trace,
    % specific to view indexes
    SortedIndexRanges = maps:get(sorted_index_ranges, Trace, []),
    % simple difference calculations to determine the results in each stage,
    % without looking at the implementation
    PartialIndexes = sets:subtract(AllIndexes, GlobalIndexes),
    OutOfScopeIndexes = sets:subtract(GlobalIndexes, PartitionIndexes),
    NotUsableIndexes = sets:subtract(PartitionIndexes, UsableIndexes),
    ExcludedIndexes = sets:subtract(UsableIndexes, FilteredIndexes),
    UnfavoredIndexes = sets:subtract(FilteredIndexes, IndexesOfType),
    % determine rankings
    UnfavoredIndexesRank = max(1, length(SortedIndexRanges)),
    [
        PartialIndexesRank,
        OutOfScopeIndexesRank,
        NotUsableIndexesRank,
        ExcludedIndexesRank,
        UnfavoredIndexesRank
    ] =
        lists:foldl(
            fun(Indexes, [PreviousRank | _] = Acc) ->
                Rank =
                    case (not sets:is_empty(Indexes)) of
                        true -> PreviousRank + 1;
                        false -> PreviousRank
                    end,
                [Rank | Acc]
            end,
            [UnfavoredIndexesRank],
            [UnfavoredIndexes, ExcludedIndexes, NotUsableIndexes, OutOfScopeIndexes]
        ),
    % start building the list of candidates
    AddCandidate =
        fun(Index, Map) ->
            Map#{Index => #{}}
        end,
    Candidates0 = sets:fold(AddCandidate, #{}, AllIndexes),
    PartialIndexesTags = tag_elems(
        #{usable => false, reason => [is_partial], ranking => PartialIndexesRank},
        PartialIndexes
    ),
    Candidates1 = enhance_candidates(Candidates0, PartialIndexesTags),
    OutOfScopeIndexesTags = tag_elems(
        #{usable => false, reason => [scope_mismatch], ranking => OutOfScopeIndexesRank},
        OutOfScopeIndexes
    ),
    Candidates2 = enhance_candidates(Candidates1, OutOfScopeIndexesTags),
    NotUsableIndexesTags = tag_elems(
        % the reason is going to be filled out by the mango_idx modules
        #{usable => false, ranking => NotUsableIndexesRank},
        NotUsableIndexes
    ),
    Candidates3 = enhance_candidates(Candidates2, NotUsableIndexesTags),
    ExcludedIndexesTags = tag_elems(
        #{usable => true, reason => [excluded_by_user], ranking => ExcludedIndexesRank},
        ExcludedIndexes
    ),
    Candidates4 = enhance_candidates(Candidates3, ExcludedIndexesTags),
    UnfavoredIndexesTags = tag_elems(
        #{usable => true, reason => [unfavored_type], ranking => UnfavoredIndexesRank},
        UnfavoredIndexes
    ),
    Candidates5 = enhance_candidates(Candidates4, UnfavoredIndexesTags),
    NotChosenIndexesTags = tag_elems(
        #{usable => true},
        IndexesOfType
    ),
    Candidates6 = enhance_candidates(Candidates5, NotChosenIndexesTags),
    NotUsableDetails =
        lists:flatmap(
            fun({Index, {Usable, Details}}) ->
                case Usable of
                    true -> [];
                    false -> [{Index, Details}]
                end
            end,
            UsabilityMap
        ),
    Candidates7 = enhance_candidates(Candidates6, NotUsableDetails),
    WinnerColumnWidth =
        case Winner of
            none ->
                0;
            W ->
                case mango_idx:columns(W) of
                    X when is_list(X) -> length(X);
                    all_fields -> 256
                end
        end,
    {_, NotChosenDetails} =
        lists:foldl(
            fun({Index, _Prefix, Distance}, {N, Acc}) ->
                ColumnWidth = length(mango_idx:columns(Index)),
                Reason =
                    if
                        Distance > 0 -> [less_overlap];
                        ColumnWidth > WinnerColumnWidth -> [too_many_fields];
                        true -> [alphabetically_comes_after]
                    end,
                Tags = #{
                    reason => Reason,
                    ranking => N
                },
                {N + 1, [{Index, Tags} | Acc]}
            end,
            {0, []},
            SortedIndexRanges
        ),
    Candidates8 = enhance_candidates(Candidates7, NotChosenDetails),
    Candidates = maps:remove(Winner, Candidates8),
    % produce the final list
    ToList =
        fun(Index, Tags, Acc) ->
            #idx{type = IndexType} = Index,
            #{usable := Usable, reason := Reason, ranking := Ranking} = Tags,
            Covering =
                case IndexType of
                    <<"json">> -> mango_idx_view:covers(Index, Fields);
                    _ -> null
                end,
            Reasons = [{[{name, hd(Reason)}]}],
            Analysis =
                {[
                    {usable, Usable},
                    {reasons, Reasons},
                    {ranking, Ranking},
                    {covering, Covering}
                ]},
            Entry =
                {[
                    {index, mango_idx:to_json(Index)},
                    {analysis, Analysis}
                ]},
            [Entry | Acc]
        end,
    maps:fold(ToList, [], Candidates).

-type selector_hint() :: {indexable_fields, [field()]} | {unindexable_fields, [field()]}.
-type selector_hints() :: {[selector_hint()]}.

-spec extract_selector_hints(selector()) -> selector_hints().
extract_selector_hints(Selector) ->
    DreyfusAvailable = dreyfus:available(),
    NouveauEnabled = nouveau:enabled(),
    AsIndex =
        fun(Module) ->
            case Module of
                mango_cursor_view -> [{mango_idx_view, json}];
                mango_cursor_text when DreyfusAvailable -> [{mango_idx_text, text}];
                mango_cursor_nouveau when NouveauEnabled -> [{mango_idx_nouveau, nouveau}];
                _ -> []
            end
        end,
    Modules = lists:flatmap(AsIndex, ?CURSOR_MODULES),
    Populate =
        fun({Module, IndexType}) ->
            AllFields = sets:from_list(mango_selector:fields(Selector)),
            Normalize = fun(N) -> hd(string:split(N, ":")) end,
            IndexableFields = sets:from_list(
                lists:map(Normalize, Module:indexable_fields(Selector))
            ),
            UnindexableFields = sets:subtract(AllFields, IndexableFields),
            {[
                {type, IndexType},
                {indexable_fields, sets:to_list(IndexableFields)},
                {unindexable_fields, sets:to_list(UnindexableFields)}
            ]}
        end,
    lists:map(Populate, Modules).

-type explain_attribute() ::
    {dbname, database()}
    | {index, ejson()}
    | {partitioned, boolean()}
    | {selector, ejson()}
    | {opts, ejson()}
    | {limit, integer()}
    | {skip, integer()}
    | {fields, [field()]}
    | {index_candidates, [candidate_index()]}
    | {selector_hints, selector_hints()}
    | {atom(), any()}.
-type explain_response() :: {[explain_attribute()]}.

-spec explain(#cursor{}) -> explain_response().
explain(#cursor{} = Cursor) ->
    #cursor{
        db = Db,
        index = Index,
        selector = Selector,
        opts = Opts0,
        limit = Limit,
        skip = Skip,
        fields = Fields0
    } = Cursor,
    DbName = couch_db:name(Db),
    Partitioned = fabric_util:is_partitioned(Db),
    {ModExplain, JSON} =
        case Index of
            none ->
                {[], null};
            Idx ->
                Mod = mango_idx:cursor_mod(Idx),
                {Mod:explain(Cursor), mango_idx:to_json(Idx)}
        end,
    Opts1 = lists:keydelete(user_ctx, 1, Opts0),
    % The value of `r` needs to be translated to an integer
    % otherwise `jiffy:encode/1` will render it as an array.
    RValue = lists:keyfind(r, 1, Opts1),
    Opts2 =
        case RValue of
            {r, R} ->
                lists:keyreplace(r, 1, Opts1, {r, list_to_integer(R)});
            false ->
                Opts1
        end,
    Fields =
        case Fields0 of
            all_fields -> [];
            Value -> Value
        end,
    OptsFields = lists:keyfind(fields, 1, Opts2),
    Opts =
        case OptsFields of
            {fields, all_fields} ->
                lists:keyreplace(fields, 1, Opts2, {fields, []});
            _ ->
                Opts2
        end,
    CandidateIndexes = extract_candidate_indexes(Cursor),
    SelectorHints = extract_selector_hints(Selector),
    {
        [
            {dbname, DbName},
            {index, JSON},
            {partitioned, Partitioned},
            {selector, Selector},
            {opts, {Opts}},
            {limit, Limit},
            {skip, Skip},
            {fields, Fields},
            {index_candidates, CandidateIndexes},
            {selector_hints, SelectorHints}
        ] ++ ModExplain
    }.

execute(#cursor{index = Idx} = Cursor, UserFun, UserAcc) ->
    Mod = mango_idx:cursor_mod(Idx),
    Mod:execute(Cursor, UserFun, UserAcc).

maybe_filter_indexes_by_ddoc(Indexes, Opts) ->
    case lists:keyfind(use_index, 1, Opts) of
        {use_index, []} ->
            [];
        {use_index, [DesignId]} ->
            filter_indexes(Indexes, DesignId);
        {use_index, [DesignId, ViewName]} ->
            filter_indexes(Indexes, DesignId, ViewName)
    end.

filter_indexes(Indexes, DesignId0) ->
    DesignId =
        case DesignId0 of
            <<"_design/", _/binary>> ->
                DesignId0;
            Else ->
                <<"_design/", Else/binary>>
        end,
    FiltFun = fun(I) -> mango_idx:ddoc(I) == DesignId end,
    lists:filter(FiltFun, Indexes).

filter_indexes(Indexes0, DesignId, ViewName) ->
    Indexes = filter_indexes(Indexes0, DesignId),
    FiltFun = fun(I) -> mango_idx:name(I) == ViewName end,
    lists:filter(FiltFun, Indexes).

remove_indexes_with_partial_filter_selector(Indexes) ->
    FiltFun = fun(Idx) ->
        case mango_idx:get_partial_filter_selector(Idx) of
            undefined -> true;
            _ -> false
        end
    end,
    lists:filter(FiltFun, Indexes).

maybe_add_warning(UserFun, #cursor{index = Index, opts = Opts}, Stats, UserAcc) ->
    W0 = invalid_index_warning(Index, Opts),
    W1 = no_index_warning(Index),
    W2 = index_scan_warning(Stats),
    Warnings = lists:append([W0, W1, W2]),
    case Warnings of
        [] ->
            UserAcc;
        _ ->
            WarningStr = iolist_to_binary(lists:join(<<"\n">>, Warnings)),
            Arg = {add_key, warning, WarningStr},
            {_Go, UserAcc1} = UserFun(Arg, UserAcc),
            UserAcc1
    end.

% create a dummy cursor in absence of usable indexes, utilized by _explain
create_cursor(Db, {[], Trace0}, Selector, Opts) ->
    Blank = #{filtered_indexes => sets:new(), indexes_of_type => sets:new()},
    Trace = maps:merge(Trace0, Blank),
    Limit = couch_util:get_value(limit, Opts, mango_opts:default_limit()),
    Skip = couch_util:get_value(skip, Opts, 0),
    Fields = couch_util:get_value(fields, Opts, all_fields),
    Bookmark = couch_util:get_value(bookmark, Opts),
    {ok, #cursor{
        db = Db,
        index = none,
        trace = Trace,
        selector = Selector,
        opts = Opts,
        limit = Limit,
        skip = Skip,
        fields = Fields,
        bookmark = Bookmark
    }};
create_cursor(Db, {Indexes, Trace0}, Selector, Opts) ->
    Trace1 = maps:merge(Trace0, #{filtered_indexes => sets:from_list(Indexes)}),
    [{CursorMod, CursorModIndexes} | _] = group_indexes_by_type(Indexes),
    Trace = maps:merge(Trace1, #{indexes_of_type => sets:from_list(CursorModIndexes)}),
    CursorMod:create(Db, {CursorModIndexes, Trace}, Selector, Opts).

group_indexes_by_type(Indexes) ->
    IdxDict = lists:foldl(
        fun(I, D) ->
            dict:append(mango_idx:cursor_mod(I), I, D)
        end,
        dict:new(),
        Indexes
    ),
    % The first cursor module that has indexes will be
    % used to service this query. This is so that we
    % don't suddenly switch indexes for existing client
    % queries.
    lists:flatmap(
        fun(CMod) ->
            case dict:find(CMod, IdxDict) of
                {ok, CModIndexes} ->
                    [{CMod, CModIndexes}];
                error ->
                    []
            end
        end,
        ?CURSOR_MODULES
    ).

% warn if the _all_docs index was used to fulfil a query
no_index_warning(#idx{type = Type}) when Type =:= <<"special">> ->
    couch_stats:increment_counter([mango, unindexed_queries]),
    [<<"No matching index found, create an index to optimize query time.">>];
no_index_warning(_) ->
    [].

% warn if user specified an index which doesn't exist or isn't valid
% for the selector.
% In this scenario, Mango will ignore the index hint and auto-select an index.
invalid_index_warning(Index, Opts) ->
    UseIndex = lists:keyfind(use_index, 1, Opts),
    invalid_index_warning_int(Index, UseIndex).

invalid_index_warning_int(Index, {use_index, [DesignId]}) ->
    Filtered = filter_indexes([Index], DesignId),
    if
        Filtered /= [] ->
            [];
        true ->
            couch_stats:increment_counter([mango, query_invalid_index]),
            Reason = fmt(
                "_design/~s was not used because it does not contain a valid index for this query.",
                [ddoc_name(DesignId)]
            ),
            [Reason]
    end;
invalid_index_warning_int(Index, {use_index, [DesignId, ViewName]}) ->
    Filtered = filter_indexes([Index], DesignId, ViewName),
    if
        Filtered /= [] ->
            [];
        true ->
            couch_stats:increment_counter([mango, query_invalid_index]),
            Reason = fmt(
                "_design/~s, ~s was not used because it is not a valid index for this query.",
                [ddoc_name(DesignId), ViewName]
            ),
            [Reason]
    end;
invalid_index_warning_int(_, _) ->
    [].

% warn if a large number of documents needed to be scanned per result
% returned, implying a lot of in-memory filtering
index_scan_warning(#execution_stats{
    totalDocsExamined = Docs,
    totalQuorumDocsExamined = DocsQuorum,
    resultsReturned = ResultCount
}) ->
    % Docs and DocsQuorum are mutually exclusive so it's safe to sum them
    DocsScanned = Docs + DocsQuorum,
    Ratio = calculate_index_scan_ratio(DocsScanned, ResultCount),
    Threshold = config:get_integer("mango", "index_scan_warning_threshold", 10),
    case Threshold > 0 andalso Ratio > Threshold of
        true ->
            couch_stats:increment_counter([mango, too_many_docs_scanned]),
            Reason =
                <<"The number of documents examined is high in proportion to the number of results returned. Consider adding a more specific index to improve this.">>,
            [Reason];
        false ->
            []
    end.

% When there is an empty array for certain operators, we don't actually
% want to execute the query so we deny it by making the range [empty].
% To clarify, we don't want this query to execute: {"$or": []}. Results should
% be empty. We do want this query to execute: {"age": 22, "$or": []}. It should
% return the same results as {"age": 22}
maybe_noop_range({[{Op, []}]}, IndexRanges) ->
    Noops = [<<"$all">>, <<"$and">>, <<"$or">>, <<"$in">>],
    case lists:member(Op, Noops) of
        true ->
            [empty];
        false ->
            IndexRanges
    end;
maybe_noop_range(_, IndexRanges) ->
    IndexRanges.

calculate_index_scan_ratio(DocsScanned, 0) ->
    DocsScanned;
calculate_index_scan_ratio(DocsScanned, ResultCount) ->
    DocsScanned / ResultCount.

fmt(Format, Args) ->
    iolist_to_binary(io_lib:format(Format, Args)).

ddoc_name(<<"_design/", Name/binary>>) ->
    Name;
ddoc_name(Name) ->
    Name.

-ifdef(TEST).
-include_lib("couch/include/couch_eunit.hrl").

create_test_() ->
    {
        foreach,
        fun() ->
            ok
        end,
        fun(_) ->
            meck:unload()
        end,
        [
            ?TDEF_FE(t_create_regular, 10),
            ?TDEF_FE(t_create_user_specified_index, 10),
            ?TDEF_FE(t_create_invalid_user_specified_index, 10)
        ]
    }.

t_create_regular(_) ->
    IndexSpecial = #idx{type = <<"special">>, def = all_docs},
    IndexView = #idx{type = <<"json">>},
    UsableIndexes = [IndexView, IndexSpecial],
    FilteredIndexes = UsableIndexes,
    IndexesOfType = [IndexView],
    Trace1 = #{},
    Trace2 =
        #{
            filtered_indexes => sets:from_list(FilteredIndexes),
            indexes_of_type => sets:from_list(IndexesOfType)
        },
    Options = [{use_index, []}],
    meck:expect(mango_selector, normalize, [selector], meck:val(normalized_selector)),
    meck:expect(
        mango_idx,
        get_usable_indexes,
        [db, normalized_selector, Options, target],
        meck:val({UsableIndexes, Trace1})
    ),
    meck:expect(
        mango_cursor_view,
        create,
        [db, {IndexesOfType, Trace2}, normalized_selector, Options],
        meck:val(view_cursor)
    ),
    ?assertEqual(view_cursor, create(db, selector, Options, target)).

t_create_user_specified_index(_) ->
    IndexSpecial = #idx{type = <<"special">>, def = all_docs},
    IndexView1 = #idx{type = <<"json">>, ddoc = <<"_design/view_idx1">>},
    IndexView2 = #idx{type = <<"json">>, ddoc = <<"_design/view_idx2">>},
    IndexView3 = #idx{type = <<"json">>, ddoc = <<"_design/view_idx3">>},
    UsableIndexes = [IndexSpecial, IndexView1, IndexView2, IndexView3],
    FilteredIndexes = [IndexView2],
    IndexesOfType = FilteredIndexes,
    Trace1 = #{},
    Trace2 =
        #{
            filtered_indexes => sets:from_list(FilteredIndexes),
            indexes_of_type => sets:from_list(IndexesOfType)
        },
    Options = [{use_index, [<<"_design/view_idx2">>]}],
    meck:expect(mango_selector, normalize, [selector], meck:val(normalized_selector)),
    meck:expect(
        mango_idx,
        get_usable_indexes,
        [db, normalized_selector, Options, target],
        meck:val({UsableIndexes, Trace1})
    ),
    meck:expect(
        mango_cursor_view,
        create,
        [db, {IndexesOfType, Trace2}, normalized_selector, Options],
        meck:val(view_cursor)
    ),
    ?assertEqual(view_cursor, create(db, selector, Options, target)).

t_create_invalid_user_specified_index(_) ->
    IndexSpecial = #idx{type = <<"special">>, def = all_docs},
    IndexView1 = #idx{type = <<"json">>, ddoc = <<"_design/view_idx1">>},
    IndexView2 = #idx{type = <<"json">>, ddoc = <<"_design/view_idx2">>},
    IndexView3 = #idx{type = <<"json">>, ddoc = <<"_design/view_idx3">>},
    UsableIndexes = [IndexSpecial, IndexView1, IndexView2, IndexView3],
    IndexesOfType = [IndexView1, IndexView2, IndexView3],
    Trace1 = #{},
    Trace2 =
        #{
            filtered_indexes => sets:from_list(UsableIndexes),
            indexes_of_type => sets:from_list(IndexesOfType)
        },
    Options = [{use_index, [<<"foobar">>]}],
    meck:expect(mango_selector, normalize, [selector], meck:val(normalized_selector)),
    meck:expect(
        mango_idx,
        get_usable_indexes,
        [db, normalized_selector, Options, target],
        meck:val({UsableIndexes, Trace1})
    ),
    meck:expect(
        mango_cursor_view,
        create,
        [db, {IndexesOfType, Trace2}, normalized_selector, Options],
        meck:val(view_cursor)
    ),
    ?assertEqual(view_cursor, create(db, selector, Options, target)).

enhance_candidates_test() ->
    Candidates1 = #{index => #{reason => [], usable => true}},
    Candidates2 = #{index => #{reason => [reason1], usable => true}},
    Candidates3 = #{index => #{reason => [reason1, reason2], usable => false}},
    Deltas1 = [{index, #{reason => [reason1], usable => true}}],
    Deltas2 = [{index, #{reason => [reason2], usable => false}}],
    ?assertEqual(Candidates2, enhance_candidates(Candidates1, Deltas1)),
    ?assertEqual(Candidates3, enhance_candidates(Candidates2, Deltas2)).

extract_candidate_indexes_test_() ->
    {
        foreach,
        fun() ->
            meck:new(mango_idx, [passthrough]),
            meck:new(mango_idx_view, [passthrough])
        end,
        fun(_) ->
            meck:unload()
        end,
        [
            ?TDEF_FE(t_extract_candidate_indexes_empty),
            ?TDEF_FE(t_extract_candidate_indexes_singleton),
            ?TDEF_FE(t_extract_candidate_indexes_user_specified),
            ?TDEF_FE(t_extract_candidate_indexes_regular)
        ]
    }.

t_extract_candidate_indexes_empty(_) ->
    Indexes = sets:new(),
    UsabilityMap = [],
    Trace =
        #{
            all_indexes => Indexes,
            global_indexes => Indexes,
            partition_indexes => Indexes,
            usable_indexes => Indexes,
            usability_map => UsabilityMap,
            filtered_indexes => Indexes,
            indexes_of_type => Indexes
        },
    Cursor =
        #cursor{
            index = none,
            trace = Trace
        },
    Candidates = [],
    ?assertNot(meck:called(mango_idx, columns, '_')),
    ?assertEqual(Candidates, extract_candidate_indexes(Cursor)).

t_extract_candidate_indexes_singleton(_) ->
    Indexes = sets:from_list([winner]),
    UsabilityMap = [{winner, {true, #{reason => []}}}],
    Trace =
        #{
            all_indexes => Indexes,
            global_indexes => Indexes,
            partition_indexes => Indexes,
            usable_indexes => Indexes,
            usability_map => UsabilityMap,
            filtered_indexes => Indexes,
            indexes_of_type => Indexes
        },
    Cursor =
        #cursor{
            index = winner,
            trace = Trace
        },
    Candidates = [],
    meck:expect(mango_idx, columns, [winner], meck:val([column])),
    ?assertEqual(Candidates, extract_candidate_indexes(Cursor)).

t_extract_candidate_indexes_user_specified(_) ->
    Partial = #idx{type = <<"json">>, name = partial},
    Partitioned = #idx{type = <<"json">>, name = partitioned},
    NotUsable = #idx{type = <<"json">>, name = not_usable},
    Filtered = #idx{type = <<"json">>, name = filtered},
    Unfavored = #idx{type = <<"special">>, name = unfavored},
    UsabilityMap =
        [
            {winner, {true, #{reason => []}}},
            {NotUsable, {false, #{reason => [field_mismatch]}}},
            {Filtered, {true, #{reason => []}}},
            {Unfavored, {true, #{reason => []}}}
        ],
    Trace =
        #{
            all_indexes => sets:from_list([
                winner, Partial, Partitioned, NotUsable, Filtered, Unfavored
            ]),
            global_indexes => sets:from_list([winner, Partitioned, NotUsable, Filtered, Unfavored]),
            partition_indexes => sets:from_list([winner, NotUsable, Filtered, Unfavored]),
            usable_indexes => sets:from_list([winner, Filtered, Unfavored]),
            usability_map => UsabilityMap,
            filtered_indexes => sets:from_list([winner, Unfavored]),
            indexes_of_type => sets:from_list([winner])
        },
    Cursor =
        #cursor{
            index = winner,
            trace = Trace,
            fields = fields
        },
    meck:expect(mango_idx, columns, [winner], meck:val(all_fields)),
    meck:expect(mango_idx, to_json, fun(#idx{name = Name}) -> Name end),
    meck:expect(mango_idx_view, covers, fun(#idx{name = Name}, fields) -> Name end),
    Candidates =
        [
            {[
                {index, unfavored},
                {analysis,
                    {[
                        {usable, true},
                        {reasons, [{[{name, unfavored_type}]}]},
                        {ranking, 1},
                        {covering, null}
                    ]}}
            ]},
            {[
                {index, partitioned},
                {analysis,
                    {[
                        {usable, false},
                        {reasons, [{[{name, scope_mismatch}]}]},
                        {ranking, 4},
                        {covering, partitioned}
                    ]}}
            ]},
            {[
                {index, partial},
                {analysis,
                    {[
                        {usable, false},
                        {reasons, [{[{name, is_partial}]}]},
                        {ranking, 5},
                        {covering, partial}
                    ]}}
            ]},
            {[
                {index, not_usable},
                {analysis,
                    {[
                        {usable, false},
                        {reasons, [{[{name, field_mismatch}]}]},
                        {ranking, 3},
                        {covering, not_usable}
                    ]}}
            ]},
            {[
                {index, filtered},
                {analysis,
                    {[
                        {usable, true},
                        {reasons, [{[{name, excluded_by_user}]}]},
                        {ranking, 2},
                        {covering, filtered}
                    ]}}
            ]}
        ],
    ?assertEqual(Candidates, extract_candidate_indexes(Cursor)).

t_extract_candidate_indexes_regular(_) ->
    Partial1 = #idx{type = <<"json">>, name = partial1},
    Partial2 = #idx{type = <<"json">>, name = partial2},
    Partitioned1 = #idx{type = <<"json">>, name = partitioned1},
    Partitioned2 = #idx{type = <<"json">>, name = partitioned2},
    NotUsable = #idx{type = <<"json">>, name = not_usable},
    Unfavored1 = #idx{type = <<"special">>, name = unfavored1},
    Unfavored2 = #idx{type = <<"text">>, name = unfavored2},
    Usable1 = #idx{type = <<"json">>, name = usable1},
    Usable2 = #idx{type = <<"json">>, name = usable2},
    Usable3 = #idx{type = <<"json">>, name = usable3},
    UsabilityMap =
        [
            {winner, {true, #{reason => []}}},
            {NotUsable, {false, #{reason => [not_usable_reason]}}},
            {Unfavored1, {true, #{reason => []}}},
            {Unfavored2, {true, #{reason => []}}},
            {Usable1, {true, #{reason => []}}},
            {Usable2, {true, #{reason => []}}},
            {Usable3, {true, #{reason => []}}}
        ],
    SortedIndexRanges = [
        {winner, prefix0, 0}, {Usable1, prefix1, 1}, {Usable2, prefix2, 0}, {Usable3, prefix3, 0}
    ],
    Trace =
        #{
            all_indexes => sets:from_list([
                winner,
                Partial1,
                Partial2,
                Partitioned1,
                Partitioned2,
                NotUsable,
                Unfavored1,
                Unfavored2,
                Usable1,
                Usable2,
                Usable3
            ]),
            global_indexes => sets:from_list([
                winner,
                Partitioned1,
                Partitioned2,
                NotUsable,
                Unfavored1,
                Unfavored2,
                Usable1,
                Usable2,
                Usable3
            ]),
            partition_indexes => sets:from_list([
                winner, NotUsable, Unfavored1, Unfavored2, Usable1, Usable2, Usable3
            ]),
            usable_indexes => sets:from_list([
                winner, Unfavored1, Unfavored2, Usable1, Usable2, Usable3
            ]),
            usability_map => UsabilityMap,
            filtered_indexes => sets:from_list([
                winner, Unfavored1, Unfavored2, Usable1, Usable2, Usable3
            ]),
            indexes_of_type => sets:from_list([winner, Usable1, Usable2, Usable3]),
            sorted_index_ranges => SortedIndexRanges
        },
    Cursor =
        #cursor{
            index = winner,
            trace = Trace,
            fields = fields
        },
    meck:expect(
        mango_idx,
        columns,
        fun(Index) ->
            case Index of
                winner -> [column];
                Usable1 -> [column1, column2];
                Usable2 -> [column1, column2, column3];
                Usable3 -> [column]
            end
        end
    ),
    meck:expect(mango_idx, to_json, fun(#idx{name = Name}) -> Name end),
    meck:expect(mango_idx_view, covers, fun(#idx{name = Name}, fields) -> Name end),
    Candidates =
        [
            {[
                {index, usable3},
                {analysis,
                    {[
                        {usable, true},
                        {reasons, [{[{name, alphabetically_comes_after}]}]},
                        {ranking, 3},
                        {covering, usable3}
                    ]}}
            ]},
            {[
                {index, usable2},
                {analysis,
                    {[
                        {usable, true},
                        {reasons, [{[{name, too_many_fields}]}]},
                        {ranking, 2},
                        {covering, usable2}
                    ]}}
            ]},
            {[
                {index, usable1},
                {analysis,
                    {[
                        {usable, true},
                        {reasons, [{[{name, less_overlap}]}]},
                        {ranking, 1},
                        {covering, usable1}
                    ]}}
            ]},
            {[
                {index, unfavored2},
                {analysis,
                    {[
                        {usable, true},
                        {reasons, [{[{name, unfavored_type}]}]},
                        {ranking, 4},
                        {covering, null}
                    ]}}
            ]},
            {[
                {index, unfavored1},
                {analysis,
                    {[
                        {usable, true},
                        {reasons, [{[{name, unfavored_type}]}]},
                        {ranking, 4},
                        {covering, null}
                    ]}}
            ]},
            {[
                {index, partitioned2},
                {analysis,
                    {[
                        {usable, false},
                        {reasons, [{[{name, scope_mismatch}]}]},
                        {ranking, 6},
                        {covering, partitioned2}
                    ]}}
            ]},
            {[
                {index, partitioned1},
                {analysis,
                    {[
                        {usable, false},
                        {reasons, [{[{name, scope_mismatch}]}]},
                        {ranking, 6},
                        {covering, partitioned1}
                    ]}}
            ]},
            {[
                {index, partial2},
                {analysis,
                    {[
                        {usable, false},
                        {reasons, [{[{name, is_partial}]}]},
                        {ranking, 7},
                        {covering, partial2}
                    ]}}
            ]},
            {[
                {index, partial1},
                {analysis,
                    {[
                        {usable, false},
                        {reasons, [{[{name, is_partial}]}]},
                        {ranking, 7},
                        {covering, partial1}
                    ]}}
            ]},
            {[
                {index, not_usable},
                {analysis,
                    {[
                        {usable, false},
                        {reasons, [{[{name, not_usable_reason}]}]},
                        {ranking, 5},
                        {covering, not_usable}
                    ]}}
            ]}
        ],
    ?assertEqual(Candidates, extract_candidate_indexes(Cursor)).

extract_selector_hints_test_() ->
    {
        foreach,
        fun() ->
            ok
        end,
        fun(_) ->
            meck:unload()
        end,
        [
            ?TDEF_FE(t_extract_selector_hints_view),
            ?TDEF_FE(t_extract_selector_hints_text),
            ?TDEF_FE(t_extract_selector_hints_nouveau)
        ]
    }.

t_extract_selector_hints_view(_) ->
    meck:expect(dreyfus, available, [], meck:val(false)),
    meck:expect(nouveau, enabled, [], meck:val(false)),
    meck:expect(mango_selector, fields, [selector], meck:val(["field1", "field2", "field3"])),
    meck:expect(mango_idx_view, indexable_fields, [selector], meck:val(["field2"])),
    Hints =
        [
            {[
                {type, json},
                {indexable_fields, ["field2"]},
                {unindexable_fields, ["field3", "field1"]}
            ]}
        ],
    ?assertEqual(Hints, extract_selector_hints(selector)).

t_extract_selector_hints_text(_) ->
    meck:expect(dreyfus, available, [], meck:val(true)),
    meck:expect(nouveau, enabled, [], meck:val(false)),
    meck:expect(mango_selector, fields, [selector], meck:val(["field1", "field2", "field3"])),
    meck:expect(mango_idx_view, indexable_fields, [selector], meck:val(["field2"])),
    meck:expect(mango_idx_text, indexable_fields, [selector], meck:val(["field1"])),
    Hints =
        [
            {[
                {type, json},
                {indexable_fields, ["field2"]},
                {unindexable_fields, ["field3", "field1"]}
            ]},
            {[
                {type, text},
                {indexable_fields, ["field1"]},
                {unindexable_fields, ["field3", "field2"]}
            ]}
        ],
    ?assertEqual(Hints, extract_selector_hints(selector)).

t_extract_selector_hints_nouveau(_) ->
    meck:expect(dreyfus, available, [], meck:val(false)),
    meck:expect(nouveau, enabled, [], meck:val(true)),
    meck:expect(mango_selector, fields, [selector], meck:val(["field1", "field2", "field3"])),
    meck:expect(mango_idx_view, indexable_fields, [selector], meck:val(["field2"])),
    meck:expect(mango_idx_nouveau, indexable_fields, [selector], meck:val(["field1"])),
    Hints =
        [
            {[
                {type, json},
                {indexable_fields, ["field2"]},
                {unindexable_fields, ["field3", "field1"]}
            ]},
            {[
                {type, nouveau},
                {indexable_fields, ["field1"]},
                {unindexable_fields, ["field3", "field2"]}
            ]}
        ],
    ?assertEqual(Hints, extract_selector_hints(selector)).

explain_test_() ->
    {
        foreach,
        fun() ->
            meck:new(mango_idx, [passthrough]),
            meck:new(mango_idx_special, [passthrough])
        end,
        fun(_) ->
            meck:unload()
        end,
        [
            ?TDEF_FE(t_explain_empty),
            ?TDEF_FE(t_explain_regular)
        ]
    }.

t_explain_empty(_) ->
    Selector = {[]},
    Indexes = sets:new(),
    Trace =
        #{
            all_indexes => Indexes,
            global_indexes => Indexes,
            partition_indexes => Indexes,
            usable_indexes => Indexes,
            usability_map => [],
            filtered_indexes => Indexes,
            indexes_of_type => Indexes
        },
    Cursor =
        #cursor{
            db = db,
            index = none,
            selector = Selector,
            opts = [{user_ctx, user_ctx}, {fields, all_fields}],
            limit = limit,
            skip = skip,
            fields = all_fields,
            trace = Trace
        },
    Hints = [{[{type, json}, {indexable_fields, []}, {unindexable_fields, []}]}],
    Output =
        {[
            {dbname, db_name},
            {index, null},
            {partitioned, db_partitioned},
            {selector, Selector},
            {opts, {[{fields, []}]}},
            {limit, limit},
            {skip, skip},
            {fields, []},
            {index_candidates, []},
            {selector_hints, Hints}
        ]},
    meck:expect(dreyfus, available, [], meck:val(false)),
    meck:expect(nouveau, enabled, [], meck:val(false)),
    meck:expect(couch_db, name, [db], meck:val(db_name)),
    meck:expect(fabric_util, is_partitioned, [db], meck:val(db_partitioned)),
    ?assertNot(meck:called(mango_idx, to_json, '_')),
    ?assertEqual(Output, explain(Cursor)).

t_explain_regular(_) ->
    Index = #idx{
        type = <<"special">>, name = index, def = all_docs, dbname = db, partitioned = partitioned
    },
    Selector = {[]},
    Indexes = sets:from_list([Index]),
    Fields = some_fields,
    Trace =
        #{
            all_indexes => Indexes,
            global_indexes => Indexes,
            partition_indexes => Indexes,
            usable_indexes => Indexes,
            usability_map => [],
            filtered_indexes => Indexes,
            indexes_of_type => Indexes
        },
    Cursor =
        #cursor{
            db = db,
            index = Index,
            selector = Selector,
            opts = [{user_ctx, user_ctx}, {fields, Fields}],
            limit = limit,
            skip = skip,
            fields = Fields,
            trace = Trace
        },
    Hints = [{[{type, json}, {indexable_fields, []}, {unindexable_fields, []}]}],
    Output =
        {[
            {dbname, db_name},
            {index, index},
            {partitioned, db_partitioned},
            {selector, Selector},
            {opts, {[{fields, Fields}]}},
            {limit, limit},
            {skip, skip},
            {fields, Fields},
            {index_candidates, []},
            {selector_hints, Hints},
            special_explain
        ]},
    meck:expect(mango_idx, to_json, fun(#idx{name = Name}) -> Name end),
    meck:expect(mango_cursor_special, explain, [Cursor], meck:val([special_explain])),
    meck:expect(dreyfus, available, [], meck:val(false)),
    meck:expect(nouveau, enabled, [], meck:val(false)),
    meck:expect(couch_db, name, [db], meck:val(db_name)),
    meck:expect(fabric_util, is_partitioned, [db], meck:val(db_partitioned)),
    ?assertEqual(Output, explain(Cursor)).
-endif.
