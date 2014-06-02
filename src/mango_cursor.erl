-module(mango_cursor).


-export([
    create/3,
    execute/3
]).


-include_lib("couch/include/couch_db.hrl").
-include("mango.hrl").
-include("mango_cursor.hrl").


-define(SUPERVISOR, mango_cursor_sup).


create(Db, Selector0, Opts) ->
    Selector = mango_selector:normalize(Selector0),
    IndexFields = mango_selector:index_fields(Selector),    
    FieldRanges = find_field_ranges(Selector, IndexFields),

    ExistingIndexes = mango_idx:list(Db),
    UsableIndexes = find_usable_indexes(IndexFields, ExistingIndexes),
    SortIndexes = get_sort_indexes(ExistingIndexes, UsableIndexes, Opts),

    Composited = composite_indexes(SortIndexes, FieldRanges),
    {Index, Ranges} = choose_best_index(Db, Composited),

    Limit = couch_util:get_value(limit, Opts, 10000000000),
    Skip = couch_util:get_value(skip, Opts, 0),
    Fields = couch_util:get_value(fields, Opts, all_fields),

    {ok, #cursor{
        db = Db,
        index = Index,
        ranges = Ranges,
        selector = Selector,
        opts = Opts,
        limit = Limit,
        skip = Skip,
        fields = Fields
    }}.


execute(#cursor{index=Idx}=Cursor, UserFun, UserAcc) ->
    Mod = mango_idx:cursor_mod(Idx),
    Mod:execute(Cursor, UserFun, UserAcc).


% Find the intersection between the Possible and Existing
% indexes.
find_usable_indexes([], _) ->
    ?MANGO_ERROR({no_usable_index, query_unsupported});
find_usable_indexes(Possible, []) ->
    ?MANGO_ERROR({no_usable_index, {fields, Possible}});
find_usable_indexes(Possible, Existing) ->
    Usable = lists:foldl(fun(Idx, Acc) ->
        [Col0 | _] = mango_idx:columns(Idx),
        case lists:member(Col0, Possible) of
            true ->
                [Idx | Acc];
            false ->
                Acc
        end
    end, [], Existing),
    if length(Usable) > 0 -> ok; true ->
        ?MANGO_ERROR({no_usable_index, {fields, Possible}})
    end,
    Usable.


get_sort_indexes(ExistingIndexes, UsableIndexes, Opts) ->
    % If a sort was specified we have to find an index that
    % can satisfy the request.
    case lists:keyfind(sort, 1, Opts) of
        {sort, {[_ | _]} = Sort} ->
            limit_to_sort(ExistingIndexes, UsableIndexes, Sort);
        _ ->
            UsableIndexes
    end.


limit_to_sort(ExistingIndexes, UsableIndexes, Sort) ->
    Fields = mango_sort:fields(Sort),

    % First make sure that we have an index that could
    % answer this sort. We split like this so that the
    % user error is more obvious.
    SortFilt = fun(Idx) ->
        Cols = mango_idx:columns(Idx),
        lists:prefix(Fields, Cols)
    end,
    SortIndexes = lists:filter(SortFilt, ExistingIndexes),
    if SortIndexes /= [] -> ok; true ->
        ?MANGO_ERROR({no_usable_index, {sort, Fields}})
    end,

    % And then check if one or more of our SortIndexes
    % is usable.
    UsableFilt = fun(Idx) -> lists:member(Idx, UsableIndexes) end,
    FinalIndexes = lists:filter(UsableFilt, SortIndexes),
    if FinalIndexes /= [] -> ok; true ->
        ?MANGO_ERROR({no_usable_index, sort_field})
    end,

    FinalIndexes.


% For each field, return {Field, Range}
find_field_ranges(Selector, Fields) ->
    find_field_ranges(Selector, Fields, []).

find_field_ranges(_Selector, [], Acc) ->
    lists:reverse(Acc);
find_field_ranges(Selector, [Field | Rest], Acc) ->
    case mango_selector:range(Selector, Field) of
        empty ->
            [{Field, empty}];
        Range ->
            find_field_ranges(Selector, Rest, [{Field, Range} | Acc])
    end.


% Any of these indexes may be a composite index. For each
% index find the most specific set of fields for each
% index. Ie, if an index has columns a, b, c, d, then
% check FieldRanges for a, b, c, and d and return
% the longest prefix of columns found.
composite_indexes(Indexes, FieldRanges) ->
    lists:foldl(fun(Idx, Acc) ->
        Cols = mango_idx:columns(Idx),
        Prefix = composite_prefix(Cols, FieldRanges),
        [{Idx, Prefix} | Acc]
    end, [], Indexes).


composite_prefix([], _) ->
    [];
composite_prefix([Col | Rest], Ranges) ->
    case lists:keyfind(Col, 1, Ranges) of
        {Col, Range} ->
            [Range | composite_prefix(Rest, Ranges)];
        false ->
            []
    end.


% Low and behold our query planner. Or something.
% So stupid, but we can fix this up later. First
% pass: Sort the IndexRanges by (num_columns, idx_name)
% and return the first element. Yes. Its going to
% be that dumb for now.
%
% In the future we can look into doing a cached parallel
% reduce view read on each index with the ranges to find
% the one that has the fewest number of rows or something.
choose_best_index(_DbName, IndexRanges) ->
    Cmp = fun({A1, A2}, {B1, B2}) ->
        case length(A2) - length(B2) of
            N when N < 0 -> true;
            N when N == 0 ->
                % This is a really bad sort and will end
                % up preferring indices based on the
                % (dbname, ddocid, view_name) triple
                A1 =< B1;
            _ ->
                false
        end
    end,
    hd(lists:sort(Cmp, IndexRanges)).
