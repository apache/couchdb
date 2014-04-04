-module(mango_query).

-export([
    prepare/3
]).


-record(q, {
    dbname,
    index,
    selector,
    ctx
}).


prepare(DbName, NonNormSelector, Ctx) ->
    Selector = mango_selector:normalize(NonNormSelector),
    IndexFields = mango_selector:index_fields(Selector),
    ExistingIndexes = mango_index:list(DbName, Ctx),
    UsableIndexes = find_usable_indexes(IndexFields, ExistingIndexes),
    FieldRanges = find_field_ranges(Selector, UsableIndexes),
    Composited = composite_indexes(UsableIndexes, FieldRanges),
    Index = choose_best_index(DbName, Composited),
    % Return a term here, or sketch out the full on
    % cursor machinery before attempting to run
    % this code?
    {ok, #q{
        dbname=DbName,
        index=Index,
        selector=Selector,
        ctx=Ctx
    }}.


% Find the intersection between the Possible and Existing
% indexes.
find_usable_indexes([], _) ->
    throw({no_usable_index, query_unsupported});
find_usable_indexes(Possible, []) ->
    throw({no_usable_index, Possible});
find_usable_indexes(_Possible, _Existing) ->
    % TODO: Write this:
    % ret = []
    % for index in existing:
    %    if index.columns[0] in possible:
    %       ret.append(index)
    % if len(ret) == 0:
    %    throw({no_usable_index, Possible})
    % return ret
    ok.


% For each field, return {Field, Range}
find_field_ranges(_Selector, []) ->
    [];
find_field_ranges(Selector, [Field | Rest]) ->
    Range = mango_selector:range(Selector, Field),
    % Derp?
    if Range /= empty -> ok; true ->
        throw({empty_query, {empty_field, Field}})
    end,
    [{Field, Range} | find_field_ranges(Selector, Rest)].


% Any of these indexes may be a composite index. For each
% index find the most specific set of fields for each
% index. Ie, if an index has columns a, b, c, d, then
% check FieldRanges for a, b, c, and d and return
% the longest prefix of columns found.
composite_indexes(_Indexes, _FieldRanges) ->
    % TODO: Write this:
    % ret = []
    % for idx in Indexes:
    %   ranges = [FieldRanges.get(idx.columns[0])]
    %   for c in idx.columns[1:]:
    %      rng = FieldRanges.find(c)
    %      if rng is None:
    %        break
    %      else:
    %        rangse.append(rng)
    %   ret.append((idx, ranges))
    % return ret
    ok.


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
                A1 =< B1;
            _ ->
                false
        end
    end,
    hd(lists:sort(Cmp, IndexRanges)).

