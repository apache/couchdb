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

-module(mango_idx_view).

-export([
    validate_new/2,
    validate_index_def/1,
    add/2,
    remove/2,
    from_ddoc/1,
    to_json/1,
    is_usable/3,
    columns/1,
    start_key/1,
    end_key/1,

    indexable_fields/1,
    field_ranges/1,
    field_ranges/2,

    covers/2
]).

-include_lib("couch/include/couch_db.hrl").
-include("mango.hrl").
-include("mango_idx_view.hrl").

validate_new(#idx{} = Idx, _Db) ->
    {ok, Def} = do_validate(Idx#idx.def),
    {ok, Idx#idx{def = Def}}.

validate_index_def(Def) ->
    def_to_json(Def).

add(#doc{body = {Props0}} = DDoc, Idx) ->
    Views1 =
        case proplists:get_value(<<"views">>, Props0) of
            {Views0} -> Views0;
            _ -> []
        end,
    NewView = make_view(Idx),
    Views2 = lists:keystore(element(1, NewView), 1, Views1, NewView),
    Props1 = lists:keystore(<<"views">>, 1, Props0, {<<"views">>, {Views2}}),
    {ok, DDoc#doc{body = {Props1}}}.

remove(#doc{body = {Props0}} = DDoc, Idx) ->
    Views1 =
        case proplists:get_value(<<"views">>, Props0) of
            {Views0} ->
                Views0;
            _ ->
                ?MANGO_ERROR({index_not_found, Idx#idx.name})
        end,
    Views2 = lists:keydelete(Idx#idx.name, 1, Views1),
    if
        Views2 /= Views1 -> ok;
        true -> ?MANGO_ERROR({index_not_found, Idx#idx.name})
    end,
    Props1 =
        case Views2 of
            [] ->
                lists:keydelete(<<"views">>, 1, Props0);
            _ ->
                lists:keystore(<<"views">>, 1, Props0, {<<"views">>, {Views2}})
        end,
    {ok, DDoc#doc{body = {Props1}}}.

from_ddoc({Props}) ->
    case lists:keyfind(<<"views">>, 1, Props) of
        {<<"views">>, {Views}} when is_list(Views) ->
            lists:flatmap(
                fun({Name, {VProps}}) ->
                    case validate_ddoc(VProps) of
                        invalid_view ->
                            [];
                        {Def, Opts} ->
                            I = #idx{
                                type = <<"json">>,
                                name = Name,
                                def = Def,
                                opts = Opts
                            },
                            [I]
                    end
                end,
                Views
            );
        _ ->
            []
    end.

to_json(Idx) ->
    {[
        {ddoc, Idx#idx.ddoc},
        {name, Idx#idx.name},
        {type, Idx#idx.type},
        {partitioned, Idx#idx.partitioned},
        {def, {def_to_json(Idx#idx.def)}}
    ]}.

columns(Idx) ->
    {Props} = Idx#idx.def,
    {<<"fields">>, {Fields}} = lists:keyfind(<<"fields">>, 1, Props),
    [Key || {Key, _} <- Fields].

-spec is_usable(#idx{}, selector(), [field()]) -> {boolean(), rejection_details()}.
is_usable(Idx, Selector, SortFields) ->
    % This index is usable if all of the columns are
    % restricted by the selector such that they are required to exist
    % and the selector is not a text search (so requires a text index)
    RequiredFields = columns(Idx),

    % sort fields are required to exist in the results so
    % we don't need to check the selector for these
    RequiredFields1 = ordsets:subtract(lists:usort(RequiredFields), lists:usort(SortFields)),

    % _id and _rev are implicitly in every document so
    % we don't need to check the selector for these either
    RequiredFields2 = ordsets:subtract(
        RequiredFields1,
        [<<"_id">>, <<"_rev">>]
    ),

    SelectorHasRequiredFields = mango_selector:has_required_fields(Selector, RequiredFields2),
    NotTextSearch = not is_text_search(Selector),
    CanUseSort = can_use_sort(RequiredFields, SortFields, Selector),
    Reason =
        [needs_text_search || not NotTextSearch] ++
            [field_mismatch || not SelectorHasRequiredFields] ++
            [sort_order_mismatch || not CanUseSort],
    Details = #{reason => Reason},
    {NotTextSearch andalso SelectorHasRequiredFields andalso CanUseSort, Details}.

is_text_search({[]}) ->
    false;
is_text_search({[{<<"$default">>, _}]}) ->
    true;
is_text_search({[{_Field, Cond}]}) when is_list(Cond) ->
    lists:foldl(
        fun(C, Exists) ->
            Exists orelse is_text_search(C)
        end,
        false,
        Cond
    );
is_text_search({[{_Field, Cond}]}) when is_tuple(Cond) ->
    is_text_search(Cond);
is_text_search({[{_Field, _Cond}]}) ->
    false;
%% we reached values, which should always be false
is_text_search(Val) when
    is_number(Val); is_boolean(Val); is_binary(Val)
->
    false.

start_key([]) ->
    [];
start_key([{'$gt', Key, _, _} | Rest]) ->
    case mango_json:special(Key) of
        true ->
            [];
        false ->
            [Key | start_key(Rest)]
    end;
start_key([{'$gte', Key, _, _} | Rest]) ->
    false = mango_json:special(Key),
    [Key | start_key(Rest)];
start_key([{'$eq', Key, '$eq', Key} | Rest]) ->
    false = mango_json:special(Key),
    [Key | start_key(Rest)].

end_key([]) ->
    [?MAX_JSON_OBJ];
end_key([{_, _, '$lt', Key} | Rest]) ->
    case mango_json:special(Key) of
        true ->
            [?MAX_JSON_OBJ];
        false ->
            [Key | end_key(Rest)]
    end;
end_key([{_, _, '$lte', Key} | Rest]) ->
    false = mango_json:special(Key),
    [Key | end_key(Rest)];
end_key([{'$eq', Key, '$eq', Key} | Rest]) ->
    false = mango_json:special(Key),
    [Key | end_key(Rest)].

do_validate({Props}) ->
    {ok, Opts} = mango_opts:validate(Props, opts()),
    {ok, {Opts}};
do_validate(Else) ->
    ?MANGO_ERROR({invalid_index_json, Else}).

def_to_json({Props}) ->
    def_to_json(Props);
def_to_json([]) ->
    [];
def_to_json([{fields, Fields} | Rest]) ->
    [{<<"fields">>, mango_sort:to_json(Fields)} | def_to_json(Rest)];
def_to_json([{<<"fields">>, Fields} | Rest]) ->
    [{<<"fields">>, mango_sort:to_json(Fields)} | def_to_json(Rest)];
% Don't include partial_filter_selector in the json conversion
% if its the default value
def_to_json([{<<"partial_filter_selector">>, {[]}} | Rest]) ->
    def_to_json(Rest);
def_to_json([{Key, Value} | Rest]) ->
    [{Key, Value} | def_to_json(Rest)].

opts() ->
    [
        {<<"fields">>, [
            {tag, fields},
            {validator, fun mango_opts:validate_sort/1}
        ]},
        {<<"partial_filter_selector">>, [
            {tag, partial_filter_selector},
            {optional, true},
            {default, {[]}},
            {validator, fun mango_opts:validate_selector/1}
        ]}
    ].

make_view(Idx) ->
    View =
        {[
            {<<"map">>, Idx#idx.def},
            {<<"reduce">>, <<"_count">>},
            {<<"options">>, {Idx#idx.opts}}
        ]},
    {Idx#idx.name, View}.

validate_ddoc(VProps) ->
    try
        Def = proplists:get_value(<<"map">>, VProps),
        validate_index_def(Def),
        {Opts0} = proplists:get_value(<<"options">>, VProps),
        Opts = lists:keydelete(<<"sort">>, 1, Opts0),
        {Def, Opts}
    catch
        Error:Reason ->
            couch_log:error(
                "Invalid Index Def ~p. Error: ~p, Reason: ~p",
                [VProps, Error, Reason]
            ),
            invalid_view
    end.

% This function returns a list of indexes that
% can be used to restrict this query. This works by
% searching the selector looking for field names that
% can be "seen".
%
% Operators that can be seen through are '$and' and any of
% the logical comparisons ('$lt', '$eq', etc). Things like
% '$regex', '$in', '$nin', and '$or' can't be serviced by
% a single index scan so we disallow them. In the future
% we may become more clever and increase our ken such that
% we will be able to see through these with crafty indexes
% or new uses for existing indexes. For instance, I could
% see an '$or' between comparisons on the same field becoming
% the equivalent of a multi-query. But that's for another
% day.

% We can see through '$and' trivially
indexable_fields({[{<<"$and">>, Args}]}) ->
    lists:usort(lists:flatten([indexable_fields(A) || A <- Args]));
% So far we can't see through any other operator
indexable_fields({[{<<"$", _/binary>>, _}]}) ->
    [];
% If we have a field with a terminator that is locatable
% using an index then the field is a possible index
indexable_fields({[{Field, Cond}]}) ->
    case indexable(Cond) of
        true ->
            [Field];
        false ->
            []
    end;
% An empty selector
indexable_fields({[]}) ->
    [].

% Check if a condition is indexable. The logical
% comparisons are mostly straight forward. We
% currently don't understand '$in' which is
% theoretically supportable. '$nin' and '$ne'
% aren't currently supported because they require
% multiple index scans.
indexable({[{<<"$lt">>, _}]}) ->
    true;
indexable({[{<<"$lte">>, _}]}) ->
    true;
indexable({[{<<"$eq">>, _}]}) ->
    true;
indexable({[{<<"$gt">>, _}]}) ->
    true;
indexable({[{<<"$gte">>, _}]}) ->
    true;
indexable({[{<<"$beginsWith">>, _}]}) ->
    true;
% This is required to improve index selection for covering indexes.
% Making `$exists` indexable should not cause problems in other cases.
indexable({[{<<"$exists">>, _}]}) ->
    true;
% All other operators are currently not indexable.
% This is also a subtle assertion that we don't
% call indexable/1 on a field name.
indexable({[{<<"$", _/binary>>, _}]}) ->
    false.

% For each field, return {Field, Range}
field_ranges(Selector) ->
    Fields = indexable_fields(Selector),
    field_ranges(Selector, Fields).

field_ranges(Selector, Fields) ->
    field_ranges(Selector, Fields, []).

field_ranges(_Selector, [], Acc) ->
    lists:reverse(Acc);
field_ranges(Selector, [Field | Rest], Acc) ->
    case range(Selector, Field) of
        empty ->
            [{Field, empty}];
        Range ->
            field_ranges(Selector, Rest, [{Field, Range} | Acc])
    end.

% Find the complete range for a given index in this
% selector. This works by AND'ing logical comparisons
% together so that we can define the start and end
% keys for a given index.
%
% Selector must have been normalized before calling
% this function.
range(Selector, Index) ->
    range(Selector, Index, '$gt', mango_json:min(), '$lt', mango_json:max()).

% Adjust Low and High based on values found for the
% given Index in Selector.
range({[{<<"$and">>, Args}]}, Index, LCmp, Low, HCmp, High) ->
    lists:foldl(
        fun
            (Arg, {LC, L, HC, H}) ->
                range(Arg, Index, LC, L, HC, H);
            (_Arg, empty) ->
                empty
        end,
        {LCmp, Low, HCmp, High},
        Args
    );
% We can currently only traverse '$and' operators
range({[{<<"$", _/binary>>}]}, _Index, LCmp, Low, HCmp, High) ->
    {LCmp, Low, HCmp, High};
% If the field name matches the index see if we can narrow
% the acceptable range.
range({[{Index, Cond}]}, Index, LCmp, Low, HCmp, High) ->
    range(Cond, LCmp, Low, HCmp, High);
% Else we have a field unrelated to this index so just
% return the current values.
range(_, _, LCmp, Low, HCmp, High) ->
    {LCmp, Low, HCmp, High}.

% The comments below are a bit cryptic at first but they show
% where the Arg cand land in the current range.
%
% For instance, given:
%
%     {$lt: N}
%     Low = 1
%     High = 5
%
% Depending on the value of N we can have one of five locations
% in regards to a given Low/High pair:
%
%     min low mid high max
%
%   That is:
%       min = (N < Low)
%       low = (N == Low)
%       mid = (Low < N < High)
%       high = (N == High)
%       max = (High < N)
%
% If N < 1, (min) then the effective range is empty.
%
% If N == 1, (low) then we have to set the range to empty because
% N < 1 && N >= 1 is an empty set. If the operator had been '$lte'
% and LCmp was '$gte' or '$eq' then we could keep around the equality
% check on Arg by setting LCmp == HCmp = '$eq' and Low == High == Arg.
%
% If 1 < N < 5 (mid), then we set High to Arg and Arg has just
% narrowed our range. HCmp is set the '$lt' operator that was
% part of the input.
%
% If N == 5 (high), We just set HCmp to '$lt' since its guaranteed
% to be equally or more restrictive than the current possible values
% of '$lt' or '$lte'.
%
% If N > 5 (max), nothing changes as our current range is already
% more narrow than the current condition.
%
% Obviously all of that logic gets tweaked for the other logical
% operators but its all straight forward once you figure out how
% we're basically just narrowing our logical ranges.

% beginsWith requires both a high and low bound
range({[{<<"$beginsWith">>, Arg}]}, LCmp, Low, HCmp, High) ->
    {LCmp0, Low0, HCmp0, High0} = range({[{<<"$gte">>, Arg}]}, LCmp, Low, HCmp, High),
    % U+FFFF is the highest sorting code point according to the collator rules,
    % even though it's not the highest code point in UTF8.
    range({[{<<"$lte">>, <<Arg/binary, 16#FFFF/utf8>>}]}, LCmp0, Low0, HCmp0, High0);
range({[{<<"$lt">>, Arg}]}, LCmp, Low, HCmp, High) ->
    case range_pos(Low, Arg, High) of
        min ->
            empty;
        low ->
            empty;
        mid ->
            {LCmp, Low, '$lt', Arg};
        high ->
            {LCmp, Low, '$lt', Arg};
        max ->
            {LCmp, Low, HCmp, High}
    end;
range({[{<<"$lte">>, Arg}]}, LCmp, Low, HCmp, High) ->
    case range_pos(Low, Arg, High) of
        min ->
            empty;
        low when LCmp == '$gte'; LCmp == '$eq' ->
            {'$eq', Arg, '$eq', Arg};
        low ->
            empty;
        mid ->
            {LCmp, Low, '$lte', Arg};
        high ->
            {LCmp, Low, HCmp, High};
        max ->
            {LCmp, Low, HCmp, High}
    end;
range({[{<<"$eq">>, Arg}]}, LCmp, Low, HCmp, High) ->
    case range_pos(Low, Arg, High) of
        min ->
            empty;
        low when LCmp == '$gte'; LCmp == '$eq' ->
            {'$eq', Arg, '$eq', Arg};
        low ->
            empty;
        mid ->
            {'$eq', Arg, '$eq', Arg};
        high when HCmp == '$lte'; HCmp == '$eq' ->
            {'$eq', Arg, '$eq', Arg};
        high ->
            empty;
        max ->
            empty
    end;
range({[{<<"$gte">>, Arg}]}, LCmp, Low, HCmp, High) ->
    case range_pos(Low, Arg, High) of
        min ->
            {LCmp, Low, HCmp, High};
        low ->
            {LCmp, Low, HCmp, High};
        mid ->
            {'$gte', Arg, HCmp, High};
        high when HCmp == '$lte'; HCmp == '$eq' ->
            {'$eq', Arg, '$eq', Arg};
        high ->
            empty;
        max ->
            empty
    end;
range({[{<<"$gt">>, Arg}]}, LCmp, Low, HCmp, High) ->
    case range_pos(Low, Arg, High) of
        min ->
            {LCmp, Low, HCmp, High};
        low ->
            {'$gt', Arg, HCmp, High};
        mid ->
            {'$gt', Arg, HCmp, High};
        high ->
            empty;
        max ->
            empty
    end;
% There's some other un-indexable restriction on the index
% that will be applied as a post-filter. Ignore it and
% carry on our merry way.
range({[{<<"$", _/binary>>, _}]}, LCmp, Low, HCmp, High) ->
    {LCmp, Low, HCmp, High}.

% Returns the value min | low | mid | high | max depending
% on how Arg compares to Low and High.
range_pos(Low, Arg, High) ->
    case mango_json:cmp(Arg, Low) of
        N when N < 0 -> min;
        N when N == 0 -> low;
        _ ->
            case mango_json:cmp(Arg, High) of
                X when X < 0 ->
                    mid;
                X when X == 0 ->
                    high;
                _ ->
                    max
            end
    end.

% Can_use_sort works as follows:
%
% * no sort fields then we can use this
% * Run out index columns we can't use this index
% * If the current column is the start of the sort, return if sort is a prefix
% * If the current column is constant, drop it and continue, else return false
%
% A constant column is a something that won't affect the sort
% for example A: {$eq: 21}}
%
% Currently we only look at constant fields that are prefixes to the sort fields
% set by the user. We considered adding in constant fields after sort fields
% but were not 100% sure that it would not affect the sorting of the query.

can_use_sort(_Cols, [], _Selector) ->
    true;
can_use_sort([], _SortFields, _Selector) ->
    false;
can_use_sort([Col | _] = Cols, [Col | _] = SortFields, _Selector) ->
    lists:prefix(SortFields, Cols);
can_use_sort([Col | RestCols], SortFields, Selector) ->
    case mango_selector:is_constant_field(Selector, Col) of
        true -> can_use_sort(RestCols, SortFields, Selector);
        false -> false
    end.

% There is no information available about the full set of fields which
% comes the following consequences: an index cannot (reliably) cover
% an "all fields" type of query and nested fields are out of scope.
-spec covers(#idx{}, fields()) -> boolean().
covers(_, all_fields) ->
    false;
covers(Idx, Fields) ->
    case mango_idx:def(Idx) of
        all_docs ->
            false;
        _ ->
            Available = [<<"_id">> | columns(Idx)],
            sets:is_subset(couch_util:set_from_list(Fields), couch_util:set_from_list(Available))
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

indexable_fields_of(Selector) ->
    indexable_fields(test_util:as_selector(Selector)).

indexable_fields_empty_test() ->
    ?assertEqual([], indexable_fields_of(#{})).

indexable_fields_and_test() ->
    Selector = #{<<"$and">> => [#{<<"field1">> => undefined, <<"field2">> => undefined}]},
    ?assertEqual([<<"field1">>, <<"field2">>], indexable_fields_of(Selector)).

indexable_fields_or_test() ->
    Selector = #{<<"$or">> => [#{<<"field1">> => undefined, <<"field2">> => undefined}]},
    ?assertEqual([], indexable_fields_of(Selector)).

indexable_fields_nor_test() ->
    Selector = #{<<"$nor">> => [#{<<"field1">> => undefined, <<"field2">> => undefined}]},
    ?assertEqual([], indexable_fields_of(Selector)).

indexable_fields_all_test() ->
    Selector = #{<<"field">> => #{<<"$all">> => []}},
    ?assertEqual([], indexable_fields_of(Selector)).

indexable_fields_elemMatch_test() ->
    Selector = #{<<"field">> => #{<<"$elemMatch">> => #{}}},
    ?assertEqual([], indexable_fields_of(Selector)).

indexable_fields_allMatch_test() ->
    Selector = #{<<"field">> => #{<<"$allMatch">> => #{}}},
    ?assertEqual([], indexable_fields_of(Selector)).

indexable_fields_keyMapMatch_test() ->
    Selector = #{<<"field">> => #{<<"$keyMapMatch">> => #{}}},
    ?assertEqual([], indexable_fields_of(Selector)).

indexable_fields_in_test() ->
    Selector = #{<<"field">> => #{<<"$in">> => []}},
    ?assertEqual([], indexable_fields_of(Selector)).

indexable_fields_nin_test() ->
    Selector = #{<<"field">> => #{<<"$nin">> => []}},
    ?assertEqual([], indexable_fields_of(Selector)).

indexable_fields_not_test() ->
    Selector = #{<<"field">> => #{<<"$not">> => #{}}},
    ?assertEqual([], indexable_fields_of(Selector)).

indexable_fields_lt_test() ->
    Selector = #{<<"field">> => #{<<"$lt">> => undefined}},
    ?assertEqual([<<"field">>], indexable_fields_of(Selector)).

indexable_fields_lte_test() ->
    Selector = #{<<"field">> => #{<<"$lte">> => undefined}},
    ?assertEqual([<<"field">>], indexable_fields_of(Selector)).

indexable_fields_eq_test() ->
    Selector = #{<<"field">> => #{<<"$eq">> => undefined}},
    ?assertEqual([<<"field">>], indexable_fields_of(Selector)).

indexable_fields_ne_test() ->
    Selector = #{<<"field">> => #{<<"$ne">> => undefined}},
    ?assertEqual([], indexable_fields_of(Selector)).

indexable_fields_gte_test() ->
    Selector = #{<<"field">> => #{<<"$gte">> => undefined}},
    ?assertEqual([<<"field">>], indexable_fields_of(Selector)).

indexable_fields_beginswith_test() ->
    Selector = #{<<"field">> => #{<<"$beginsWith">> => undefined}},
    ?assertEqual([<<"field">>], indexable_fields_of(Selector)).

indexable_fields_gt_test() ->
    Selector = #{<<"field">> => #{<<"$gt">> => undefined}},
    ?assertEqual([<<"field">>], indexable_fields_of(Selector)).

indexable_fields_mod_test() ->
    Selector = #{<<"field">> => #{<<"$mod">> => [0, 0]}},
    ?assertEqual([], indexable_fields_of(Selector)).

indexable_fields_regex_test() ->
    Selector = #{<<"field">> => #{<<"$regex">> => undefined}},
    ?assertEqual([], indexable_fields_of(Selector)).

indexable_fields_exists_test() ->
    Selector = #{<<"field">> => #{<<"$exists">> => true}},
    ?assertEqual([<<"field">>], indexable_fields_of(Selector)).

indexable_fields_type_test() ->
    Selector = #{<<"field">> => #{<<"$type">> => undefined}},
    ?assertEqual([], indexable_fields_of(Selector)).

indexable_fields_size_test() ->
    Selector = #{<<"field">> => #{<<"$size">> => 0}},
    ?assertEqual([], indexable_fields_of(Selector)).

covers_all_fields_test() ->
    ?assertNot(covers(undefined, all_fields)).

covers_all_docs_test() ->
    ?assertNot(covers(#idx{def = all_docs}, undefined)).

covers_empty_index_test() ->
    Index = #idx{def = {[{<<"fields">>, {[]}}]}},
    ?assert(covers(Index, [])),
    ?assert(covers(Index, [<<"_id">>])).

covers_regular_index_test() ->
    Index = #idx{def = {[{<<"fields">>, {[{field1, undefined}, {field2, undefined}]}}]}},
    ?assert(covers(Index, [])),
    ?assert(covers(Index, [<<"_id">>])),
    ?assert(covers(Index, [field1])),
    ?assert(covers(Index, [field2, field1])),
    ?assert(covers(Index, [<<"_id">>, field1, field2])),
    ?assertNot(covers(Index, [field3, field1, field2])).

usable(Index, undefined, Fields) ->
    is_usable(Index, undefined, Fields);
usable(Index, Selector, Fields) ->
    is_usable(Index, test_util:as_selector(Selector), Fields).

is_usable_test() ->
    Usable = {true, #{reason => []}},
    NeedsTextSearch = {false, #{reason => [needs_text_search, field_mismatch]}},
    FieldMismatch = {false, #{reason => [field_mismatch]}},
    SortOrderMismatch = {false, #{reason => [sort_order_mismatch]}},

    Index = #idx{
        def = {[{<<"fields">>, {[{<<"field1">>, undefined}, {<<"field2">>, undefined}]}}]}
    },
    ?assertEqual(FieldMismatch, usable(Index, #{}, [])),
    ?assertEqual(FieldMismatch, usable(Index, #{<<"field1">> => <<"value1">>}, [])),
    ?assertEqual(
        NeedsTextSearch,
        usable(
            Index,
            #{
                <<"$or">> => [
                    #{<<"$text">> => <<"foobar">>},
                    #{<<"field1">> => <<"value1">>}
                ],
                <<"field2">> => 42
            },
            []
        )
    ),
    ?assertEqual(
        SortOrderMismatch,
        usable(Index, #{<<"field1">> => <<"value1">>, <<"field2">> => 42}, [
            <<"field3">>, <<"field4">>
        ])
    ),
    ?assertEqual(
        Usable, usable(Index, #{<<"field1">> => <<"value1">>, <<"field2">> => <<"value2">>}, [])
    ).
-endif.
