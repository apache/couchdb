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

-module(mango_selector_text).

-export([
    convert/1,
    convert/2,

    append_sort_type/2
]).

-include("mango.hrl").

%% Regex for <<"\\.">>
-define(PERIOD, "\\.").

-spec convert(SelectorObject) -> LuceneQueryString when
    SelectorObject :: any(),
    LuceneQueryString :: binary().
convert(Object) ->
    TupleTree = convert([], Object),
    iolist_to_binary(to_query(TupleTree)).

-spec convert(CurrentPath, SelectorObject) -> abstract_text_selector() when
    CurrentPath :: [binary()],
    SelectorObject :: any().
convert(Path, {[{<<"$and">>, Args}]}) ->
    Parts = [convert(Path, Arg) || Arg <- Args],
    {op_and, Parts};
convert(Path, {[{<<"$or">>, Args}]}) ->
    Parts = [convert(Path, Arg) || Arg <- Args],
    {op_or, Parts};
convert(Path, {[{<<"$not">>, Arg}]}) ->
    {op_not, {field_exists_query(Path), convert(Path, Arg)}};
convert(Path, {[{<<"$default">>, Arg}]}) ->
    {op_field, {_, Query}} = convert(Path, Arg),
    {op_default, Query};
% The $text operator specifies a Lucene syntax query
% so we just pull it in directly.
convert(Path, {[{<<"$text">>, Query}]}) when is_binary(Query) ->
    {op_field, {make_field(Path, Query), value_str(Query)}};
% The MongoDB docs for $all are super confusing and read more
% like they screwed up the implementation of this operator
% and then just documented it as a feature.
%
% This implementation will match the behavior as closely as
% possible based on the available docs but we'll need to have
% the testing team validate how MongoDB handles edge conditions
convert(Path, {[{<<"$all">>, Args}]}) ->
    case Args of
        [Values] when is_list(Values) ->
            % If Args is a single element array then we have to
            % either match if Path is that array or if it contains
            % the array as an element of an array (which isn't at all
            % confusing). For Lucene to return us all possible matches
            % that means we just need to search for each value in
            % Path.[] and Path.[].[] and rely on our filtering to limit
            % the results properly.
            Fields1 = convert(Path, {[{<<"$eq">>, Values}]}),
            Fields2 = convert([<<"[]">> | Path], {[{<<"$eq">>, Values}]}),
            {op_or, [Fields1, Fields2]};
        _ ->
            % Otherwise the $all operator is equivalent to an $and
            % operator so we treat it as such.
            convert([<<"[]">> | Path], {[{<<"$and">>, Args}]})
    end;
% The $elemMatch Lucene query is not an exact translation
% as we can't enforce that the matches are all for the same
% item in an array. We just rely on the final selector match
% to filter out anything that doesn't match. The only trick
% is that we have to add the `[]` path element since the docs
% say this has to match against an array.
convert(Path, {[{<<"$elemMatch">>, Arg}]}) ->
    convert([<<"[]">> | Path], Arg);
convert(Path, {[{<<"$allMatch">>, Arg}]}) ->
    convert([<<"[]">> | Path], Arg);
% Our comparison operators are fairly straight forward
convert(Path, {[{<<"$lt">>, Arg}]}) when
    is_list(Arg);
    is_tuple(Arg);
    Arg =:= null
->
    field_exists_query(Path);
convert(Path, {[{<<"$lt">>, Arg}]}) ->
    {op_field, {make_field(Path, Arg), range(lt, Arg)}};
convert(Path, {[{<<"$lte">>, Arg}]}) when
    is_list(Arg);
    is_tuple(Arg);
    Arg =:= null
->
    field_exists_query(Path);
convert(Path, {[{<<"$lte">>, Arg}]}) ->
    {op_field, {make_field(Path, Arg), range(lte, Arg)}};
%% This is for indexable_fields
convert(Path, {[{<<"$eq">>, Arg}]}) when Arg =:= null ->
    {op_null, {make_field(Path, Arg), value_str(Arg)}};
convert(Path, {[{<<"$eq">>, Args}]}) when is_list(Args) ->
    Path0 = [<<"[]">> | Path],
    LPart = {op_field, {make_field(Path0, length), value_str(length(Args))}},
    Parts0 = [convert(Path0, {[{<<"$eq">>, Arg}]}) || Arg <- Args],
    Parts = [LPart | Parts0],
    {op_and, Parts};
convert(Path, {[{<<"$eq">>, {_} = Arg}]}) ->
    convert(Path, Arg);
convert(Path, {[{<<"$eq">>, Arg}]}) ->
    {op_field, {make_field(Path, Arg), value_str(Arg)}};
convert(Path, {[{<<"$ne">>, Arg}]}) ->
    {op_not, {field_exists_query(Path), convert(Path, {[{<<"$eq">>, Arg}]})}};
convert(Path, {[{<<"$gte">>, Arg}]}) when
    is_list(Arg);
    is_tuple(Arg);
    Arg =:= null
->
    field_exists_query(Path);
convert(Path, {[{<<"$gte">>, Arg}]}) ->
    {op_field, {make_field(Path, Arg), range(gte, Arg)}};
convert(Path, {[{<<"$gt">>, Arg}]}) when
    is_list(Arg);
    is_tuple(Arg);
    Arg =:= null
->
    field_exists_query(Path);
convert(Path, {[{<<"$gt">>, Arg}]}) ->
    {op_field, {make_field(Path, Arg), range(gt, Arg)}};
convert(Path, {[{<<"$in">>, Args}]}) ->
    {op_or, convert_in(Path, Args)};
convert(Path, {[{<<"$nin">>, Args}]}) ->
    {op_not, {field_exists_query(Path), convert(Path, {[{<<"$in">>, Args}]})}};
convert(Path, {[{<<"$exists">>, ShouldExist}]}) ->
    FieldExists = field_exists_query(Path),
    case ShouldExist of
        true -> FieldExists;
        false -> {op_not, {FieldExists, false}}
    end;
convert(Path, {[{<<"$beginsWith">>, Arg}]}) when is_binary(Arg) ->
    Prefix = mango_util:lucene_escape_query_value(Arg),
    Suffix = <<"*">>,
    PrefixSearch = <<Prefix/binary, Suffix/binary>>,
    {op_field, {make_field(Path, Arg), PrefixSearch}};
% We're not checking the actual type here, just looking for
% anything that has a possibility of matching by checking
% for the field name. We use the same logic for $exists on
% the actual query.
convert(Path, {[{<<"$type">>, _}]}) ->
    field_exists_query(Path);
convert(Path, {[{<<"$mod">>, _}]}) ->
    field_exists_query(Path, "number");
% The lucene regular expression engine does not use java's regex engine but
% instead a custom implementation. The syntax is therefore different, so we do
% would get different behavior than our view indexes. To be consistent, we will
% simply return docs for fields that exist and then run our match filter.
%
% It has a dedicated type so that `mango_idx_text:indexable_fields/1`
% could handle this case properly.
convert(Path, {[{<<"$regex">>, _}]}) ->
    {op_regex, path_str(Path)};
convert(Path, {[{<<"$size">>, Arg}]}) ->
    {op_field, {make_field([<<"[]">> | Path], length), value_str(Arg)}};
% All other operators are internal assertion errors for
% matching because we either should've removed them during
% normalization or something else broke.
convert(_Path, {[{<<"$", _/binary>> = Op, _}]}) ->
    ?MANGO_ERROR({invalid_operator, Op});
% We've hit a field name specifier. Check if the field name is accessing
% arrays. Convert occurrences of element position references to .[]. Then we
% need to break the name into path parts and continue our conversion.
convert(Path, {[{Field0, Cond}]}) ->
    {ok, PP0} =
        case Field0 of
            <<>> ->
                {ok, []};
            _ ->
                mango_util:parse_field(Field0)
        end,
    % Later on, we perform a lucene_escape_user call on the
    % final Path, which calls parse_field again. Calling the function
    % twice converts <<"a\\.b">> to [<<"a">>,<<"b">>]. This leads to
    % an incorrect query since we need [<<"a.b">>]. Without breaking
    % our escaping mechanism, we simply revert this first parse_field
    % effect and replace instances of "." to "\\.".
    MP = mango_util:cached_re(mango_period, ?PERIOD),
    PP1 = [
        re:replace(
            P,
            MP,
            <<"\\\\.">>,
            [global, {return, binary}]
        )
     || P <- PP0
    ],
    {PP2, HasInteger} = replace_array_indexes(PP1, [], false),
    NewPath = PP2 ++ Path,
    case HasInteger of
        true ->
            OldPath = lists:reverse(PP1, Path),
            OldParts = convert(OldPath, Cond),
            NewParts = convert(NewPath, Cond),
            {op_or, [OldParts, NewParts]};
        false ->
            convert(NewPath, Cond)
    end;
%% For $in
convert(Path, Val) when is_binary(Val); is_number(Val); is_boolean(Val) ->
    {op_field, {make_field(Path, Val), value_str(Val)}};
% Anything else is a bad selector.
convert(_Path, {Props} = Sel) when length(Props) > 1 ->
    erlang:error({unnormalized_selector, Sel}).

to_query_nested(Args) ->
    QueryArgs = lists:map(fun to_query/1, Args),
    % removes empty queries that result from selectors with empty arrays
    FilterFun = fun(A) -> A =/= [] andalso A =/= "()" end,
    lists:filter(FilterFun, QueryArgs).

-spec to_query(abstract_text_selector()) -> LuceneQueryStringPieces when
    LuceneQueryStringPieces :: [binary()].
to_query({op_and, []}) ->
    [];
to_query({op_and, Args}) when is_list(Args) ->
    case to_query_nested(Args) of
        [] -> [];
        QueryArgs -> ["(", mango_util:join(<<" AND ">>, QueryArgs), ")"]
    end;
to_query({op_or, []}) ->
    [];
to_query({op_or, Args}) when is_list(Args) ->
    case to_query_nested(Args) of
        [] -> [];
        QueryArgs -> ["(", mango_util:join(" OR ", QueryArgs), ")"]
    end;
to_query({op_not, {ExistsQuery, Arg}}) when is_tuple(Arg) ->
    case to_query(Arg) of
        [] -> ["(", to_query(ExistsQuery), ")"];
        Query -> ["(", to_query(ExistsQuery), " AND NOT (", Query, "))"]
    end;
%% For $exists:false
to_query({op_not, {ExistsQuery, false}}) ->
    ["($fieldnames:/.*/ ", " AND NOT (", to_query(ExistsQuery), "))"];
%% We escape : and / for now for values and all lucene chars for fieldnames
%% This needs to be resolved.
to_query({op_field, {Name, Value}}) ->
    NameBin = iolist_to_binary(Name),
    ["(", mango_util:lucene_escape_user(NameBin), ":", Value, ")"];
%% This is for indexable_fields
to_query({op_null, {Name, Value}}) ->
    NameBin = iolist_to_binary(Name),
    ["(", mango_util:lucene_escape_user(NameBin), ":", Value, ")"];
to_query({op_fieldname, {Name, Wildcard}}) ->
    NameBin = iolist_to_binary(Name),
    ["($fieldnames:", mango_util:lucene_escape_user(NameBin), Wildcard, ")"];
%% This is for indexable_fields
to_query({op_regex, Name}) ->
    NameBin = iolist_to_binary([Name, ":"]),
    ["($fieldnames:", mango_util:lucene_escape_user(NameBin), "string)"];
to_query({op_default, Value}) ->
    ["($default:", Value, ")"].

%% We match on fieldname and fieldname.[]
convert_in(Path, Args) ->
    Path0 = [<<"[]">> | Path],
    lists:map(
        fun(Arg) ->
            case Arg of
                {Object} ->
                    Parts = lists:map(
                        fun(SubObject) ->
                            Fields1 = convert(Path, {[SubObject]}),
                            Fields2 = convert(Path0, {[SubObject]}),
                            {op_or, [Fields1, Fields2]}
                        end,
                        Object
                    ),
                    {op_or, Parts};
                SingleVal ->
                    Fields1 = {op_field, {make_field(Path, SingleVal), value_str(SingleVal)}},
                    Fields2 = {op_field, {make_field(Path0, SingleVal), value_str(SingleVal)}},
                    {op_or, [Fields1, Fields2]}
            end
        end,
        Args
    ).

make_field(Path, length) ->
    [path_str(Path), <<":length">>];
make_field(Path, Arg) ->
    [path_str(Path), <<":">>, type_str(Arg)].

range(lt, Arg) ->
    Min = get_range(min, Arg),
    [<<"[", Min/binary, " TO ">>, value_str(Arg), <<"}">>];
range(lte, Arg) ->
    Min = get_range(min, Arg),
    [<<"[", Min/binary, " TO ">>, value_str(Arg), <<"]">>];
range(gte, Arg) ->
    Max = get_range(max, Arg),
    [<<"[">>, value_str(Arg), <<" TO ", Max/binary, "]">>];
range(gt, Arg) ->
    Max = get_range(max, Arg),
    [<<"{">>, value_str(Arg), <<" TO ", Max/binary, "]">>].

get_range(min, Arg) when is_number(Arg) ->
    <<"-Infinity">>;
get_range(min, _Arg) ->
    <<"\"\"">>;
get_range(max, Arg) when is_number(Arg) ->
    <<"Infinity">>;
get_range(max, _Arg) ->
    <<"\u0x10FFFF">>.

field_exists_query(Path) ->
    % We specify two here for :* and .* so that we don't incorrectly
    % match a path foo.name against foo.name_first (if were to just
    % append * instead).
    Parts = [
        % We need to remove the period from the path list to indicate that it is
        % a path separator. We escape the colon because it is not used as a
        % separator and we escape colons in field names.
        {op_fieldname, {[path_str(Path), ":"], "*"}},
        {op_fieldname, {[path_str(Path)], ".*"}}
    ],
    {op_or, Parts}.

field_exists_query(Path, Type) ->
    {op_fieldname, {[path_str(Path), ":"], Type}}.

path_str(Path) ->
    path_str(Path, []).

path_str([], Acc) ->
    Acc;
path_str([Part], Acc) ->
    % No reverse because Path is backwards
    % during recursion of convert.
    [Part | Acc];
path_str([Part | Rest], Acc) ->
    case Part of
        % do not append a period if Part is blank
        <<>> ->
            path_str(Rest, [Acc]);
        _ ->
            path_str(Rest, [<<".">>, Part | Acc])
    end.

type_str(Value) when is_number(Value) ->
    <<"number">>;
type_str(Value) when is_boolean(Value) ->
    <<"boolean">>;
type_str(Value) when is_binary(Value) ->
    <<"string">>;
type_str(null) ->
    <<"null">>.

value_str(Value) when is_binary(Value) ->
    case mango_util:is_number_string(Value) of
        true ->
            <<"\"", Value/binary, "\"">>;
        false ->
            Escaped = mango_util:lucene_escape_query_value(Value),
            <<"\"", Escaped/binary, "\"">>
    end;
value_str(Value) when is_integer(Value) ->
    list_to_binary(integer_to_list(Value));
value_str(Value) when is_float(Value) ->
    list_to_binary(float_to_list(Value));
value_str(true) ->
    <<"true">>;
value_str(false) ->
    <<"false">>;
value_str(null) ->
    <<"true">>.

append_sort_type(RawSortField, Selector) ->
    EncodeField = mango_util:lucene_escape_user(RawSortField),
    String = mango_util:has_suffix(EncodeField, <<"_3astring">>),
    Number = mango_util:has_suffix(EncodeField, <<"_3anumber">>),
    case {String, Number} of
        {true, _} ->
            <<EncodeField/binary, "<string>">>;
        {_, true} ->
            <<EncodeField/binary, "<number>">>;
        _ ->
            Type = get_sort_type(RawSortField, Selector),
            <<EncodeField/binary, Type/binary>>
    end.

get_sort_type(Field, Selector) ->
    Types = get_sort_types(Field, Selector, []),
    case lists:usort(Types) of
        [str] -> <<"_3astring<string>">>;
        [num] -> <<"_3anumber<number>">>;
        _ -> ?MANGO_ERROR({text_sort_error, Field})
    end.

get_sort_types(Field, {[{Field, {[{<<"$", _/binary>>, Cond}]}}]}, Acc) when
    is_binary(Cond)
->
    [str | Acc];
get_sort_types(Field, {[{Field, {[{<<"$", _/binary>>, Cond}]}}]}, Acc) when
    is_number(Cond)
->
    [num | Acc];
get_sort_types(Field, {[{_, Cond}]}, Acc) when is_list(Cond) ->
    lists:foldl(
        fun(Arg, InnerAcc) ->
            get_sort_types(Field, Arg, InnerAcc)
        end,
        Acc,
        Cond
    );
get_sort_types(Field, {[{_, Cond}]}, Acc) when is_tuple(Cond) ->
    get_sort_types(Field, Cond, Acc);
get_sort_types(_Field, _, Acc) ->
    Acc.

replace_array_indexes([], NewPartsAcc, HasIntAcc) ->
    {NewPartsAcc, HasIntAcc};
replace_array_indexes([Part | Rest], NewPartsAcc, HasIntAcc) ->
    {NewPart, HasInt} =
        try
            _ = list_to_integer(binary_to_list(Part)),
            {<<"[]">>, true}
        catch
            _:_ ->
                {Part, false}
        end,
    replace_array_indexes(
        Rest,
        [NewPart | NewPartsAcc],
        HasInt or HasIntAcc
    ).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

convert_selector(Selector) ->
    convert([], test_util:as_selector(Selector)).

convert_fields_test() ->
    ?assertEqual(
        {op_null, {[[<<"field">>], <<":">>, <<"null">>], <<"true">>}},
        convert_selector(#{<<"field">> => null})
    ),
    ?assertEqual(
        {op_field, {[[<<"field">>], <<":">>, <<"boolean">>], <<"true">>}},
        convert_selector(#{<<"field">> => true})
    ),
    ?assertEqual(
        {op_field, {[[<<"field">>], <<":">>, <<"number">>], <<"42">>}},
        convert_selector(#{<<"field">> => 42})
    ),
    ?assertEqual(
        {op_field, {[[<<"field">>], <<":">>, <<"string">>], <<"\"value\"">>}},
        convert_selector(#{<<"field">> => <<"value">>})
    ),
    ?assertEqual(
        {op_and, [
            {op_field, {[[<<"field">>, <<".">>, <<"[]">>], <<":length">>], <<"3">>}},
            {op_field, {[[<<"field">>, <<".">>, <<"[]">>], <<":">>, <<"number">>], <<"1">>}},
            {op_field, {[[<<"field">>, <<".">>, <<"[]">>], <<":">>, <<"number">>], <<"2">>}},
            {op_field, {[[<<"field">>, <<".">>, <<"[]">>], <<":">>, <<"number">>], <<"3">>}}
        ]},
        convert_selector(#{<<"field">> => [1, 2, 3]})
    ),
    ?assertEqual(
        {op_field, {
            [[<<"field1">>, <<".">>, <<"field2">>], <<":">>, <<"string">>], <<"\"value\"">>
        }},
        convert_selector(#{<<"field1">> => #{<<"field2">> => <<"value">>}})
    ),
    ?assertEqual(
        {op_and, [
            {op_field, {[[<<"field2">>], <<":">>, <<"string">>], <<"\"value2\"">>}},
            {op_field, {[[<<"field1">>], <<":">>, <<"string">>], <<"\"value1\"">>}}
        ]},
        convert_selector(#{<<"field1">> => <<"value1">>, <<"field2">> => <<"value2">>})
    ).

convert_default_test() ->
    ?assertEqual(
        {op_default, <<"\"text\"">>},
        convert_selector(#{<<"$default">> => #{<<"$text">> => <<"text">>}})
    ).

convert_lt_test() ->
    ?assertEqual(
        {op_field,
            {[[<<"field">>], <<":">>, <<"number">>], [<<"[-Infinity TO ">>, <<"42">>, <<"}">>]}},
        convert_selector(#{<<"field">> => #{<<"$lt">> => 42}})
    ),
    ?assertEqual(
        {op_or, [
            {op_fieldname, {[[<<"field">>], ":"], "*"}},
            {op_fieldname, {[[<<"field">>]], ".*"}}
        ]},
        convert_selector(#{<<"field">> => #{<<"$lt">> => [1, 2, 3]}})
    ),
    ?assertEqual(
        {op_or, [
            {op_fieldname, {[[<<"field">>], ":"], "*"}},
            {op_fieldname, {[[<<"field">>]], ".*"}}
        ]},
        convert_selector(#{<<"field">> => #{<<"$lt">> => null}})
    ).

convert_lte_test() ->
    ?assertEqual(
        {op_field,
            {[[<<"field">>], <<":">>, <<"number">>], [<<"[-Infinity TO ">>, <<"42">>, <<"]">>]}},
        convert_selector(#{<<"field">> => #{<<"$lte">> => 42}})
    ),
    ?assertEqual(
        {op_or, [
            {op_fieldname, {[[<<"field">>], ":"], "*"}},
            {op_fieldname, {[[<<"field">>]], ".*"}}
        ]},
        convert_selector(#{<<"field">> => #{<<"$lte">> => [1, 2, 3]}})
    ),
    ?assertEqual(
        {op_or, [
            {op_fieldname, {[[<<"field">>], ":"], "*"}},
            {op_fieldname, {[[<<"field">>]], ".*"}}
        ]},
        convert_selector(#{<<"field">> => #{<<"$lte">> => null}})
    ).

convert_eq_test() ->
    ?assertEqual(
        {op_field, {[[<<"field">>], <<":">>, <<"number">>], <<"42">>}},
        convert_selector(#{<<"field">> => #{<<"$eq">> => 42}})
    ),
    ?assertEqual(
        {op_and, [
            {op_field, {[[<<"field">>, <<".">>, <<"[]">>], <<":length">>], <<"3">>}},
            {op_field, {[[<<"field">>, <<".">>, <<"[]">>], <<":">>, <<"number">>], <<"1">>}},
            {op_field, {[[<<"field">>, <<".">>, <<"[]">>], <<":">>, <<"number">>], <<"2">>}},
            {op_field, {[[<<"field">>, <<".">>, <<"[]">>], <<":">>, <<"number">>], <<"3">>}}
        ]},
        convert_selector(#{<<"field">> => #{<<"$eq">> => [1, 2, 3]}})
    ),
    ?assertEqual(
        {op_null, {[[<<"field">>], <<":">>, <<"null">>], <<"true">>}},
        convert_selector(#{<<"field">> => #{<<"$eq">> => null}})
    ).

convert_ne_test() ->
    ?assertEqual(
        {op_not, {
            {op_or, [
                {op_fieldname, {[[<<"field">>], ":"], "*"}},
                {op_fieldname, {[[<<"field">>]], ".*"}}
            ]},
            {op_field, {[[<<"field">>], <<":">>, <<"number">>], <<"42">>}}
        }},
        convert_selector(#{<<"field">> => #{<<"$ne">> => 42}})
    ).

convert_gte_test() ->
    ?assertEqual(
        {op_field,
            {[[<<"field">>], <<":">>, <<"number">>], [<<"[">>, <<"42">>, <<" TO Infinity]">>]}},
        convert_selector(#{<<"field">> => #{<<"$gte">> => 42}})
    ),
    ?assertEqual(
        {op_or, [
            {op_fieldname, {[[<<"field">>], ":"], "*"}},
            {op_fieldname, {[[<<"field">>]], ".*"}}
        ]},
        convert_selector(#{<<"field">> => #{<<"$gte">> => [1, 2, 3]}})
    ),
    ?assertEqual(
        {op_or, [
            {op_fieldname, {[[<<"field">>], ":"], "*"}},
            {op_fieldname, {[[<<"field">>]], ".*"}}
        ]},
        convert_selector(#{<<"field">> => #{<<"$gte">> => null}})
    ).

convert_gt_test() ->
    ?assertEqual(
        {op_field,
            {[[<<"field">>], <<":">>, <<"number">>], [<<"{">>, <<"42">>, <<" TO Infinity]">>]}},
        convert_selector(#{<<"field">> => #{<<"$gt">> => 42}})
    ),
    ?assertEqual(
        {op_or, [
            {op_fieldname, {[[<<"field">>], ":"], "*"}},
            {op_fieldname, {[[<<"field">>]], ".*"}}
        ]},
        convert_selector(#{<<"field">> => #{<<"$gt">> => [1, 2, 3]}})
    ),
    ?assertEqual(
        {op_or, [
            {op_fieldname, {[[<<"field">>], ":"], "*"}},
            {op_fieldname, {[[<<"field">>]], ".*"}}
        ]},
        convert_selector(#{<<"field">> => #{<<"$gt">> => null}})
    ).

convert_all_test() ->
    ?assertEqual(
        {op_and, [
            {op_field, {
                [[<<"field">>, <<".">>, <<"[]">>], <<":">>, <<"string">>], <<"\"value1\"">>
            }},
            {op_field, {
                [[<<"field">>, <<".">>, <<"[]">>], <<":">>, <<"string">>], <<"\"value2\"">>
            }}
        ]},
        convert_selector(#{<<"field">> => #{<<"$all">> => [<<"value1">>, <<"value2">>]}})
    ).

convert_elemMatch_test() ->
    ?assertEqual(
        {op_field, {[[<<"field">>, <<".">>, <<"[]">>], <<":">>, <<"string">>], <<"\"value\"">>}},
        convert_selector(#{<<"field">> => #{<<"$elemMatch">> => #{<<"$eq">> => <<"value">>}}})
    ).

convert_allMatch_test() ->
    ?assertEqual(
        {op_field, {[[<<"field">>, <<".">>, <<"[]">>], <<":">>, <<"string">>], <<"\"value\"">>}},
        convert_selector(#{<<"field">> => #{<<"$allMatch">> => #{<<"$eq">> => <<"value">>}}})
    ).

convert_keyMapMatch_test() ->
    ?assertThrow(
        {mango_error, mango_selector_text, {invalid_operator, <<"$keyMapMatch">>}},
        convert_selector(#{<<"field">> => #{<<"$keyMapMatch">> => #{<<"key">> => <<"value">>}}})
    ).

convert_in_test() ->
    ?assertEqual(
        {op_or, []},
        convert_selector(#{<<"field">> => #{<<"$in">> => []}})
    ),
    ?assertEqual(
        {op_or, [
            {op_or, [
                {op_field, {[[<<"field">>], <<":">>, <<"string">>], <<"\"value1\"">>}},
                {op_field, {
                    [[<<"field">>, <<".">>, <<"[]">>], <<":">>, <<"string">>], <<"\"value1\"">>
                }}
            ]},
            {op_or, [
                {op_field, {[[<<"field">>], <<":">>, <<"string">>], <<"\"value2\"">>}},
                {op_field, {
                    [[<<"field">>, <<".">>, <<"[]">>], <<":">>, <<"string">>], <<"\"value2\"">>
                }}
            ]}
        ]},
        convert_selector(#{<<"field">> => #{<<"$in">> => [<<"value1">>, <<"value2">>]}})
    ).

convert_nin_test() ->
    ?assertEqual(
        {op_not, {
            {op_or, [
                {op_fieldname, {[[<<"field">>], ":"], "*"}},
                {op_fieldname, {[[<<"field">>]], ".*"}}
            ]},
            {op_or, []}
        }},
        convert_selector(#{<<"field">> => #{<<"$nin">> => []}})
    ),
    ?assertEqual(
        {op_not, {
            {op_or, [
                {op_fieldname, {[[<<"field">>], ":"], "*"}},
                {op_fieldname, {[[<<"field">>]], ".*"}}
            ]},
            {op_or, [
                {op_or, [
                    {op_field, {[[<<"field">>], <<":">>, <<"number">>], <<"1">>}},
                    {op_field, {[[<<"field">>, <<".">>, <<"[]">>], <<":">>, <<"number">>], <<"1">>}}
                ]},
                {op_or, [
                    {op_field, {[[<<"field">>], <<":">>, <<"number">>], <<"2">>}},
                    {op_field, {[[<<"field">>, <<".">>, <<"[]">>], <<":">>, <<"number">>], <<"2">>}}
                ]}
            ]}
        }},
        convert_selector(#{<<"field">> => #{<<"$nin">> => [1, 2]}})
    ).

convert_exists_test() ->
    ?assertEqual(
        {op_or, [
            {op_fieldname, {[[<<"field">>], ":"], "*"}},
            {op_fieldname, {[[<<"field">>]], ".*"}}
        ]},
        convert_selector(#{<<"field">> => #{<<"$exists">> => true}})
    ),
    ?assertEqual(
        {op_not, {
            {op_or, [
                {op_fieldname, {[[<<"field">>], ":"], "*"}},
                {op_fieldname, {[[<<"field">>]], ".*"}}
            ]},
            false
        }},
        convert_selector(#{<<"field">> => #{<<"$exists">> => false}})
    ).

convert_type_test() ->
    ?assertEqual(
        {op_or, [
            {op_fieldname, {[[<<"field">>], ":"], "*"}},
            {op_fieldname, {[[<<"field">>]], ".*"}}
        ]},
        convert_selector(#{<<"field">> => #{<<"$type">> => <<"string">>}})
    ).

convert_mod_test() ->
    ?assertEqual(
        {op_fieldname, {[[<<"field">>], ":"], "number"}},
        convert_selector(#{<<"field">> => #{<<"$mod">> => [2, 0]}})
    ).

convert_regex_test() ->
    ?assertEqual(
        {op_regex, [<<"field">>]},
        convert_selector(#{<<"field">> => #{<<"$regex">> => <<".*">>}})
    ).

convert_size_test() ->
    ?assertEqual(
        {op_field, {[[<<"field">>, <<".">>, <<"[]">>], <<":length">>], <<"6">>}},
        convert_selector(#{<<"field">> => #{<<"$size">> => 6}})
    ).

convert_not_test() ->
    ?assertEqual(
        {op_not, {
            {op_or, [
                {op_fieldname, {[[<<"field">>], ":"], "*"}},
                {op_fieldname, {[[<<"field">>]], ".*"}}
            ]},
            {op_fieldname, {[[<<"field">>], ":"], "number"}}
        }},
        convert_selector(#{<<"field">> => #{<<"$not">> => #{<<"$mod">> => [2, 0]}}})
    ).

convert_and_test() ->
    ?assertEqual(
        {op_and, []},
        convert_selector(#{<<"$and">> => []})
    ),
    ?assertEqual(
        {op_and, [{op_field, {[[<<"field">>], <<":">>, <<"string">>], <<"\"value\"">>}}]},
        convert_selector(#{<<"$and">> => [#{<<"field">> => <<"value">>}]})
    ),
    ?assertEqual(
        {op_and, [
            {op_field, {[[<<"field1">>], <<":">>, <<"string">>], <<"\"value1\"">>}},
            {op_field, {[[<<"field2">>], <<":">>, <<"string">>], <<"\"value2\"">>}}
        ]},
        convert_selector(#{
            <<"$and">> => [#{<<"field1">> => <<"value1">>}, #{<<"field2">> => <<"value2">>}]
        })
    ).

convert_or_test() ->
    ?assertEqual(
        {op_or, []},
        convert_selector(#{<<"$or">> => []})
    ),
    ?assertEqual(
        {op_or, [{op_field, {[[<<"field">>], <<":">>, <<"string">>], <<"\"value\"">>}}]},
        convert_selector(#{<<"$or">> => [#{<<"field">> => <<"value">>}]})
    ),
    ?assertEqual(
        {op_or, [
            {op_field, {[[<<"field1">>], <<":">>, <<"string">>], <<"\"value1\"">>}},
            {op_field, {[[<<"field2">>], <<":">>, <<"string">>], <<"\"value2\"">>}}
        ]},
        convert_selector(#{
            <<"$or">> => [#{<<"field1">> => <<"value1">>}, #{<<"field2">> => <<"value2">>}]
        })
    ).

convert_nor_test() ->
    ?assertEqual(
        {op_and, []},
        convert_selector(#{<<"$nor">> => []})
    ),
    ?assertEqual(
        {op_and, [
            {op_not, {
                {op_or, [
                    {op_fieldname, {[[<<"field">>], ":"], "*"}},
                    {op_fieldname, {[[<<"field">>]], ".*"}}
                ]},
                {op_field, {[[<<"field">>], <<":">>, <<"string">>], <<"\"value\"">>}}
            }}
        ]},
        convert_selector(#{<<"$nor">> => [#{<<"field">> => <<"value">>}]})
    ),
    ?assertEqual(
        {op_and, [
            {op_not, {
                {op_or, [
                    {op_fieldname, {[[<<"field1">>], ":"], "*"}},
                    {op_fieldname, {[[<<"field1">>]], ".*"}}
                ]},
                {op_field, {[[<<"field1">>], <<":">>, <<"string">>], <<"\"value1\"">>}}
            }},
            {op_not, {
                {op_or, [
                    {op_fieldname, {[[<<"field2">>], ":"], "*"}},
                    {op_fieldname, {[[<<"field2">>]], ".*"}}
                ]},
                {op_field, {[[<<"field2">>], <<":">>, <<"string">>], <<"\"value2\"">>}}
            }}
        ]},
        convert_selector(#{
            <<"$nor">> => [#{<<"field1">> => <<"value1">>}, #{<<"field2">> => <<"value2">>}]
        })
    ).

convert_beginswith_test() ->
    ?assertEqual(
        {op_field, {[[<<"field">>], <<":">>, <<"string">>], <<"foo*">>}},
        convert_selector(#{<<"field">> => #{<<"$beginsWith">> => <<"foo">>}})
    ).

to_query_test() ->
    F = fun(S) -> iolist_to_binary(to_query(S)) end,
    Input = {<<"name">>, <<"value">>},
    ?assertEqual(<<"(name:value)">>, F({op_field, Input})),
    ?assertEqual(
        <<"(name1.name2_3atype:value)">>,
        F({op_field, {[[<<"name1">>, <<".">>, <<"name2">>], <<":">>, <<"type">>], <<"value">>}})
    ),
    ?assertEqual(<<"(name:value)">>, F({op_null, Input})),
    ?assertEqual(<<"($fieldnames:name_3astring)">>, F({op_regex, <<"name">>})),
    ?assertEqual(<<"($fieldnames:name_3a.*)">>, F({op_fieldname, {<<"name">>, <<"_3a.*">>}})),
    Arg1 = {op_default, <<"value">>},
    ?assertEqual(<<"($default:value)">>, F(Arg1)),
    Arg2 = {op_field, Input},
    ?assertEqual(<<"($fieldnames:/.*/  AND NOT ((name:value)))">>, F({op_not, {Arg2, false}})),
    ?assertEqual(<<"((name:value))">>, F({op_not, {Arg2, {op_and, []}}})),
    ?assertEqual(<<"">>, F({op_and, []})),
    ?assertEqual(<<"(($default:value))">>, F({op_and, [Arg1]})),
    ?assertEqual(<<"(($default:value) AND (name:value))">>, F({op_and, [Arg1, Arg2]})),
    ?assertEqual(
        <<"(($default:value) AND (name:value))">>, F({op_and, [Arg1, {op_and, []}, Arg2]})
    ),
    ?assertEqual(<<"">>, F({op_or, []})),
    ?assertEqual(<<"(($default:value))">>, F({op_or, [Arg1]})),
    ?assertEqual(<<"(($default:value) OR (name:value))">>, F({op_or, [Arg1, Arg2]})),
    ?assertEqual(<<"(($default:value) OR (name:value))">>, F({op_or, [Arg1, Arg2, {op_or, []}]})).
-endif.
