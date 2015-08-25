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


-include_lib("couch/include/couch_db.hrl").
-include("mango.hrl").


convert(Object) ->
    TupleTree = convert([], Object),
    iolist_to_binary(to_query(TupleTree)).


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
    Value = maybe_append_quotes(value_str(Query)),
    {op_field, {make_field(Path, Query), Value}};

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
            Fields1 = convert(Path, {[{<<"$eq">> , Values}]}),
            Fields2 = convert([<<"[]">>| Path], {[{<<"$eq">> , Values}]}),
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

% Our comparison operators are fairly straight forward
convert(Path, {[{<<"$lt">>, Arg}]}) when is_list(Arg); is_tuple(Arg);
        Arg =:= null ->
    field_exists_query(Path);
convert(Path, {[{<<"$lt">>, Arg}]}) ->
    {op_field, {make_field(Path, Arg), range(lt, Arg)}};
convert(Path, {[{<<"$lte">>, Arg}]}) when is_list(Arg); is_tuple(Arg);
        Arg =:= null->
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
convert(Path, {[{<<"$gte">>, Arg}]}) when is_list(Arg); is_tuple(Arg);
        Arg =:= null ->
    field_exists_query(Path);
convert(Path, {[{<<"$gte">>, Arg}]}) ->
    {op_field, {make_field(Path, Arg), range(gte, Arg)}};
convert(Path, {[{<<"$gt">>, Arg}]}) when is_list(Arg); is_tuple(Arg);
        Arg =:= null->
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
convert(Path, {[{<<"$regex">>, _}]}) ->
    field_exists_query(Path, "string");

convert(Path, {[{<<"$size">>, Arg}]}) ->
    {op_field, {make_field([<<"[]">> | Path], length), value_str(Arg)}};

% All other operators are internal assertion errors for
% matching because we either should've removed them during
% normalization or something else broke.
convert(_Path, {[{<<"$", _/binary>>=Op, _}]}) ->
    ?MANGO_ERROR({invalid_operator, Op});

% We've hit a field name specifier. We need to break the name
% into path parts and continue our conversion.
convert(Path, {[{Field, Cond}]}) ->
    NewPathParts = re:split(Field, <<"\\.">>),
    NewPath = lists:reverse(NewPathParts) ++ Path,
    convert(NewPath, Cond);

%% For $in
convert(Path, Val) when is_binary(Val); is_number(Val); is_boolean(Val) ->
    {op_field, {make_field(Path, Val), value_str(Val)}};

% Anything else is a bad selector.
convert(_Path, {Props} = Sel) when length(Props) > 1 ->
    erlang:error({unnormalized_selector, Sel}).


to_query({op_and, Args}) when is_list(Args) ->
    QueryArgs = lists:map(fun to_query/1, Args),
    ["(", mango_util:join(<<" AND ">>, QueryArgs), ")"];

to_query({op_or, Args}) when is_list(Args) ->
    ["(", mango_util:join(" OR ", lists:map(fun to_query/1, Args)), ")"];

to_query({op_not, {ExistsQuery, Arg}}) when is_tuple(Arg) ->
    ["(", to_query(ExistsQuery), " AND NOT (", to_query(Arg), "))"];

%% For $exists:false
to_query({op_not, {ExistsQuery, false}}) ->
    ["($fieldnames:/.*/ ", " AND NOT (", to_query(ExistsQuery), "))"];

to_query({op_insert, Arg}) when is_binary(Arg) ->
    ["(", Arg, ")"];

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

to_query({op_default, Value}) ->
    ["($default:", Value, ")"].


%% We match on fieldname and fieldname.[]
convert_in(Path, Args) ->
    Path0 = [<<"[]">> | Path],
    lists:map(fun(Arg) ->
        case Arg of
            {Object} ->
                Parts = lists:map(fun (SubObject) ->
                    Fields1 = convert(Path, {[SubObject]}),
                    Fields2 = convert(Path0, {[SubObject]}),
                    {op_or, [Fields1, Fields2]}
                end, Object),
                {op_or, Parts};
            SingleVal ->
                Fields1 = {op_field, {make_field(Path, SingleVal),
                value_str(SingleVal)}},
                Fields2 = {op_field, {make_field(Path0, SingleVal),
                value_str(SingleVal)}},
                {op_or, [Fields1, Fields2]}
        end
    end, Args).


make_field(Path, length) ->
    [path_str(Path), <<":length">>];
make_field(Path, Arg) ->
    [path_str(Path), <<":">>, type_str(Arg)].


range(lt, Arg) ->
    [<<"[-Infinity TO ">>, value_str(Arg), <<"}">>];
range(lte, Arg) ->
    [<<"[-Infinity TO ">>, value_str(Arg), <<"]">>];
range(gte, Arg) ->
    [<<"[">>, value_str(Arg), <<" TO Infinity]">>];
range(gt, Arg) ->
    [<<"{">>, value_str(Arg), <<" TO Infinity]">>].


field_exists_query(Path) ->
    % We specify two here for :* and .* so that we don't incorrectly
    % match a path foo.name against foo.name_first (if were to just
    % appened * isntead).
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
    mango_util:lucene_escape_query_value(Value);
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


maybe_append_quotes(TextValue) ->
    case mango_util:is_number_string(TextValue) of
        true ->
            <<"\"", TextValue/binary, "\"">>;
        false ->
            TextValue
    end.


get_sort_type(Field, Selector) ->
    Types = get_sort_types(Field, Selector, []),
    case lists:usort(Types) of
        [str] -> <<"_3astring<string>">>;
        [num] -> <<"_3anumber<number>">>;
        _ -> ?MANGO_ERROR({text_sort_error, Field})
    end.


get_sort_types(Field, {[{Field, {[{<<"$", _/binary>>, Cond}]}}]}, Acc)
        when is_binary(Cond) ->
    [str | Acc];

get_sort_types(Field, {[{Field, {[{<<"$", _/binary>>, Cond}]}}]}, Acc)
        when is_number(Cond) ->
    [num | Acc];

get_sort_types(Field, {[{_, Cond}]}, Acc) when is_list(Cond) ->
    lists:foldl(fun(Arg, InnerAcc) ->
        get_sort_types(Field, Arg, InnerAcc)
    end, Acc, Cond);

get_sort_types(Field, {[{_, Cond}]}, Acc)  when is_tuple(Cond)->
    get_sort_types(Field, Cond, Acc);

get_sort_types(_Field, _, Acc)  ->
    Acc.
