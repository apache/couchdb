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

-module(mango_selector).


-export([
    normalize/1,
    match/2,
    has_required_fields/2,
    is_constant_field/2
]).


-include_lib("couch/include/couch_db.hrl").
-include("mango.hrl").


% Validate and normalize each operator. This translates
% every selector operator into a consistent version that
% we can then rely on for all other selector functions.
% See the definition of each step below for more information
% on what each one does.
normalize({[]}) ->
    {[]};
normalize(Selector) ->
    Steps = [
        fun norm_ops/1,
        fun norm_fields/1,
        fun norm_negations/1
    ],
    {NProps} = lists:foldl(fun(Step, Sel) -> Step(Sel) end, Selector, Steps),
    FieldNames = [Name || {Name, _} <- NProps],
    case lists:member(<<>>, FieldNames) of
        true ->
            ?MANGO_ERROR({invalid_selector, missing_field_name});
        false ->
            ok
    end,
    {NProps}.


% Match a selector against a #doc{} or EJSON value.
% This assumes that the Selector has been normalized.
% Returns true or false.
match(Selector, D) ->
    couch_stats:increment_counter([mango, evaluate_selector]),
    match_int(Selector, D).


% An empty selector matches any value.
match_int({[]}, _) ->
    true;

match_int(Selector, #doc{body=Body}) ->
    match(Selector, Body, fun mango_json:cmp/2);

match_int(Selector, {Props}) ->
    match(Selector, {Props}, fun mango_json:cmp/2).

% Convert each operator into a normalized version as well
% as convert an implict operators into their explicit
% versions.
norm_ops({[{<<"$and">>, Args}]}) when is_list(Args) ->
    {[{<<"$and">>, [norm_ops(A) || A <- Args]}]};
norm_ops({[{<<"$and">>, Arg}]}) ->
    ?MANGO_ERROR({bad_arg, '$and', Arg});

norm_ops({[{<<"$or">>, Args}]}) when is_list(Args) ->
    {[{<<"$or">>, [norm_ops(A) || A <- Args]}]};
norm_ops({[{<<"$or">>, Arg}]}) ->
    ?MANGO_ERROR({bad_arg, '$or', Arg});

norm_ops({[{<<"$not">>, {_}=Arg}]}) ->
    {[{<<"$not">>, norm_ops(Arg)}]};
norm_ops({[{<<"$not">>, Arg}]}) ->
    ?MANGO_ERROR({bad_arg, '$not', Arg});

norm_ops({[{<<"$nor">>, Args}]}) when is_list(Args) ->
    {[{<<"$nor">>, [norm_ops(A) || A <- Args]}]};
norm_ops({[{<<"$nor">>, Arg}]}) ->
    ?MANGO_ERROR({bad_arg, '$nor', Arg});

norm_ops({[{<<"$in">>, Args}]} = Cond) when is_list(Args) ->
    Cond;
norm_ops({[{<<"$in">>, Arg}]}) ->
    ?MANGO_ERROR({bad_arg, '$in', Arg});

norm_ops({[{<<"$nin">>, Args}]} = Cond) when is_list(Args) ->
    Cond;
norm_ops({[{<<"$nin">>, Arg}]}) ->
    ?MANGO_ERROR({bad_arg, '$nin', Arg});

norm_ops({[{<<"$exists">>, Arg}]} = Cond) when is_boolean(Arg) ->
    Cond;
norm_ops({[{<<"$exists">>, Arg}]}) ->
    ?MANGO_ERROR({bad_arg, '$exists', Arg});

norm_ops({[{<<"$type">>, Arg}]} = Cond) when is_binary(Arg) ->
    Cond;
norm_ops({[{<<"$type">>, Arg}]}) ->
    ?MANGO_ERROR({bad_arg, '$type', Arg});

norm_ops({[{<<"$mod">>, [D, R]}]} = Cond) when is_integer(D), is_integer(R) ->
    Cond;
norm_ops({[{<<"$mod">>, Arg}]}) ->
    ?MANGO_ERROR({bad_arg, '$mod', Arg});

norm_ops({[{<<"$regex">>, Regex}]} = Cond) when is_binary(Regex) ->
    case re:compile(Regex) of
        {ok, _} ->
            Cond;
        _ ->
            ?MANGO_ERROR({bad_arg, '$regex', Regex})
    end;

norm_ops({[{<<"$all">>, Args}]}) when is_list(Args) ->
    {[{<<"$all">>, Args}]};
norm_ops({[{<<"$all">>, Arg}]}) ->
    ?MANGO_ERROR({bad_arg, '$all', Arg});

norm_ops({[{<<"$elemMatch">>, {_}=Arg}]}) ->
    {[{<<"$elemMatch">>, norm_ops(Arg)}]};
norm_ops({[{<<"$elemMatch">>, Arg}]}) ->
    ?MANGO_ERROR({bad_arg, '$elemMatch', Arg});

norm_ops({[{<<"$allMatch">>, {_}=Arg}]}) ->
    {[{<<"$allMatch">>, norm_ops(Arg)}]};
norm_ops({[{<<"$allMatch">>, Arg}]}) ->
    ?MANGO_ERROR({bad_arg, '$allMatch', Arg});

norm_ops({[{<<"$keyMapMatch">>, {_}=Arg}]}) ->
    {[{<<"$keyMapMatch">>, norm_ops(Arg)}]};
norm_ops({[{<<"$keyMapMatch">>, Arg}]}) ->
    ?MANGO_ERROR({bad_arg, '$keyMapMatch', Arg});

norm_ops({[{<<"$size">>, Arg}]}) when is_integer(Arg), Arg >= 0 ->
    {[{<<"$size">>, Arg}]};
norm_ops({[{<<"$size">>, Arg}]}) ->
    ?MANGO_ERROR({bad_arg, '$size', Arg});

norm_ops({[{<<"$text">>, Arg}]}) when is_binary(Arg); is_number(Arg);
        is_boolean(Arg) ->
    {[{<<"$default">>, {[{<<"$text">>, Arg}]}}]};
norm_ops({[{<<"$text">>, Arg}]}) ->
    ?MANGO_ERROR({bad_arg, '$text', Arg});

% Not technically an operator but we pass it through here
% so that this function accepts its own output. This exists
% so that $text can have a field name value which simplifies
% logic elsewhere.
norm_ops({[{<<"$default">>, _}]} = Selector) ->
    Selector;

% Terminals where we can't perform any validation
% on the value because any value is acceptable.
norm_ops({[{<<"$lt">>, _}]} = Cond) ->
    Cond;
norm_ops({[{<<"$lte">>, _}]} = Cond) ->
    Cond;
norm_ops({[{<<"$eq">>, _}]} = Cond) ->
    Cond;
norm_ops({[{<<"$ne">>, _}]} = Cond) ->
    Cond;
norm_ops({[{<<"$gte">>, _}]} = Cond) ->
    Cond;
norm_ops({[{<<"$gt">>, _}]} = Cond) ->
    Cond;

% Known but unsupported operators
norm_ops({[{<<"$where">>, _}]}) ->
    ?MANGO_ERROR({not_supported, '$where'});
norm_ops({[{<<"$geoWithin">>, _}]}) ->
    ?MANGO_ERROR({not_supported, '$geoWithin'});
norm_ops({[{<<"$geoIntersects">>, _}]}) ->
    ?MANGO_ERROR({not_supported, '$geoIntersects'});
norm_ops({[{<<"$near">>, _}]}) ->
    ?MANGO_ERROR({not_supported, '$near'});
norm_ops({[{<<"$nearSphere">>, _}]}) ->
    ?MANGO_ERROR({not_supported, '$nearSphere'});

% Unknown operator
norm_ops({[{<<"$", _/binary>>=Op, _}]}) ->
    ?MANGO_ERROR({invalid_operator, Op});

% A {Field: Cond} pair
norm_ops({[{Field, Cond}]}) ->
    {[{Field, norm_ops(Cond)}]};

% An implicit $and
norm_ops({[_, _ | _] = Props}) ->
    {[{<<"$and">>, [norm_ops({[P]}) || P <- Props]}]};

% A bare value condition means equality
norm_ops(Value) ->
    {[{<<"$eq">>, Value}]}.


% This takes a selector and normalizes all of the
% field names as far as possible. For instance:
%
%   Unnormalized:
%     {foo: {$and: [{$gt: 5}, {$lt: 10}]}}
%
%   Normalized:
%     {$and: [{foo: {$gt: 5}}, {foo: {$lt: 10}}]}
%
% And another example:
%
%   Unnormalized:
%     {foo: {bar: {$gt: 10}}}
%
%   Normalized:
%     {"foo.bar": {$gt: 10}}
%
% Its important to note that we can only normalize
% field names like this through boolean operators where
% we can gaurantee commutativity. We can't necessarily
% do the same through the '$elemMatch' or '$allMatch'
% operators but we can apply the same algorithm to its
% arguments.
norm_fields({[]}) ->
    {[]};
norm_fields(Selector) ->
    norm_fields(Selector, <<>>).


% Operators where we can push the field names further
% down the operator tree
norm_fields({[{<<"$and">>, Args}]}, Path) ->
    {[{<<"$and">>, [norm_fields(A, Path) || A <- Args]}]};

norm_fields({[{<<"$or">>, Args}]}, Path) ->
    {[{<<"$or">>, [norm_fields(A, Path) || A <- Args]}]};

norm_fields({[{<<"$not">>, Arg}]}, Path) ->
    {[{<<"$not">>, norm_fields(Arg, Path)}]};

norm_fields({[{<<"$nor">>, Args}]}, Path) ->
    {[{<<"$nor">>, [norm_fields(A, Path) || A <- Args]}]};

% Fields where we can normalize fields in the
% operator arguments independently.
norm_fields({[{<<"$elemMatch">>, Arg}]}, Path) ->
    Cond = {[{<<"$elemMatch">>, norm_fields(Arg)}]},
    {[{Path, Cond}]};

norm_fields({[{<<"$allMatch">>, Arg}]}, Path) ->
    Cond = {[{<<"$allMatch">>, norm_fields(Arg)}]},
    {[{Path, Cond}]};

norm_fields({[{<<"$keyMapMatch">>, Arg}]}, Path) ->
    Cond = {[{<<"$keyMapMatch">>, norm_fields(Arg)}]},
    {[{Path, Cond}]};


% The text operator operates against the internal
% $default field. This also asserts that the $default
% field is at the root as well as that it only has
% a $text operator applied.
norm_fields({[{<<"$default">>, {[{<<"$text">>, _Arg}]}}]}=Sel, <<>>) ->
    Sel;
norm_fields({[{<<"$default">>, _}]} = Selector, _) ->
    ?MANGO_ERROR({bad_field, Selector});


% Any other operator is a terminal below which no
% field names should exist. Set the path to this
% terminal and return it.
norm_fields({[{<<"$", _/binary>>, _}]} = Cond, Path) ->
    {[{Path, Cond}]};

% We've found a field name. Append it to the path
% and skip this node as we unroll the stack as
% the full path will be further down the branch.
norm_fields({[{Field, Cond}]}, <<>>) ->
    % Don't include the '.' for the first element of
    % the path.
    norm_fields(Cond, Field);
norm_fields({[{Field, Cond}]}, Path) ->
    norm_fields(Cond, <<Path/binary, ".", Field/binary>>);

% An empty selector
norm_fields({[]}, Path) ->
    {Path, {[]}};

% Else we have an invalid selector
norm_fields(BadSelector, _) ->
    ?MANGO_ERROR({bad_field, BadSelector}).


% Take all the negation operators and move the logic
% as far down the branch as possible. This does things
% like:
%
%   Unnormalized:
%     {$not: {foo: {$gt: 10}}}
%
%   Normalized:
%     {foo: {$lte: 10}}
%
% And we also apply DeMorgan's laws
%
%   Unnormalized:
%     {$not: {$and: [{foo: {$gt: 10}}, {foo: {$lt: 5}}]}}
%
%   Normalized:
%     {$or: [{foo: {$lte: 10}}, {foo: {$gte: 5}}]}
%
% This logic is important because we can't "see" through
% a '$not' operator to be able to locate indices that may
% service a specific query. Though if we move the negations
% down to the terminals we may be able to negate specific
% operators which allows us to find usable indices.

% Operators that cause a negation
norm_negations({[{<<"$not">>, Arg}]}) ->
    negate(Arg);

norm_negations({[{<<"$nor">>, Args}]}) ->
    {[{<<"$and">>, [negate(A) || A <- Args]}]};

% Operators that we merely seek through as we look for
% negations.
norm_negations({[{<<"$and">>, Args}]}) ->
    {[{<<"$and">>, [norm_negations(A) || A <- Args]}]};

norm_negations({[{<<"$or">>, Args}]}) ->
    {[{<<"$or">>, [norm_negations(A) || A <- Args]}]};

norm_negations({[{<<"$elemMatch">>, Arg}]}) ->
    {[{<<"$elemMatch">>, norm_negations(Arg)}]};

norm_negations({[{<<"$allMatch">>, Arg}]}) ->
    {[{<<"$allMatch">>, norm_negations(Arg)}]};

norm_negations({[{<<"$keyMapMatch">>, Arg}]}) ->
    {[{<<"$keyMapMatch">>, norm_negations(Arg)}]};

% All other conditions can't introduce negations anywhere
% further down the operator tree.
norm_negations(Cond) ->
    Cond.


% Actually negate an expression. Make sure and read up
% on DeMorgan's laws if you're trying to read this, but
% in a nutshell:
%
%     NOT(a AND b) == NOT(a) OR NOT(b)
%     NOT(a OR b) == NOT(a) AND NOT(b)
%
% Also notice that if a negation hits another negation
% operator that we just nullify the combination. Its
% possible that below the nullification we have more
% negations so we have to recurse back to norm_negations/1.

% Negating negation, nullify but recurse to
% norm_negations/1
negate({[{<<"$not">>, Arg}]}) ->
    norm_negations(Arg);

negate({[{<<"$nor">>, Args}]}) ->
    {[{<<"$or">>, [norm_negations(A) || A <- Args]}]};

% DeMorgan Negations
negate({[{<<"$and">>, Args}]}) ->
    {[{<<"$or">>, [negate(A) || A <- Args]}]};

negate({[{<<"$or">>, Args}]}) ->
    {[{<<"$and">>, [negate(A) || A <- Args]}]};

negate({[{<<"$default">>, _}]} = Arg) ->
    ?MANGO_ERROR({bad_arg, '$not', Arg});

% Negating comparison operators is straight forward
negate({[{<<"$lt">>, Arg}]}) ->
    {[{<<"$gte">>, Arg}]};
negate({[{<<"$lte">>, Arg}]}) ->
    {[{<<"$gt">>, Arg}]};
negate({[{<<"$eq">>, Arg}]}) ->
    {[{<<"$ne">>, Arg}]};
negate({[{<<"$ne">>, Arg}]}) ->
    {[{<<"$eq">>, Arg}]};
negate({[{<<"$gte">>, Arg}]}) ->
    {[{<<"$lt">>, Arg}]};
negate({[{<<"$gt">>, Arg}]}) ->
    {[{<<"$lte">>, Arg}]};
negate({[{<<"$in">>, Args}]}) ->
    {[{<<"$nin">>, Args}]};
negate({[{<<"$nin">>, Args}]}) ->
    {[{<<"$in">>, Args}]};

% We can also trivially negate the exists operator
negate({[{<<"$exists">>, Arg}]}) ->
    {[{<<"$exists">>, not Arg}]};

% Anything else we have to just terminate the
% negation by reinserting the negation operator
negate({[{<<"$", _/binary>>, _}]} = Cond) ->
    {[{<<"$not">>, Cond}]};

% Finally, negating a field just means we negate its
% condition.
negate({[{Field, Cond}]}) ->
    {[{Field, negate(Cond)}]}.


% We need to treat an empty array as always true. This will be applied
% for $or, $in, $all, $nin as well.
match({[{<<"$and">>, []}]}, _, _) ->
    true;
match({[{<<"$and">>, Args}]}, Value, Cmp) ->
    Pred = fun(SubSel) -> match(SubSel, Value, Cmp) end,
    lists:all(Pred, Args);

match({[{<<"$or">>, []}]}, _, _) ->
    true;
match({[{<<"$or">>, Args}]}, Value, Cmp) ->
    Pred = fun(SubSel) -> match(SubSel, Value, Cmp) end,
    lists:any(Pred, Args);

match({[{<<"$not">>, Arg}]}, Value, Cmp) ->
    not match(Arg, Value, Cmp);

match({[{<<"$all">>, []}]}, _, _) ->
    false;
% All of the values in Args must exist in Values or
% Values == hd(Args) if Args is a single element list
% that contains a list.
match({[{<<"$all">>, Args}]}, Values, _Cmp) when is_list(Values) ->
    Pred = fun(A) -> lists:member(A, Values) end,
    HasArgs = lists:all(Pred, Args),
    IsArgs = case Args of
        [A] when is_list(A) ->
            A == Values;
        _ ->
            false
    end,
    HasArgs orelse IsArgs;
match({[{<<"$all">>, _Args}]}, _Values, _Cmp) ->
    false;

%% This is for $elemMatch, $allMatch, and possibly $in because of our normalizer.
%% A selector such as {"field_name": {"$elemMatch": {"$gte": 80, "$lt": 85}}}
%% gets normalized to:
%% {[{<<"field_name">>,
%%     {[{<<"$elemMatch">>,
%%         {[{<<"$and">>, [
%%             {[{<<>>,{[{<<"$gte">>,80}]}}]},
%%             {[{<<>>,{[{<<"$lt">>,85}]}}]}
%%         ]}]}
%%     }]}
%% }]}.
%% So we filter out the <<>>.
match({[{<<>>, Arg}]}, Values, Cmp) ->
    match(Arg, Values, Cmp);

% Matches when any element in values matches the
% sub-selector Arg.
match({[{<<"$elemMatch">>, Arg}]}, Values, Cmp) when is_list(Values) ->
    try
        lists:foreach(fun(V) ->
            case match(Arg, V, Cmp) of
                true -> throw(matched);
                _ -> ok
            end
        end, Values),
        false
    catch
        throw:matched ->
            true;
        _:_ ->
            false
    end;
match({[{<<"$elemMatch">>, _Arg}]}, _Value, _Cmp) ->
    false;

% Matches when all elements in values match the
% sub-selector Arg.
match({[{<<"$allMatch">>, Arg}]}, [_ | _] = Values, Cmp) ->
    try
        lists:foreach(fun(V) ->
            case match(Arg, V, Cmp) of
              false -> throw(unmatched);
              _ -> ok
            end
        end, Values),
        true
    catch
        _:_ ->
            false
    end;
match({[{<<"$allMatch">>, _Arg}]}, _Value, _Cmp) ->
    false;

% Matches when any key in the map value matches the
% sub-selector Arg.
match({[{<<"$keyMapMatch">>, Arg}]}, Value, Cmp) when is_tuple(Value) ->
    try
        lists:foreach(fun(V) ->
            case match(Arg, V, Cmp) of
                true -> throw(matched);
                _ -> ok
            end
        end, [Key || {Key, _} <- element(1, Value)]),
        false
    catch
        throw:matched ->
            true;
        _:_ ->
            false
    end;
match({[{<<"$keyMapMatch">>, _Arg}]}, _Value, _Cmp) ->
    false;

% Our comparison operators are fairly straight forward
match({[{<<"$lt">>, Arg}]}, Value, Cmp) ->
    Cmp(Value, Arg) < 0;
match({[{<<"$lte">>, Arg}]}, Value, Cmp) ->
    Cmp(Value, Arg) =< 0;
match({[{<<"$eq">>, Arg}]}, Value, Cmp) ->
    Cmp(Value, Arg) == 0;
match({[{<<"$ne">>, Arg}]}, Value, Cmp) ->
    Cmp(Value, Arg) /= 0;
match({[{<<"$gte">>, Arg}]}, Value, Cmp) ->
    Cmp(Value, Arg) >= 0;
match({[{<<"$gt">>, Arg}]}, Value, Cmp) ->
    Cmp(Value, Arg) > 0;

match({[{<<"$in">>, []}]}, _, _) ->
    false;
match({[{<<"$in">>, Args}]}, Values, Cmp) when is_list(Values)->
    Pred = fun(Arg) ->
        lists:foldl(fun(Value,Match) ->
            (Cmp(Value, Arg) == 0) or Match
        end, false, Values)
    end,
    lists:any(Pred, Args);
match({[{<<"$in">>, Args}]}, Value, Cmp) ->
    Pred = fun(Arg) -> Cmp(Value, Arg) == 0 end,
    lists:any(Pred, Args);

match({[{<<"$nin">>, []}]}, _, _) ->
    true;
match({[{<<"$nin">>, Args}]}, Values, Cmp) when is_list(Values)->
    not match({[{<<"$in">>, Args}]}, Values, Cmp);
match({[{<<"$nin">>, Args}]}, Value, Cmp) ->
    Pred = fun(Arg) -> Cmp(Value, Arg) /= 0 end,
    lists:all(Pred, Args);

% This logic is a bit subtle. Basically, if value is
% not undefined, then it exists.
match({[{<<"$exists">>, ShouldExist}]}, Value, _Cmp) ->
    Exists = Value /= undefined,
    ShouldExist andalso Exists;

match({[{<<"$type">>, Arg}]}, Value, _Cmp) when is_binary(Arg) ->
    Arg == mango_json:type(Value);

match({[{<<"$mod">>, [D, R]}]}, Value, _Cmp) when is_integer(Value) ->
    Value rem D == R;
match({[{<<"$mod">>, _}]}, _Value, _Cmp) ->
    false;

match({[{<<"$regex">>, Regex}]}, Value, _Cmp) when is_binary(Value) ->
    try
        match == re:run(Value, Regex, [{capture, none}])
    catch _:_ ->
        false
    end;
match({[{<<"$regex">>, _}]}, _Value, _Cmp) ->
    false;

match({[{<<"$size">>, Arg}]}, Values, _Cmp) when is_list(Values) ->
    length(Values) == Arg;
match({[{<<"$size">>, _}]}, _Value, _Cmp) ->
    false;

% We don't have any choice but to believe that the text
% index returned valid matches
match({[{<<"$default">>, _}]}, _Value, _Cmp) ->
    true;

% All other operators are internal assertion errors for
% matching because we either should've removed them during
% normalization or something else broke.
match({[{<<"$", _/binary>>=Op, _}]}, _, _) ->
    ?MANGO_ERROR({invalid_operator, Op});

% We need to traverse value to find field. The call to
% mango_doc:get_field/2 may return either not_found or
% bad_path in which case matching fails.
match({[{Field, Cond}]}, Value, Cmp) ->
    case mango_doc:get_field(Value, Field) of
        not_found when Cond == {[{<<"$exists">>, false}]} ->
            true;
        not_found ->
            false;
        bad_path ->
            false;
        SubValue when Field == <<"_id">> ->
            match(Cond, SubValue, fun mango_json:cmp_raw/2);
        SubValue ->
            match(Cond, SubValue, Cmp)
    end;

match({[_, _ | _] = _Props} = Sel, _Value, _Cmp) ->
    erlang:error({unnormalized_selector, Sel}).


% Returns true if Selector requires all
% fields in RequiredFields to exist in any matching documents.

% For each condition in the selector, check
% whether the field is in RequiredFields.
% If it is, remove it from RequiredFields and continue
% until we match then all or run out of selector to
% match against.

has_required_fields(Selector, RequiredFields) ->
    Remainder = has_required_fields_int(Selector, RequiredFields),
    Remainder == [].

% Empty selector
has_required_fields_int({[]}, Remainder) ->
    Remainder;

% No more required fields
has_required_fields_int(_, []) ->
    [];

% No more selector
has_required_fields_int([], Remainder) ->
    Remainder;

has_required_fields_int(Selector, RequiredFields) when not is_list(Selector) ->
    has_required_fields_int([Selector], RequiredFields);

% We can "see" through $and operator. Iterate
% through the list of child operators.
has_required_fields_int([{[{<<"$and">>, Args}]}], RequiredFields)
        when is_list(Args) ->
    has_required_fields_int(Args, RequiredFields);

% We can "see" through $or operator. Required fields
% must be covered by all children.
has_required_fields_int([{[{<<"$or">>, Args}]} | Rest], RequiredFields)
        when is_list(Args) ->
    Remainder0 = lists:foldl(fun(Arg, Acc) ->
        % for each child test coverage against the full
        % set of required fields
        Remainder = has_required_fields_int(Arg, RequiredFields),

        % collect the remaining fields across all children
        Acc ++ Remainder
    end, [], Args),

    % remove duplicate fields
    Remainder1 = lists:usort(Remainder0),
    has_required_fields_int(Rest, Remainder1);

% Handle $and operator where it has peers. Required fields
% can be covered by any child.
has_required_fields_int([{[{<<"$and">>, Args}]} | Rest], RequiredFields)
        when is_list(Args) ->
    Remainder = has_required_fields_int(Args, RequiredFields),
    has_required_fields_int(Rest, Remainder);

has_required_fields_int([{[{Field, Cond}]} | Rest], RequiredFields) ->
    case Cond of
        % $exists:false is a special case - this is the only operator
        % that explicitly does not require a field to exist
        {[{<<"$exists">>, false}]} ->
            has_required_fields_int(Rest, RequiredFields);
        _ ->
            has_required_fields_int(Rest, lists:delete(Field, RequiredFields))
    end.


% Returns true if a field in the selector is a constant value e.g. {a: {$eq: 1}}
is_constant_field({[]}, _Field) ->
    false;

is_constant_field(Selector, Field) when not is_list(Selector) ->
    is_constant_field([Selector], Field);

is_constant_field([], _Field) ->
    false;

is_constant_field([{[{<<"$and">>, Args}]}], Field) when is_list(Args) ->
    lists:any(fun(Arg) -> is_constant_field(Arg, Field) end, Args);

is_constant_field([{[{<<"$and">>, Args}]}], Field) ->
    is_constant_field(Args, Field);

is_constant_field([{[{Field, {[{Cond, _Val}]}}]} | _Rest], Field) ->
    Cond =:= <<"$eq">>;

is_constant_field([{[{_UnMatched, _}]} | Rest], Field) ->
    is_constant_field(Rest, Field).


%%%%%%%% module tests below %%%%%%%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

is_constant_field_basic_test() ->
    Selector = normalize({[{<<"A">>, <<"foo">>}]}),
    Field = <<"A">>,
    ?assertEqual(true, is_constant_field(Selector, Field)).

is_constant_field_basic_two_test() ->
    Selector = normalize({[{<<"$and">>,
        [
            {[{<<"cars">>,{[{<<"$eq">>,<<"2">>}]}}]},
            {[{<<"age">>,{[{<<"$gt">>,10}]}}]}
        ]
    }]}),
    Field = <<"cars">>,
    ?assertEqual(true, is_constant_field(Selector, Field)).

is_constant_field_not_eq_test() ->
    Selector = normalize({[{<<"$and">>,
        [
            {[{<<"cars">>,{[{<<"$eq">>,<<"2">>}]}}]},
            {[{<<"age">>,{[{<<"$gt">>,10}]}}]}
        ]
    }]}),
    Field = <<"age">>,
    ?assertEqual(false, is_constant_field(Selector, Field)).

is_constant_field_missing_field_test() ->
    Selector = normalize({[{<<"$and">>,
        [
            {[{<<"cars">>,{[{<<"$eq">>,<<"2">>}]}}]},
            {[{<<"age">>,{[{<<"$gt">>,10}]}}]}
        ]
    }]}),
    Field = <<"wrong">>,
    ?assertEqual(false, is_constant_field(Selector, Field)).

is_constant_field_or_field_test() ->
    Selector = {[{<<"$or">>,
          [
              {[{<<"A">>, <<"foo">>}]},
              {[{<<"B">>, <<"foo">>}]}
          ]
    }]},
    Normalized = normalize(Selector),
    Field = <<"A">>,
    ?assertEqual(false, is_constant_field(Normalized, Field)).

is_constant_field_empty_selector_test() ->
    Selector = normalize({[]}),
    Field = <<"wrong">>,
    ?assertEqual(false, is_constant_field(Selector, Field)).

is_constant_nested_and_test() ->
    Selector1 = {[{<<"$and">>,
          [
              {[{<<"A">>, <<"foo">>}]}
          ]
    }]},
    Selector2 = {[{<<"$and">>,
          [
              {[{<<"B">>, {[{<<"$gt">>,10}]}}]}
          ]
    }]},
    Selector = {[{<<"$and">>,
          [
              Selector1,
              Selector2
          ]
    }]},

    Normalized = normalize(Selector),
    ?assertEqual(true, is_constant_field(Normalized, <<"A">>)),
    ?assertEqual(false, is_constant_field(Normalized, <<"B">>)).

is_constant_combined_or_and_equals_test() ->
    Selector = {[{<<"A">>, "foo"},
          {<<"$or">>,
              [
                  {[{<<"B">>, <<"bar">>}]},
                  {[{<<"B">>, <<"baz">>}]}
              ]
          },
		  {<<"C">>, "qux"}
	]},
    Normalized = normalize(Selector),
    ?assertEqual(true, is_constant_field(Normalized, <<"C">>)),
    ?assertEqual(false, is_constant_field(Normalized, <<"B">>)).

has_required_fields_basic_test() ->
    RequiredFields = [<<"A">>],
    Selector = {[{<<"A">>, <<"foo">>}]},
    Normalized = normalize(Selector),
    ?assertEqual(true, has_required_fields(Normalized, RequiredFields)).

has_required_fields_basic_failure_test() ->
    RequiredFields = [<<"B">>],
    Selector = {[{<<"A">>, <<"foo">>}]},
    Normalized = normalize(Selector),
    ?assertEqual(false, has_required_fields(Normalized, RequiredFields)).

has_required_fields_empty_selector_test() ->
    RequiredFields = [<<"A">>],
    Selector = {[]},
    Normalized = normalize(Selector),
    ?assertEqual(false, has_required_fields(Normalized, RequiredFields)).

has_required_fields_exists_false_test() ->
    RequiredFields = [<<"A">>],
    Selector = {[{<<"A">>,{[{<<"$exists">>, false}]}}]},
    Normalized = normalize(Selector),
    ?assertEqual(false, has_required_fields(Normalized, RequiredFields)).

has_required_fields_and_true_test() ->
    RequiredFields = [<<"A">>],
    Selector = {[{<<"$and">>,
          [
              {[{<<"A">>, <<"foo">>}]},
              {[{<<"B">>, <<"foo">>}]}
          ]
    }]},
    Normalized = normalize(Selector),
    ?assertEqual(true, has_required_fields(Normalized, RequiredFields)).

has_required_fields_nested_and_true_test() ->
    RequiredFields = [<<"A">>, <<"B">>],
    Selector1 = {[{<<"$and">>,
          [
              {[{<<"A">>, <<"foo">>}]}
          ]
    }]},
    Selector2 = {[{<<"$and">>,
          [
              {[{<<"B">>, <<"foo">>}]}
          ]
    }]},
    Selector = {[{<<"$and">>,
          [
              Selector1,
              Selector2
          ]
    }]},

    Normalized = normalize(Selector),
    ?assertEqual(true, has_required_fields(Normalized, RequiredFields)).

has_required_fields_and_false_test() ->
    RequiredFields = [<<"A">>, <<"C">>],
    Selector = {[{<<"$and">>,
          [
              {[{<<"A">>, <<"foo">>}]},
              {[{<<"B">>, <<"foo">>}]}
          ]
    }]},
    Normalized = normalize(Selector),
    ?assertEqual(false, has_required_fields(Normalized, RequiredFields)).

has_required_fields_or_false_test() ->
    RequiredFields = [<<"A">>],
    Selector = {[{<<"$or">>,
          [
              {[{<<"A">>, <<"foo">>}]},
              {[{<<"B">>, <<"foo">>}]}
          ]
    }]},
    Normalized = normalize(Selector),
    ?assertEqual(false, has_required_fields(Normalized, RequiredFields)).

has_required_fields_or_true_test() ->
    RequiredFields = [<<"A">>, <<"B">>, <<"C">>],
    Selector = {[{<<"A">>, "foo"},
          {<<"$or">>,
              [
                  {[{<<"B">>, <<"bar">>}]},
                  {[{<<"B">>, <<"baz">>}]}
              ]
          },
		  {<<"C">>, "qux"}
	]},
    Normalized = normalize(Selector),
    ?assertEqual(true, has_required_fields(Normalized, RequiredFields)).

has_required_fields_and_nested_or_true_test() ->
    RequiredFields = [<<"A">>, <<"B">>],
    Selector1 = {[{<<"$and">>,
          [
              {[{<<"A">>, <<"foo">>}]}
          ]
    }]},
    Selector2 = {[{<<"$or">>,
          [
              {[{<<"B">>, <<"foo">>}]},
              {[{<<"B">>, <<"foo">>}]}
          ]
    }]},
    Selector = {[{<<"$and">>,
          [
              Selector1,
              Selector2
          ]
    }]},
    Normalized = normalize(Selector),
    ?assertEqual(true, has_required_fields(Normalized, RequiredFields)),

    SelectorReverse = {[{<<"$and">>,
          [
              Selector2,
              Selector1
          ]
    }]},
    NormalizedReverse = normalize(SelectorReverse),
    ?assertEqual(true, has_required_fields(NormalizedReverse, RequiredFields)).

has_required_fields_and_nested_or_false_test() ->
    RequiredFields = [<<"A">>, <<"B">>],
    Selector1 = {[{<<"$and">>,
          [
              {[{<<"A">>, <<"foo">>}]}
          ]
    }]},
    Selector2 = {[{<<"$or">>,
          [
              {[{<<"A">>, <<"foo">>}]},
              {[{<<"B">>, <<"foo">>}]}
          ]
    }]},
    Selector = {[{<<"$and">>,
          [
              Selector1,
              Selector2
          ]
    }]},
    Normalized = normalize(Selector),
    ?assertEqual(false, has_required_fields(Normalized, RequiredFields)),

    SelectorReverse = {[{<<"$and">>,
          [
              Selector2,
              Selector1
          ]
    }]},

    NormalizedReverse = normalize(SelectorReverse),
    ?assertEqual(false, has_required_fields(NormalizedReverse, RequiredFields)).

has_required_fields_or_nested_and_true_test() ->
    RequiredFields = [<<"A">>],
    Selector1 = {[{<<"$and">>,
          [
              {[{<<"A">>, <<"foo">>}]}
          ]
    }]},
    Selector2 = {[{<<"$and">>,
          [
              {[{<<"A">>, <<"foo">>}]}
          ]
    }]},
    Selector = {[{<<"$or">>,
          [
              Selector1,
              Selector2
          ]
    }]},
    Normalized = normalize(Selector),
    ?assertEqual(true, has_required_fields(Normalized, RequiredFields)).

has_required_fields_or_nested_or_true_test() ->
    RequiredFields = [<<"A">>],
    Selector1 = {[{<<"$or">>,
          [
              {[{<<"A">>, <<"foo">>}]}
          ]
    }]},
    Selector2 = {[{<<"$or">>,
          [
              {[{<<"A">>, <<"bar">>}]}
          ]
    }]},
    Selector = {[{<<"$or">>,
          [
              Selector1,
              Selector2
          ]
    }]},
    Normalized = normalize(Selector),
    ?assertEqual(true, has_required_fields(Normalized, RequiredFields)).

has_required_fields_or_nested_or_false_test() ->
    RequiredFields = [<<"A">>],
    Selector1 = {[{<<"$or">>,
          [
              {[{<<"A">>, <<"foo">>}]}
          ]
    }]},
    Selector2 = {[{<<"$or">>,
          [
              {[{<<"B">>, <<"bar">>}]}
          ]
    }]},
    Selector = {[{<<"$or">>,
          [
              Selector1,
              Selector2
          ]
    }]},
    Normalized = normalize(Selector),
    ?assertEqual(false, has_required_fields(Normalized, RequiredFields)).

-endif.
