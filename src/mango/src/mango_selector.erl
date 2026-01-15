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
    is_constant_field/2,
    fields/1
]).

-include_lib("couch/include/couch_db.hrl").
-include("mango.hrl").

-record(ctx, {
    cmp,
    path = []
}).

-record(failure, {
    op,
    type = mismatch,
    params = [],
    path = []
}).

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

match_int(Selector, D) ->
    case match_failures(Selector, D) of
        [] -> true;
        [_ | _] -> false;
        Other -> Other
    end.

% An empty selector matches any value.
match_failures({[]}, _) ->
    [];
match_failures(Selector, #doc{body = Body}) ->
    match_failures(Selector, Body);
match_failures(Selector, {Props}) ->
    match(Selector, {Props}, #ctx{cmp = fun mango_json:cmp/2}).

% Convert each operator into a normalized version as well
% as convert an implicit operators into their explicit
% versions.
norm_ops({[{<<"$and">>, Args}]}) when is_list(Args) ->
    {[{<<"$and">>, [norm_ops(A) || A <- Args]}]};
norm_ops({[{<<"$and">>, Arg}]}) ->
    ?MANGO_ERROR({bad_arg, '$and', Arg});
norm_ops({[{<<"$or">>, Args}]}) when is_list(Args) ->
    {[{<<"$or">>, [norm_ops(A) || A <- Args]}]};
norm_ops({[{<<"$or">>, Arg}]}) ->
    ?MANGO_ERROR({bad_arg, '$or', Arg});
norm_ops({[{<<"$not">>, {_} = Arg}]}) ->
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
norm_ops({[{<<"$elemMatch">>, {_} = Arg}]}) ->
    {[{<<"$elemMatch">>, norm_ops(Arg)}]};
norm_ops({[{<<"$elemMatch">>, Arg}]}) ->
    ?MANGO_ERROR({bad_arg, '$elemMatch', Arg});
norm_ops({[{<<"$allMatch">>, {_} = Arg}]}) ->
    {[{<<"$allMatch">>, norm_ops(Arg)}]};
norm_ops({[{<<"$allMatch">>, Arg}]}) ->
    ?MANGO_ERROR({bad_arg, '$allMatch', Arg});
norm_ops({[{<<"$keyMapMatch">>, {_} = Arg}]}) ->
    {[{<<"$keyMapMatch">>, norm_ops(Arg)}]};
norm_ops({[{<<"$keyMapMatch">>, Arg}]}) ->
    ?MANGO_ERROR({bad_arg, '$keyMapMatch', Arg});
norm_ops({[{<<"$size">>, Arg}]}) when is_integer(Arg), Arg >= 0 ->
    {[{<<"$size">>, Arg}]};
norm_ops({[{<<"$size">>, Arg}]}) ->
    ?MANGO_ERROR({bad_arg, '$size', Arg});
norm_ops({[{<<"$text">>, Arg}]}) when
    is_binary(Arg);
    is_number(Arg);
    is_boolean(Arg)
->
    {[{<<"$default">>, {[{<<"$text">>, Arg}]}}]};
norm_ops({[{<<"$text">>, Arg}]}) ->
    ?MANGO_ERROR({bad_arg, '$text', Arg});
norm_ops({[{<<"$beginsWith">>, Arg}]} = Cond) when is_binary(Arg) ->
    case couch_util:validate_utf8(Arg) of
        true -> Cond;
        false -> ?MANGO_ERROR({bad_arg, '$beginsWith', Arg})
    end;
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
norm_ops({[{<<"$", _/binary>> = Op, _}]}) ->
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
% we can guarantee commutativity. We can't necessarily
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
norm_fields({[{<<"$default">>, {[{<<"$text">>, _Arg}]}}]} = Sel, <<>>) ->
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
    [];
match({[{<<"$and">>, Args}]}, Value, Ctx) ->
    MatchSubSel = fun(SubSel) -> match(SubSel, Value, Ctx) end,
    lists:flatmap(MatchSubSel, Args);
match({[{<<"$or">>, []}]}, _, _) ->
    [];
match({[{<<"$or">>, Args}]}, Value, Ctx) ->
    SubSelFailures = [match(A, Value, Ctx) || A <- Args],
    case lists:any(fun(Res) -> Res == [] end, SubSelFailures) of
        true -> [];
        _ -> lists:flatten(SubSelFailures)
    end;
% TODO: producing good failure messages requires that normalize/1 fully removes
% $not from the tree by pushing it to the leaves.
match({[{<<"$not">>, Arg}]}, Value, Ctx) ->
    case match(Arg, Value, Ctx) of
        [] -> [#failure{op = 'not', path = Ctx#ctx.path}];
        _ -> []
    end;
% All of the values in Args must exist in Values or
% Values == hd(Args) if Args is a single element list
% that contains a list.
match({[{<<"$all">>, []}]}, _Values, Ctx) ->
    % { "$all": [] } is defined to eval to false, so return a failure
    [#failure{op = all, params = [[]], path = Ctx#ctx.path}];
match({[{<<"$all">>, [A]}]}, Values, _Ctx) when is_list(A), A == Values ->
    [];
match({[{<<"$all">>, Args}]}, Values, Ctx) when is_list(Values) ->
    lists:flatmap(
        fun(Arg) ->
            case lists:member(Arg, Values) of
                true -> [];
                _ -> [#failure{op = all, params = [Arg], path = Ctx#ctx.path}]
            end
        end,
        Args
    );
match({[{<<"$all">>, _}]}, Value, Ctx) ->
    [#failure{op = all, type = bad_value, params = [Value], path = Ctx#ctx.path}];
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
match({[{<<>>, Arg}]}, Values, Ctx) ->
    match(Arg, Values, Ctx);
% Matches when any element in values matches the
% sub-selector Arg.
match({[{<<"$elemMatch">>, _Arg}]}, [], Ctx) ->
    [#failure{op = elemMatch, type = empty_list, path = Ctx#ctx.path}];
match({[{<<"$elemMatch">>, Arg}]}, Values, #ctx{path = Path} = Ctx) when is_list(Values) ->
    ValueFailures = [
        match(Arg, V, Ctx#ctx{path = [Idx | Path]})
     || {Idx, V} <- lists:enumerate(0, Values)
    ],
    case lists:member([], ValueFailures) of
        true -> [];
        _ -> lists:flatten(ValueFailures)
    end;
match({[{<<"$elemMatch">>, _}]}, Value, Ctx) ->
    [#failure{op = elemMatch, type = bad_value, params = [Value], path = Ctx#ctx.path}];
% Matches when all elements in values match the
% sub-selector Arg.
match({[{<<"$allMatch">>, Arg}]}, [_ | _] = Values, #ctx{path = Path} = Ctx) ->
    EnumValues = lists:enumerate(0, Values),
    MatchValue = fun({Idx, Value}) -> match(Arg, Value, Ctx#ctx{path = [Idx | Path]}) end,
    lists:flatmap(MatchValue, EnumValues);
match({[{<<"$allMatch">>, _}]}, Value, Ctx) ->
    [#failure{op = allMatch, type = bad_value, params = [Value], path = Ctx#ctx.path}];
% Matches when any key in the map value matches the
% sub-selector Arg.
match({[{<<"$keyMapMatch">>, _Arg}]}, {[]}, Ctx) ->
    [#failure{op = keyMapMatch, type = empty_list, path = Ctx#ctx.path}];
match({[{<<"$keyMapMatch">>, Arg}]}, {Value}, Ctx) when is_list(Value) ->
    KeyFailures = [match(Arg, K, Ctx) || {K, _} <- Value],
    case lists:member([], KeyFailures) of
        true -> [];
        _ -> lists:flatten(KeyFailures)
    end;
match({[{<<"$keyMapMatch">>, _}]}, Value, Ctx) ->
    [#failure{op = keyMapMatch, type = bad_value, params = [Value], path = Ctx#ctx.path}];
% Our comparison operators are fairly straight forward
match({[{<<"$lt">>, Arg}]}, Value, #ctx{cmp = Cmp, path = Path}) ->
    compare(lt, Arg, Path, Cmp(Value, Arg) < 0);
match({[{<<"$lte">>, Arg}]}, Value, #ctx{cmp = Cmp, path = Path}) ->
    compare(lte, Arg, Path, Cmp(Value, Arg) =< 0);
match({[{<<"$eq">>, Arg}]}, Value, #ctx{cmp = Cmp, path = Path}) ->
    compare(eq, Arg, Path, Cmp(Value, Arg) == 0);
match({[{<<"$ne">>, Arg}]}, Value, #ctx{cmp = Cmp, path = Path}) ->
    compare(ne, Arg, Path, Cmp(Value, Arg) /= 0);
match({[{<<"$gte">>, Arg}]}, Value, #ctx{cmp = Cmp, path = Path}) ->
    compare(gte, Arg, Path, Cmp(Value, Arg) >= 0);
match({[{<<"$gt">>, Arg}]}, Value, #ctx{cmp = Cmp, path = Path}) ->
    compare(gt, Arg, Path, Cmp(Value, Arg) > 0);
match({[{<<"$in">>, Args}]}, Values, #ctx{cmp = Cmp, path = Path}) when is_list(Values) ->
    Pred = fun(Arg) ->
        lists:foldl(
            fun(Value, Match) ->
                (Cmp(Value, Arg) == 0) or Match
            end,
            false,
            Values
        )
    end,
    case lists:any(Pred, Args) of
        true -> [];
        _ -> [#failure{op = in, params = [Args], path = Path}]
    end;
match({[{<<"$in">>, Args}]}, Value, #ctx{cmp = Cmp, path = Path}) ->
    Pred = fun(Arg) -> Cmp(Value, Arg) == 0 end,
    case lists:any(Pred, Args) of
        true -> [];
        _ -> [#failure{op = in, params = [Args], path = Path}]
    end;
match({[{<<"$nin">>, Args}]}, Values, #ctx{cmp = Cmp, path = Path}) when is_list(Values) ->
    Pred = fun(Arg) ->
        lists:foldl(
            fun(Value, Match) ->
                (Cmp(Value, Arg) /= 0) and Match
            end,
            true,
            Values
        )
    end,
    case lists:all(Pred, Args) of
        true -> [];
        _ -> [#failure{op = nin, params = [Args], path = Path}]
    end;
match({[{<<"$nin">>, Args}]}, Value, #ctx{cmp = Cmp, path = Path}) ->
    Pred = fun(Arg) -> Cmp(Value, Arg) /= 0 end,
    case lists:all(Pred, Args) of
        true -> [];
        _ -> [#failure{op = nin, params = [Args], path = Path}]
    end;
% This logic is a bit subtle. Basically, if value is
% not undefined, then it exists.
match({[{<<"$exists">>, ShouldExist}]}, Value, Ctx) ->
    case {ShouldExist, Value} of
        {true, undefined} -> [#failure{op = exists, params = [ShouldExist], path = Ctx#ctx.path}];
        {true, _} -> [];
        {false, undefined} -> [];
        {false, _} -> [#failure{op = exists, params = [ShouldExist], path = Ctx#ctx.path}]
    end;
match({[{<<"$type">>, Arg}]}, Value, Ctx) when is_binary(Arg) ->
    case mango_json:type(Value) of
        Arg -> [];
        _ -> [#failure{op = type, params = [Arg], path = Ctx#ctx.path}]
    end;
match({[{<<"$mod">>, [D, R]}]}, Value, Ctx) when is_integer(Value) ->
    case Value rem D of
        R -> [];
        _ -> [#failure{op = mod, params = [D, R], path = Ctx#ctx.path}]
    end;
match({[{<<"$mod">>, _}]}, Value, Ctx) ->
    [#failure{op = mod, type = bad_value, params = [Value], path = Ctx#ctx.path}];
match({[{<<"$beginsWith">>, Prefix}]}, Value, Ctx) when is_binary(Prefix), is_binary(Value) ->
    case string:prefix(Value, Prefix) of
        nomatch -> [#failure{op = beginsWith, params = [Prefix], path = Ctx#ctx.path}];
        _ -> []
    end;
% When Value is not a string, do not match
match({[{<<"$beginsWith">>, Prefix}]}, Value, Ctx) when is_binary(Prefix) ->
    [#failure{op = beginsWith, type = bad_value, params = [Value], path = Ctx#ctx.path}];
match({[{<<"$regex">>, Regex}]}, Value, Ctx) when is_binary(Value) ->
    try
        case re:run(Value, Regex, [{capture, none}]) of
            match -> [];
            _ -> [#failure{op = regex, params = [Regex], path = Ctx#ctx.path}]
        end
    catch
        _:_ ->
            [#failure{op = regex, params = [Regex], path = Ctx#ctx.path}]
    end;
match({[{<<"$regex">>, _}]}, Value, Ctx) ->
    [#failure{op = regex, type = bad_value, params = [Value], path = Ctx#ctx.path}];
match({[{<<"$size">>, Arg}]}, Values, Ctx) when is_list(Values) ->
    case length(Values) of
        Arg -> [];
        _ -> [#failure{op = size, params = [Arg], path = Ctx#ctx.path}]
    end;
match({[{<<"$size">>, _}]}, Value, Ctx) ->
    [#failure{op = size, type = bad_value, params = [Value], path = Ctx#ctx.path}];
% We don't have any choice but to believe that the text
% index returned valid matches
match({[{<<"$default">>, _}]}, _Value, _Ctx) ->
    [];
% All other operators are internal assertion errors for
% matching because we either should've removed them during
% normalization or something else broke.
match({[{<<"$", _/binary>> = Op, _}]}, _, _) ->
    ?MANGO_ERROR({invalid_operator, Op});
% We need to traverse value to find field. The call to
% mango_doc:get_field/2 may return either not_found or
% bad_path in which case matching fails.
match({[{Field, Cond}]}, Value, #ctx{path = Path} = Ctx) ->
    InnerCtx = Ctx#ctx{path = [Field | Path]},
    case mango_doc:get_field(Value, Field) of
        not_found when Cond == {[{<<"$exists">>, false}]} ->
            [];
        not_found ->
            [#failure{op = field, type = not_found, path = InnerCtx#ctx.path}];
        bad_path ->
            [#failure{op = field, type = bad_path, path = InnerCtx#ctx.path}];
        SubValue when Field == <<"_id">> ->
            match(Cond, SubValue, InnerCtx#ctx{cmp = fun mango_json:cmp_raw/2});
        SubValue ->
            match(Cond, SubValue, InnerCtx)
    end;
match({[_, _ | _] = _Props} = Sel, _Value, _Ctx) ->
    error({unnormalized_selector, Sel}).

compare(Op, Arg, Path, Cond) ->
    case Cond of
        true -> [];
        _ -> [#failure{op = Op, params = [Arg], path = Path}]
    end.

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
has_required_fields_int([{[{<<"$and">>, Args}]}], RequiredFields) when
    is_list(Args)
->
    has_required_fields_int(Args, RequiredFields);
% We can "see" through $or operator. Required fields
% must be covered by all children.
has_required_fields_int([{[{<<"$or">>, Args}]} | Rest], RequiredFields) when
    is_list(Args)
->
    Remainder0 = lists:foldl(
        fun(Arg, Acc) ->
            % for each child test coverage against the full
            % set of required fields
            Remainder = has_required_fields_int(Arg, RequiredFields),

            % collect the remaining fields across all children
            Acc ++ Remainder
        end,
        [],
        Args
    ),

    % remove duplicate fields
    Remainder1 = lists:usort(Remainder0),
    has_required_fields_int(Rest, Remainder1);
% Handle $and operator where it has peers. Required fields
% can be covered by any child.
has_required_fields_int([{[{<<"$and">>, Args}]} | Rest], RequiredFields) when
    is_list(Args)
->
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

-spec fields(selector()) -> fields().
fields({[{<<"$", _/binary>>, Args}]}) when is_list(Args) ->
    lists:flatmap(fun fields/1, Args);
fields({[{Field, _Cond}]}) ->
    [Field];
fields({[]}) ->
    [].

%%%%%%%% module tests below %%%%%%%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(TEST_DOC,
    {[
        {<<"_id">>, <<"foo">>},
        {<<"_rev">>, <<"bar">>},
        {<<"user_id">>, 11}
    ]}
).

is_constant_field_basic_test() ->
    Selector = normalize({[{<<"A">>, <<"foo">>}]}),
    Field = <<"A">>,
    ?assertEqual(true, is_constant_field(Selector, Field)).

is_constant_field_basic_two_test() ->
    Selector = normalize(
        {[
            {<<"$and">>, [
                {[{<<"cars">>, {[{<<"$eq">>, <<"2">>}]}}]},
                {[{<<"age">>, {[{<<"$gt">>, 10}]}}]}
            ]}
        ]}
    ),
    Field = <<"cars">>,
    ?assertEqual(true, is_constant_field(Selector, Field)).

is_constant_field_not_eq_test() ->
    Selector = normalize(
        {[
            {<<"$and">>, [
                {[{<<"cars">>, {[{<<"$eq">>, <<"2">>}]}}]},
                {[{<<"age">>, {[{<<"$gt">>, 10}]}}]}
            ]}
        ]}
    ),
    Field = <<"age">>,
    ?assertEqual(false, is_constant_field(Selector, Field)).

is_constant_field_missing_field_test() ->
    Selector = normalize(
        {[
            {<<"$and">>, [
                {[{<<"cars">>, {[{<<"$eq">>, <<"2">>}]}}]},
                {[{<<"age">>, {[{<<"$gt">>, 10}]}}]}
            ]}
        ]}
    ),
    Field = <<"wrong">>,
    ?assertEqual(false, is_constant_field(Selector, Field)).

is_constant_field_or_field_test() ->
    Selector =
        {[
            {<<"$or">>, [
                {[{<<"A">>, <<"foo">>}]},
                {[{<<"B">>, <<"foo">>}]}
            ]}
        ]},
    Normalized = normalize(Selector),
    Field = <<"A">>,
    ?assertEqual(false, is_constant_field(Normalized, Field)).

is_constant_field_empty_selector_test() ->
    Selector = normalize({[]}),
    Field = <<"wrong">>,
    ?assertEqual(false, is_constant_field(Selector, Field)).

is_constant_nested_and_test() ->
    Selector1 =
        {[
            {<<"$and">>, [
                {[{<<"A">>, <<"foo">>}]}
            ]}
        ]},
    Selector2 =
        {[
            {<<"$and">>, [
                {[{<<"B">>, {[{<<"$gt">>, 10}]}}]}
            ]}
        ]},
    Selector =
        {[
            {<<"$and">>, [
                Selector1,
                Selector2
            ]}
        ]},

    Normalized = normalize(Selector),
    ?assertEqual(true, is_constant_field(Normalized, <<"A">>)),
    ?assertEqual(false, is_constant_field(Normalized, <<"B">>)).

is_constant_combined_or_and_equals_test() ->
    Selector =
        {[
            {<<"A">>, "foo"},
            {<<"$or">>, [
                {[{<<"B">>, <<"bar">>}]},
                {[{<<"B">>, <<"baz">>}]}
            ]},
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
    Selector = {[{<<"A">>, {[{<<"$exists">>, false}]}}]},
    Normalized = normalize(Selector),
    ?assertEqual(false, has_required_fields(Normalized, RequiredFields)).

has_required_fields_and_true_test() ->
    RequiredFields = [<<"A">>],
    Selector =
        {[
            {<<"$and">>, [
                {[{<<"A">>, <<"foo">>}]},
                {[{<<"B">>, <<"foo">>}]}
            ]}
        ]},
    Normalized = normalize(Selector),
    ?assertEqual(true, has_required_fields(Normalized, RequiredFields)).

has_required_fields_nested_and_true_test() ->
    RequiredFields = [<<"A">>, <<"B">>],
    Selector1 =
        {[
            {<<"$and">>, [
                {[{<<"A">>, <<"foo">>}]}
            ]}
        ]},
    Selector2 =
        {[
            {<<"$and">>, [
                {[{<<"B">>, <<"foo">>}]}
            ]}
        ]},
    Selector =
        {[
            {<<"$and">>, [
                Selector1,
                Selector2
            ]}
        ]},

    Normalized = normalize(Selector),
    ?assertEqual(true, has_required_fields(Normalized, RequiredFields)).

has_required_fields_and_false_test() ->
    RequiredFields = [<<"A">>, <<"C">>],
    Selector =
        {[
            {<<"$and">>, [
                {[{<<"A">>, <<"foo">>}]},
                {[{<<"B">>, <<"foo">>}]}
            ]}
        ]},
    Normalized = normalize(Selector),
    ?assertEqual(false, has_required_fields(Normalized, RequiredFields)).

has_required_fields_or_false_test() ->
    RequiredFields = [<<"A">>],
    Selector =
        {[
            {<<"$or">>, [
                {[{<<"A">>, <<"foo">>}]},
                {[{<<"B">>, <<"foo">>}]}
            ]}
        ]},
    Normalized = normalize(Selector),
    ?assertEqual(false, has_required_fields(Normalized, RequiredFields)).

has_required_fields_or_true_test() ->
    RequiredFields = [<<"A">>, <<"B">>, <<"C">>],
    Selector =
        {[
            {<<"A">>, "foo"},
            {<<"$or">>, [
                {[{<<"B">>, <<"bar">>}]},
                {[{<<"B">>, <<"baz">>}]}
            ]},
            {<<"C">>, "qux"}
        ]},
    Normalized = normalize(Selector),
    ?assertEqual(true, has_required_fields(Normalized, RequiredFields)).

has_required_fields_and_nested_or_true_test() ->
    RequiredFields = [<<"A">>, <<"B">>],
    Selector1 =
        {[
            {<<"$and">>, [
                {[{<<"A">>, <<"foo">>}]}
            ]}
        ]},
    Selector2 =
        {[
            {<<"$or">>, [
                {[{<<"B">>, <<"foo">>}]},
                {[{<<"B">>, <<"foo">>}]}
            ]}
        ]},
    Selector =
        {[
            {<<"$and">>, [
                Selector1,
                Selector2
            ]}
        ]},
    Normalized = normalize(Selector),
    ?assertEqual(true, has_required_fields(Normalized, RequiredFields)),

    SelectorReverse =
        {[
            {<<"$and">>, [
                Selector2,
                Selector1
            ]}
        ]},
    NormalizedReverse = normalize(SelectorReverse),
    ?assertEqual(true, has_required_fields(NormalizedReverse, RequiredFields)).

has_required_fields_and_nested_or_false_test() ->
    RequiredFields = [<<"A">>, <<"B">>],
    Selector1 =
        {[
            {<<"$and">>, [
                {[{<<"A">>, <<"foo">>}]}
            ]}
        ]},
    Selector2 =
        {[
            {<<"$or">>, [
                {[{<<"A">>, <<"foo">>}]},
                {[{<<"B">>, <<"foo">>}]}
            ]}
        ]},
    Selector =
        {[
            {<<"$and">>, [
                Selector1,
                Selector2
            ]}
        ]},
    Normalized = normalize(Selector),
    ?assertEqual(false, has_required_fields(Normalized, RequiredFields)),

    SelectorReverse =
        {[
            {<<"$and">>, [
                Selector2,
                Selector1
            ]}
        ]},

    NormalizedReverse = normalize(SelectorReverse),
    ?assertEqual(false, has_required_fields(NormalizedReverse, RequiredFields)).

has_required_fields_or_nested_and_true_test() ->
    RequiredFields = [<<"A">>],
    Selector1 =
        {[
            {<<"$and">>, [
                {[{<<"A">>, <<"foo">>}]}
            ]}
        ]},
    Selector2 =
        {[
            {<<"$and">>, [
                {[{<<"A">>, <<"foo">>}]}
            ]}
        ]},
    Selector =
        {[
            {<<"$or">>, [
                Selector1,
                Selector2
            ]}
        ]},
    Normalized = normalize(Selector),
    ?assertEqual(true, has_required_fields(Normalized, RequiredFields)).

has_required_fields_or_nested_or_true_test() ->
    RequiredFields = [<<"A">>],
    Selector1 =
        {[
            {<<"$or">>, [
                {[{<<"A">>, <<"foo">>}]}
            ]}
        ]},
    Selector2 =
        {[
            {<<"$or">>, [
                {[{<<"A">>, <<"bar">>}]}
            ]}
        ]},
    Selector =
        {[
            {<<"$or">>, [
                Selector1,
                Selector2
            ]}
        ]},
    Normalized = normalize(Selector),
    ?assertEqual(true, has_required_fields(Normalized, RequiredFields)).

has_required_fields_or_nested_or_false_test() ->
    RequiredFields = [<<"A">>],
    Selector1 =
        {[
            {<<"$or">>, [
                {[{<<"A">>, <<"foo">>}]}
            ]}
        ]},
    Selector2 =
        {[
            {<<"$or">>, [
                {[{<<"B">>, <<"bar">>}]}
            ]}
        ]},
    Selector =
        {[
            {<<"$or">>, [
                Selector1,
                Selector2
            ]}
        ]},
    Normalized = normalize(Selector),
    ?assertEqual(false, has_required_fields(Normalized, RequiredFields)).

check_match(Selector) ->
    % Call match_int/2 to avoid ERROR for missing metric; this is confusing
    % in the middle of test output.
    match_int(mango_selector:normalize(Selector), ?TEST_DOC).

%% This test shows the shape match/2 expects for its arguments.
match_demo_test() ->
    % matching
    ?assertEqual(true, check_match({[{<<"user_id">>, 11}]})),
    ?assertEqual(true, check_match({[{<<"_id">>, <<"foo">>}]})),
    ?assertEqual(true, check_match({[{<<"_id">>, <<"foo">>}, {<<"_rev">>, <<"bar">>}]})),
    % non-matching
    ?assertEqual(false, check_match({[{<<"user_id">>, 1234}]})),
    % string 11 doesn't match number 11
    ?assertEqual(false, check_match({[{<<"user_id">>, <<"11">>}]})),
    ?assertEqual(false, check_match({[{<<"_id">>, <<"foo">>}, {<<"_rev">>, <<"quux">>}]})).

fields_of(Selector) ->
    fields(test_util:as_selector(Selector)).

fields_empty_test() ->
    ?assertEqual([], fields_of(#{})).

fields_primitive_test() ->
    Selector = #{<<"field">> => undefined},
    ?assertEqual([<<"field">>], fields_of(Selector)).

fields_nested_test() ->
    Selector = #{<<"field1">> => #{<<"field2">> => undefined}},
    ?assertEqual([<<"field1.field2">>], fields_of(Selector)).

fields_and_test() ->
    Selector1 = #{<<"$and">> => []},
    ?assertEqual([], fields_of(Selector1)),
    Selector2 = #{
        <<"$and">> => [#{<<"field1">> => undefined}, #{<<"field2">> => undefined}]
    },
    ?assertEqual([<<"field1">>, <<"field2">>], fields_of(Selector2)).

fields_or_test() ->
    Selector1 = #{<<"$or">> => []},
    ?assertEqual([], fields_of(Selector1)),
    Selector2 = #{
        <<"$or">> => [#{<<"field1">> => undefined}, #{<<"field2">> => undefined}]
    },
    ?assertEqual([<<"field1">>, <<"field2">>], fields_of(Selector2)).

fields_nor_test() ->
    Selector1 = #{<<"$nor">> => []},
    ?assertEqual([], fields_of(Selector1)),
    Selector2 = #{
        <<"$nor">> => [#{<<"field1">> => undefined}, #{<<"field2">> => undefined}]
    },
    ?assertEqual([<<"field1">>, <<"field2">>], fields_of(Selector2)).

check_beginswith(Field, Prefix) ->
    Selector = {[{Field, {[{<<"$beginsWith">>, Prefix}]}}]},
    % Call match_int/2 to avoid ERROR for missing metric; this is confusing
    % in the middle of test output.
    match_int(mango_selector:normalize(Selector), ?TEST_DOC).

match_beginswith_errors_test() ->
    % matching
    ?assertEqual(true, check_beginswith(<<"_id">>, <<"f">>)),
    % no match (user_id field in the test doc contains an integer)
    ?assertEqual(false, check_beginswith(<<"user_id">>, <<"f">>)),
    % invalid (prefix is not a binary string)
    ?assertThrow(
        {mango_error, mango_selector, {invalid_operator, <<"$beginsWith">>}},
        check_beginswith(<<"user_id">>, 1)
    ),
    % invalid (prefix is not a utf8 string)
    InvalidArg = <<16#EF>>,
    ?assertThrow(
        {mango_error, mango_selector, {bad_arg, '$beginsWith', InvalidArg}},
        check_beginswith(<<"user_id">>, InvalidArg)
    ).

check_selector(Selector, Results) ->
    SelPos = normalize({[{<<"x">>, Selector}]}),
    SelNeg = normalize({[{<<"x">>, {[{<<"$not">>, Selector}]}}]}),

    Check = fun({Result, Value}) ->
        Doc = {[{<<"x">>, Value}]},
        ?assertEqual(Result, match_int(SelPos, Doc)),
        ?assertEqual(not Result, match_int(SelNeg, Doc))
    end,

    lists:foreach(Check, Results).

match_lt_test() ->
    check_selector({[{<<"$lt">>, 5}]}, [{true, 4}, {false, 5}, {false, 6}]),

    check_selector({[{<<"$lt">>, <<"hello">>}]}, [
        {true, <<"held">>},
        {false, <<"hello">>},
        {false, <<"help">>}
    ]),

    check_selector({[{<<"$lt">>, [1, 2, 3]}]}, [
        {true, [1, 2, 2]},
        {true, [1, 2]},
        {false, [1, 2, 3]},
        {false, [1, 2, 4]},
        {false, [1, 3]}
    ]).

match_lte_test() ->
    check_selector({[{<<"$lte">>, 5}]}, [{true, 4}, {true, 5}, {false, 6}]),

    check_selector({[{<<"$lte">>, <<"hello">>}]}, [
        {true, <<"held">>},
        {true, <<"hello">>},
        {false, <<"help">>}
    ]),

    check_selector({[{<<"$lte">>, [1, 2, 3]}]}, [
        {true, [1, 2, 2]},
        {true, [1, 2]},
        {true, [1, 2, 3]},
        {false, [1, 2, 4]},
        {false, [1, 3]}
    ]).

match_gt_test() ->
    check_selector({[{<<"$gt">>, 5}]}, [{false, 4}, {false, 5}, {true, 6}]),

    check_selector({[{<<"$gt">>, <<"hello">>}]}, [
        {false, <<"held">>},
        {false, <<"hello">>},
        {true, <<"help">>}
    ]),

    check_selector({[{<<"$gt">>, [1, 2, 3]}]}, [
        {false, [1, 2, 2]},
        {false, [1, 2]},
        {false, [1, 2, 3]},
        {true, [1, 2, 4]},
        {true, [1, 3]}
    ]).

match_gte_test() ->
    check_selector({[{<<"$gte">>, 5}]}, [{false, 4}, {true, 5}, {true, 6}]),

    check_selector({[{<<"$gte">>, <<"hello">>}]}, [
        {false, <<"held">>},
        {true, <<"hello">>},
        {true, <<"help">>}
    ]),

    check_selector({[{<<"$gte">>, [1, 2, 3]}]}, [
        {false, [1, 2, 2]},
        {false, [1, 2]},
        {true, [1, 2, 3]},
        {true, [1, 2, 4]},
        {true, [1, 3]}
    ]).

match_eq_test() ->
    check_selector({[{<<"$eq">>, 5}]}, [{true, 5}, {false, 6}]),
    check_selector({[{<<"$eq">>, <<"hello">>}]}, [{true, <<"hello">>}, {false, <<"help">>}]),

    check_selector({[{<<"$eq">>, [1, [2, 3, 4], 5]}]}, [
        {true, [1, [2, 3, 4], 5]},
        {false, [1, [2, 3, 4]]},
        {false, [1, [2, 3, 4], 5, 6]},
        {false, [1, [2, 7, 4], 5]}
    ]),

    check_selector({[{<<"$eq">>, {[{<<"a">>, {[{<<"b">>, {[{<<"c">>, 7}]}}]}}]}}]}, [
        {true, {[{<<"a">>, {[{<<"b">>, {[{<<"c">>, 7}]}}]}}]}},
        {false, {[{<<"a">>, {[{<<"b">>, {[{<<"c">>, 8}]}}]}}]}},
        {false, {[{<<"a">>, {[{<<"b">>, {[{<<"d">>, 7}]}}]}}]}},
        {false, {[{<<"a">>, {[{<<"d">>, {[{<<"c">>, 7}]}}]}}]}}
    ]).

match_ne_test() ->
    check_selector({[{<<"$ne">>, 5}]}, [{false, 5}, {true, 6}]),

    % the %ne operator still requires a value to be present...
    SelInt = normalize({[{<<"x">>, {[{<<"$ne">>, 5}]}}]}),
    ?assertEqual(false, match_int(SelInt, {[]})),

    % ... which, due to normalization, means that using $not with $eq does not
    % match the empty doc
    SelNotEq = normalize({[{<<"$not">>, {[{<<"x">>, 5}]}}]}),
    ?assertEqual(false, match_int(SelNotEq, {[]})),

    check_selector({[{<<"$ne">>, <<"hello">>}]}, [{false, <<"hello">>}, {true, <<"help">>}]),

    check_selector({[{<<"$ne">>, [1, [2, 3, 4], 5]}]}, [
        {false, [1, [2, 3, 4], 5]},
        {true, [1, [2, 3, 4]]},
        {true, [1, [2, 3, 4], 5, 6]},
        {true, [1, [2, 7, 4], 5]}
    ]),

    check_selector({[{<<"$ne">>, {[{<<"a">>, {[{<<"b">>, {[{<<"c">>, 7}]}}]}}]}}]}, [
        {false, {[{<<"a">>, {[{<<"b">>, {[{<<"c">>, 7}]}}]}}]}},
        {true, {[{<<"a">>, {[{<<"b">>, {[{<<"c">>, 8}]}}]}}]}},
        {true, {[{<<"a">>, {[{<<"b">>, {[{<<"d">>, 7}]}}]}}]}},
        {true, {[{<<"a">>, {[{<<"d">>, {[{<<"c">>, 7}]}}]}}]}}
    ]).

match_in_test() ->
    check_selector({[{<<"$in">>, []}]}, [
        {false, 0},
        {false, true},
        {false, <<"foo">>}
    ]),

    check_selector(
        {[
            {<<"$in">>, [
                42,
                false,
                <<"bar">>,
                [[<<"nested">>], <<"list">>],
                {[{<<"b">>, 2}]}
            ]}
        ]},
        [
            {true, 42},
            {true, false},
            {true, <<"bar">>},
            {true, {[{<<"b">>, 2}]}},

            {false, 43},
            {false, true},
            {false, <<"bars">>},
            {false, {[{<<"b">>, 2}, {<<"c">>, 3}]}},

            % when the input is an array, $in matches if any of the array items
            % match...
            {true, [0, 42]},
            {true, [0, false]},
            {true, [0, <<"bar">>]},
            {true, [0, {[{<<"b">>, 2}]}]},

            % ... which means it doesn't directly match when one of the
            % candiate values is itself an array
            {false, [[<<"nested">>], <<"list">>]},
            {true, [0, [[<<"nested">>], <<"list">>]]}
        ]
    ).

match_nin_test() ->
    check_selector({[{<<"$nin">>, []}]}, [
        {true, 0},
        {true, true},
        {true, <<"foo">>}
    ]),

    check_selector(
        {[
            {<<"$nin">>, [
                42,
                false,
                <<"bar">>,
                [[<<"nested">>], <<"list">>],
                {[{<<"b">>, 2}]}
            ]}
        ]},
        [
            {false, 42},
            {false, false},
            {false, <<"bar">>},
            {false, {[{<<"b">>, 2}]}},

            {true, 43},
            {true, true},
            {true, <<"bars">>},
            {true, {[{<<"b">>, 2}, {<<"c">>, 3}]}},

            % when the input is an array, $nin matches if none of the array items
            % match...
            {false, [0, 42]},
            {false, [0, false]},
            {false, [0, <<"bar">>]},
            {false, [0, {[{<<"b">>, 2}]}]},

            % ... which means it doesn't directly match when one of the
            % candiate values is itself an array
            {true, [[<<"nested">>], <<"list">>]},
            {false, [0, [[<<"nested">>], <<"list">>]]}
        ]
    ).

match_all_test() ->
    % { "$all": [] } matches nothing, not even arrays
    check_selector({[{<<"$all">>, []}]}, [
        {false, []},
        {false, [42]},
        {false, {[]}},
        {false, <<"foo">>}
    ]),

    % normally, input lists can contain the required items in any order
    check_selector({[{<<"$all">>, [1, 2, 3, 4]}]}, [
        {true, [3, 2, 4, 1]},
        {true, [0, 4, 3, 5, 2, 1, 6]},
        {false, [3, 2, 4]},
        {false, []}
    ]),

    % negation means the input must lack at least one of the items
    check_selector({[{<<"$not">>, {[{<<"$all">>, [1, 2, 3, 4]}]}}]}, [
        {true, [2, 4, 1]},
        {false, [2, 4, 1, 3]},
        {true, []}
    ]),

    % the special $all: [List] form allows the input to exactly match List...
    check_selector({[{<<"$all">>, [[1, 2, 3, 4]]}]}, [
        {true, [1, 2, 3, 4]},
        {false, [4, 3, 2, 1]},
        {false, [1, 3, 4]},
        {false, []},
        % ... or to contain List
        {true, [5, [1, 2, 3, 4], 6]},
        {false, [5, [1, 3, 4], 6]},
        {false, [5, [1, 3, 2, 4], 6]}
    ]),

    % the special behaviour of $all: [X] only applies when X is a list
    check_selector({[{<<"$all">>, [<<"hello">>]}]}, [
        {false, <<"hello">>},
        {true, [<<"hello">>]},
        {true, [0, <<"hello">>, 1]},
        {false, []}
    ]),

    % values must match exactly and not contain extra fields
    check_selector({[{<<"$all">>, [{[{<<"a">>, 1}]}]}]}, [
        {true, [{[{<<"a">>, 1}]}]},
        {false, [{[{<<"a">>, 1}, {<<"b">>, 2}]}]}
    ]).

match_exists_test() ->
    check_selector({[{<<"x">>, {[{<<"$exists">>, true}]}}]}, [
        {true, {[{<<"x">>, 0}]}},
        {false, {[{<<"y">>, 0}]}},
        {false, {[]}}
    ]),

    check_selector({[{<<"x">>, {[{<<"$exists">>, false}]}}]}, [
        {false, {[{<<"x">>, 0}]}},
        {true, {[{<<"y">>, 0}]}},
        {true, {[]}}
    ]),

    % due to normalizing to { "x": { "$ne": 0 } }, this does not match the empty doc
    SelNeg = normalize({[{<<"x">>, {[{<<"$not">>, {[{<<"$eq">>, 0}]}}]}}]}),
    SelPos = normalize({[{<<"x">>, 0}]}),
    ?assertEqual(false, match_int(SelNeg, {[]})),
    ?assertEqual(false, match_int(SelPos, {[]})),

    % including { "$exists": true } in the negated part *does* match the empty doc
    check_selector(
        {[
            {<<"x">>,
                {[
                    {<<"$not">>,
                        {[
                            {<<"$exists">>, true},
                            {<<"$eq">>, 0}
                        ]}}
                ]}}
        ]},
        [
            {true, {[{<<"x">>, 1}]}},
            {false, {[{<<"x">>, 0}]}},
            {true, {[]}}
        ]
    ).

match_type_test() ->
    check_selector({[{<<"$type">>, <<"null">>}]}, [
        {true, null},
        {false, false},
        {false, {[]}}
    ]),

    check_selector({[{<<"$type">>, <<"boolean">>}]}, [
        {true, true},
        {true, false},
        {false, 0}
    ]),

    check_selector({[{<<"$type">>, <<"number">>}]}, [
        {true, 42},
        {true, 3.14},
        {true, 0},
        {false, true},
        {false, [1]},
        {false, <<"1">>}
    ]),

    check_selector({[{<<"$type">>, <<"string">>}]}, [
        {true, <<"">>},
        {true, <<"hello">>},
        {false, []}
    ]),

    check_selector({[{<<"$type">>, <<"array">>}]}, [
        {true, []},
        {true, [1, 2]},
        {false, {[]}},
        {false, <<"hi">>}
    ]),

    check_selector({[{<<"$type">>, <<"object">>}]}, [
        {true, {[]}},
        {true, {[{<<"a">>, 1}]}},
        {false, [{<<"a">>, 1}]},
        {false, null}
    ]).

match_regex_test() ->
    check_selector({[{<<"$regex">>, <<"^[0-9a-f]+$">>}]}, [
        {false, <<"">>},
        {true, <<"3a0df5e">>},
        {false, <<"3a0gf5e">>},
        {false, 42}
    ]).

match_beginswith_test() ->
    check_selector({[{<<"$beginsWith">>, <<"foo">>}]}, [
        {true, <<"foo">>},
        {true, <<"food">>},
        {true, <<"fool me once">>},
        {false, <<"more food">>},
        {false, <<"fo">>},
        {false, 42}
    ]).

match_mod_test() ->
    check_selector({[{<<"$mod">>, [28, 1]}]}, [
        {true, 1},
        {true, 29},
        {true, 57},
        {false, 58},
        {false, <<"57">>}
    ]).

match_size_test() ->
    check_selector({[{<<"$size">>, 3}]}, [
        {false, 3},
        {false, <<"fun">>},
        {true, [0, 0, 0]},
        {false, [0, 0]},
        {false, [0, 0, 0, 0]}
    ]).

match_allmatch_test() ->
    % $allMatch is defined to return false for empty lists
    check_selector({[{<<"$allMatch">>, {[{<<"$eq">>, 0}]}}]}, [
        {false, []},
        {true, [0]},
        {false, [1]},
        {false, [0, 1]}
    ]),

    % because of their behaviour on empty lists, { "$not": { "$allMatch": S } }
    % is not equivalent to { "$elemMatch": { "$not": S } }
    check_selector({[{<<"$elemMatch">>, {[{<<"$ne">>, 0}]}}]}, [
        {false, []},
        {false, [0]},
        {true, [1]},
        {true, [0, 1]}
    ]).

match_elemmatch_test() ->
    check_selector({[{<<"$elemMatch">>, {[{<<"$eq">>, 0}]}}]}, [
        {false, []},
        {true, [0]},
        {false, [1]},
        {true, [0, 1]}
    ]).

match_keymapmatch_test() ->
    check_selector({[{<<"$keyMapMatch">>, {[{<<"$regex">>, <<"^[a-z]+$">>}]}}]}, [
        {true, {[{<<"hello">>, 0}]}},
        {true, {[{<<"a">>, 1}, {<<"b">>, 2}]}},
        {true, {[{<<"a">>, 1}, {<<"b4">>, 2}]}},
        {false, {[{<<"b4">>, 2}]}},
        {false, {[]}}
    ]).

match_object_test() ->
    Doc1 = {[]},
    Doc2 = {[{<<"x">>, {[]}}]},
    Doc3 = {[{<<"x">>, {[{<<"a">>, 1}]}}]},
    Doc4 = {[{<<"x">>, {[{<<"a">>, 1}, {<<"b">>, 2}]}}]},
    Doc5 = {[{<<"x">>, []}]},

    % the empty selector matches any document
    SelEmpty = normalize({[]}),
    ?assertEqual({[]}, SelEmpty),
    ?assertEqual(true, match_int(SelEmpty, Doc1)),
    ?assertEqual(true, match_int(SelEmpty, Doc2)),
    ?assertEqual(true, match_int(SelEmpty, Doc3)),
    ?assertEqual(true, match_int(SelEmpty, Doc4)),
    ?assertEqual(true, match_int(SelEmpty, Doc5)),

    % an inner empty object selector matches only empty objects
    SelEmptyField = normalize({[{<<"x">>, {[]}}]}),
    ?assertEqual({[{<<"x">>, {[{<<"$eq">>, {[]}}]}}]}, SelEmptyField),
    ?assertEqual(false, match_int(SelEmptyField, Doc1)),
    ?assertEqual(true, match_int(SelEmptyField, Doc2)),
    ?assertEqual(false, match_int(SelEmptyField, Doc3)),
    ?assertEqual(false, match_int(SelEmptyField, Doc4)),
    ?assertEqual(false, match_int(SelEmptyField, Doc5)),

    % negated empty object selector matches a value which is present and is not the empty object
    SelNotEmptyField = normalize({[{<<"$not">>, {[{<<"x">>, {[]}}]}}]}),
    ?assertEqual({[{<<"x">>, {[{<<"$ne">>, {[]}}]}}]}, SelNotEmptyField),
    ?assertEqual(false, match_int(SelNotEmptyField, Doc1)),
    ?assertEqual(false, match_int(SelNotEmptyField, Doc2)),
    ?assertEqual(true, match_int(SelNotEmptyField, Doc3)),
    ?assertEqual(true, match_int(SelNotEmptyField, Doc4)),
    ?assertEqual(true, match_int(SelNotEmptyField, Doc5)),

    % inner object selectors with fields match objects with at least those fields
    Sel1Field = normalize({[{<<"x">>, {[{<<"a">>, 1}]}}]}),
    ?assertEqual({[{<<"x.a">>, {[{<<"$eq">>, 1}]}}]}, Sel1Field),
    ?assertEqual(false, match_int(Sel1Field, Doc1)),
    ?assertEqual(false, match_int(Sel1Field, Doc2)),
    ?assertEqual(true, match_int(Sel1Field, Doc3)),
    ?assertEqual(true, match_int(Sel1Field, Doc4)),
    ?assertEqual(false, match_int(Sel1Field, Doc5)),

    % inner object selectors with multiple fields are normalized with $and
    Sel2Field = normalize({[{<<"x">>, {[{<<"a">>, 1}, {<<"b">>, 2}]}}]}),
    ?assertEqual(
        {[
            {<<"$and">>, [
                {[{<<"x.a">>, {[{<<"$eq">>, 1}]}}]},
                {[{<<"x.b">>, {[{<<"$eq">>, 2}]}}]}
            ]}
        ]},
        Sel2Field
    ),
    ?assertEqual(false, match_int(Sel2Field, Doc1)),
    ?assertEqual(false, match_int(Sel2Field, Doc2)),
    ?assertEqual(false, match_int(Sel2Field, Doc3)),
    ?assertEqual(true, match_int(Sel2Field, Doc4)),
    ?assertEqual(false, match_int(Sel2Field, Doc5)),

    % check shorthand syntax
    SelShort = normalize({[{<<"x.b">>, 2}]}),
    ?assertEqual({[{<<"x.b">>, {[{<<"$eq">>, 2}]}}]}, SelShort),
    ?assertEqual(false, match_int(SelShort, Doc1)),
    ?assertEqual(false, match_int(SelShort, Doc2)),
    ?assertEqual(false, match_int(SelShort, Doc3)),
    ?assertEqual(true, match_int(SelShort, Doc4)),
    ?assertEqual(false, match_int(SelShort, Doc5)).

match_and_test() ->
    % $and with an empty array matches anything
    SelEmpty = normalize({[{<<"x">>, {[{<<"$and">>, []}]}}]}),
    ?assertEqual(true, match_int(SelEmpty, {[{<<"x">>, 0}]})),
    ?assertEqual(true, match_int(SelEmpty, {[{<<"x">>, false}]})),
    ?assertEqual(true, match_int(SelEmpty, {[{<<"x">>, []}]})),
    ?assertEqual(true, match_int(SelEmpty, {[]})),

    % due to { "$or": [] } matching anything, negating { "$and": [] } also
    % matches anything
    SelNotEmpty = normalize({[{<<"x">>, {[{<<"$not">>, {[{<<"$and">>, []}]}}]}}]}),
    ?assertEqual(true, match_int(SelNotEmpty, {[{<<"x">>, 0}]})),
    ?assertEqual(true, match_int(SelNotEmpty, {[{<<"x">>, false}]})),
    ?assertEqual(true, match_int(SelNotEmpty, {[{<<"x">>, []}]})),

    % and, because { "x": { "$and": [A, B] } } normalizes to
    % { "$and": [{ "x": A }, { "x": B }] }, that means
    % { "x": { "$not": { "$and": [] } } } normalizes to { "$or": [] },
    % so it matches docs where "x" is not present
    ?assertEqual(true, match_int(SelNotEmpty, {[]})),

    % $and with multiple selectors matches if all selectors match
    SelMulti = normalize(
        {[
            {<<"x">>,
                {[
                    {<<"$and">>, [
                        {[{<<"$gt">>, 3}]},
                        {[{<<"$lt">>, 7}]}
                    ]}
                ]}}
        ]}
    ),
    ?assertEqual(true, match_int(SelMulti, {[{<<"x">>, 6}]})),
    ?assertEqual(false, match_int(SelMulti, {[{<<"x">>, 2}]})),
    ?assertEqual(false, match_int(SelMulti, {[{<<"x">>, 9}]})),
    ?assertEqual(false, match_int(SelMulti, {[]})),

    % $not -> $and with multiple selectors matches if any selector does not match
    SelNotMulti = normalize(
        {[
            {<<"x">>,
                {[
                    {<<"$not">>,
                        {[
                            {<<"$and">>, [
                                {[{<<"$gt">>, 3}]},
                                {[{<<"$lt">>, 7}]}
                            ]}
                        ]}}
                ]}}
        ]}
    ),
    ?assertEqual(false, match_int(SelNotMulti, {[{<<"x">>, 6}]})),
    ?assertEqual(true, match_int(SelNotMulti, {[{<<"x">>, 2}]})),
    ?assertEqual(true, match_int(SelNotMulti, {[{<<"x">>, 9}]})),
    ?assertEqual(false, match_int(SelNotMulti, {[]})).

match_or_test() ->
    % $or with an empty array matches anything
    SelEmpty = normalize({[{<<"x">>, {[{<<"$or">>, []}]}}]}),
    ?assertEqual(true, match_int(SelEmpty, {[{<<"x">>, 0}]})),
    ?assertEqual(true, match_int(SelEmpty, {[{<<"x">>, false}]})),
    ?assertEqual(true, match_int(SelEmpty, {[{<<"x">>, []}]})),
    ?assertEqual(true, match_int(SelEmpty, {[]})),

    % similar to $and, due to { "$or": [] } matching anything and our
    % normalization rules, negating $or also matches anything
    SelNotEmpty = normalize({[{<<"x">>, {[{<<"$not">>, {[{<<"$or">>, []}]}}]}}]}),
    ?assertEqual(true, match_int(SelNotEmpty, {[{<<"x">>, 0}]})),
    ?assertEqual(true, match_int(SelNotEmpty, {[{<<"x">>, false}]})),
    ?assertEqual(true, match_int(SelNotEmpty, {[{<<"x">>, []}]})),
    ?assertEqual(true, match_int(SelNotEmpty, {[]})),

    % $or with multiple selectors matches if any selector matches
    SelMulti = normalize(
        {[
            {<<"x">>,
                {[
                    {<<"$or">>, [
                        {[{<<"$lt">>, 3}]},
                        {[{<<"$gt">>, 7}]}
                    ]}
                ]}}
        ]}
    ),
    ?assertEqual(false, match_int(SelMulti, {[{<<"x">>, 6}]})),
    ?assertEqual(true, match_int(SelMulti, {[{<<"x">>, 2}]})),
    ?assertEqual(true, match_int(SelMulti, {[{<<"x">>, 9}]})),
    ?assertEqual(false, match_int(SelMulti, {[]})),

    % $not -> $or with multiple selectors matches if no selector matches
    SelNotMulti = normalize(
        {[
            {<<"x">>,
                {[
                    {<<"$not">>,
                        {[
                            {<<"$or">>, [
                                {[{<<"$lt">>, 3}]},
                                {[{<<"$gt">>, 7}]}
                            ]}
                        ]}}
                ]}}
        ]}
    ),
    ?assertEqual(true, match_int(SelNotMulti, {[{<<"x">>, 6}]})),
    ?assertEqual(false, match_int(SelNotMulti, {[{<<"x">>, 2}]})),
    ?assertEqual(false, match_int(SelNotMulti, {[{<<"x">>, 9}]})),
    ?assertEqual(false, match_int(SelNotMulti, {[]})).

match_nor_test() ->
    % $nor with an empty array matches anything
    SelEmpty = normalize({[{<<"x">>, {[{<<"$nor">>, []}]}}]}),
    ?assertEqual(true, match_int(SelEmpty, {[{<<"x">>, 0}]})),
    ?assertEqual(true, match_int(SelEmpty, {[{<<"x">>, false}]})),
    ?assertEqual(true, match_int(SelEmpty, {[{<<"x">>, []}]})),
    ?assertEqual(true, match_int(SelEmpty, {[]})),

    % $nor with multiple selectors matches if no selector matches
    SelMulti = normalize(
        {[
            {<<"x">>,
                {[
                    {<<"$nor">>, [
                        {[{<<"$lt">>, 3}]},
                        {[{<<"$gt">>, 7}]}
                    ]}
                ]}}
        ]}
    ),
    ?assertEqual(true, match_int(SelMulti, {[{<<"x">>, 6}]})),
    ?assertEqual(false, match_int(SelMulti, {[{<<"x">>, 2}]})),
    ?assertEqual(false, match_int(SelMulti, {[{<<"x">>, 9}]})),
    ?assertEqual(false, match_int(SelMulti, {[]})).

match_failures_object_test() ->
    Selector = normalize(
        {[
            {<<"a">>, 1},
            {<<"b">>, {[{<<"c">>, 3}]}}
        ]}
    ),

    Fails0 = match_failures(
        Selector,
        {[
            {<<"a">>, 1},
            {<<"b">>, {[{<<"c">>, 3}]}}
        ]}
    ),
    ?assertEqual([], Fails0),

    Fails1 = match_failures(
        Selector,
        {[
            {<<"a">>, 0},
            {<<"b">>, {[{<<"c">>, 3}]}}
        ]}
    ),
    ?assertEqual(
        [#failure{op = eq, type = mismatch, params = [1], path = [<<"a">>]}],
        Fails1
    ),

    Fails2 = match_failures(
        Selector,
        {[
            {<<"a">>, 1},
            {<<"b">>, {[{<<"c">>, 4}]}}
        ]}
    ),
    ?assertEqual(
        [#failure{op = eq, type = mismatch, params = [3], path = [<<"b.c">>]}],
        Fails2
    ).

match_failures_elemmatch_test() ->
    SelElemMatch = normalize(
        {[
            {<<"a">>,
                {[
                    {<<"$elemMatch">>, {[{<<"$gt">>, 4}]}}
                ]}}
        ]}
    ),

    Fails0 = match_failures(
        SelElemMatch, {[{<<"a">>, [5, 3, 2]}]}
    ),
    ?assertEqual([], Fails0),

    Fails1 = match_failures(
        SelElemMatch, {[{<<"a">>, []}]}
    ),
    ?assertEqual(
        [#failure{op = elemMatch, type = empty_list, params = [], path = [<<"a">>]}],
        Fails1
    ),

    Fails2 = match_failures(
        SelElemMatch, {[{<<"a">>, [3, 2]}]}
    ),
    ?assertEqual(
        [
            #failure{op = gt, type = mismatch, params = [4], path = [0, <<"a">>]},
            #failure{op = gt, type = mismatch, params = [4], path = [1, <<"a">>]}
        ],
        Fails2
    ).

match_failures_allmatch_test() ->
    SelAllMatch = normalize(
        {[
            {<<"a">>,
                {[
                    {<<"$allMatch">>, {[{<<"$gt">>, 4}]}}
                ]}}
        ]}
    ),

    Fails0 = match_failures(
        SelAllMatch, {[{<<"a">>, [5]}]}
    ),
    ?assertEqual([], Fails0),

    Fails1 = match_failures(
        SelAllMatch, {[{<<"a">>, [4]}]}
    ),
    ?assertEqual(
        [#failure{op = gt, type = mismatch, params = [4], path = [0, <<"a">>]}],
        Fails1
    ),

    Fails2 = match_failures(
        SelAllMatch, {[{<<"a">>, [5, 6, 3, 7, 0]}]}
    ),
    ?assertEqual(
        [
            #failure{op = gt, type = mismatch, params = [4], path = [2, <<"a">>]},
            #failure{op = gt, type = mismatch, params = [4], path = [4, <<"a">>]}
        ],
        Fails2
    ).

match_failures_allmatch_object_test() ->
    SelAllMatch = normalize(
        {[
            {<<"a.b">>,
                {[
                    {<<"$allMatch">>, {[{<<"c">>, {[{<<"$gt">>, 4}]}}]}}
                ]}}
        ]}
    ),

    Fails0 = match_failures(
        SelAllMatch, {[{<<"a">>, {[{<<"b">>, [{[{<<"c">>, 5}]}]}]}}]}
    ),
    ?assertEqual([], Fails0),

    Fails1 = match_failures(
        SelAllMatch, {[{<<"a">>, {[{<<"b">>, [{[{<<"c">>, 4}]}]}]}}]}
    ),
    ?assertEqual(
        [#failure{op = gt, type = mismatch, params = [4], path = [<<"c">>, 0, <<"a.b">>]}],
        Fails1
    ),

    Fails2 = match_failures(
        SelAllMatch,
        {[{<<"a">>, {[{<<"b">>, [{[{<<"c">>, 5}]}, {[{<<"c">>, 6}]}, {[{<<"c">>, 3}]}]}]}}]}
    ),
    ?assertEqual(
        [#failure{op = gt, type = mismatch, params = [4], path = [<<"c">>, 2, <<"a.b">>]}],
        Fails2
    ),

    Fails3 = match_failures(
        SelAllMatch,
        {[{<<"a">>, {[{<<"b">>, [{[{<<"c">>, 1}]}, {[]}]}]}}]}
    ),
    ?assertEqual(
        [
            #failure{op = gt, type = mismatch, params = [4], path = [<<"c">>, 0, <<"a.b">>]},
            #failure{op = field, type = not_found, params = [], path = [<<"c">>, 1, <<"a.b">>]}
        ],
        Fails3
    ).

-endif.
