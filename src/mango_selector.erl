-module(mango_selector).


-export([
    normalize/1,
    index_fields/1,
    range/2,
    match/2
]).


-include_lib("couch/include/couch_db.hrl").
-include("mango.hrl").


% Validate and normalize each operator. This translates
% every selector operator into a consistent version that
% we can then rely on for all other selector functions.
% See the definition of each step below for more information
% on what each one does.
normalize(Selector) ->
    Steps = [
        fun norm_ops/1,
        fun norm_fields/1,
        fun norm_negations/1
    ],
    lists:foldl(fun(Step, Sel) -> Step(Sel) end, Selector, Steps).


% This function returns a list of indexes that
% can be used to restrict this query. This works by
% searching the selector looking for field names that
% can be "seen".
%
% Operators that can be seen through are '$and' and any of
% the logical comparisons ('$lt', '$eq', etc). Things like
% '$regexp', '$in', '$nin', and '$or' can't be serviced by
% a single index scan so we disallow them. In the future
% we may become more clever and increase our ken such that
% we will be able to see through these with crafty indexes
% or new uses for existing indexes. For instance, I could
% see an '$or' between comparisons on the same field becoming
% the equivalent of a multi-query. But that's for another
% day.

% We can see through '$and' trivially
index_fields({[{<<"$and">>, Args}]}) ->
    lists:usort(lists:flatten([index_fields(A) || A <- Args]));

% So far we can't see through any other operator
index_fields({[{<<"$", _/binary>>, _}]}) ->
    [];

% If we have a field with a terminator that is locatable
% using an index then the field is a possible index
index_fields({[{Field, Cond}]}) ->
    case indexable(Cond) of
        true ->
            [Field];
        false ->
            []
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


% Match a selector against a #doc{} or EJSON value.
% This assumes that the Selector has been normalized.
% Returns true or false.

% An empty selector matches any value.
match({[]}, _) ->
    true;

match(Selector, #doc{body=Body}) ->
    match(Selector, Body, fun mango_json:cmp/2);

match(Selector, {Props}) ->
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

norm_ops({[{<<"$regex">>, R}, {<<"$options">>, O}]} = Cond)
        when is_binary(R), is_binary(O) ->
    % TODO: write translate_options(O),
    Opts = [{capture, none}],
    case re:compile(R, Opts) of
        {ok, _} ->
            Cond;
        _ ->
            ?MANGO_ERROR({bad_arg, '$regex', {R, O}})
    end;
norm_ops({[{<<"$options">>, O}, {<<"$regex">>, R}]}) ->
    norm_ops({[{<<"$regex">>, R}, {<<"$options">>, O}]});
norm_ops({[{<<"$regex">>, R}]}) ->
    % Add this clause out of an overabundance of caution.
    norm_ops({[{<<"$regex">>, R}, {<<"$options">>, <<>>}]});

norm_ops({[{<<"$all">>, Args}]}) when is_list(Args) ->
    {[{<<"$all">>, [norm_ops(A) || A <- Args]}]};
norm_ops({[{<<"$all">>, Arg}]}) ->
    ?MANGO_ERROR({bad_arg, '$all', Arg});

norm_ops({[{<<"$elemMatch">>, {_}=Arg}]}) ->
    {[{<<"$elemMatch">>, norm_ops(Arg)}]};
norm_ops({[{<<"$elemMatch">>, Arg}]}) ->
    ?MANGO_ERROR({bad_arg, '$elemMatch', Arg});

norm_ops({[{<<"$size">>, Arg}]}) when is_integer(Arg), Arg >= 0 ->
    {[{<<"$size">>, Arg}]};
norm_ops({[{<<"$size">>, Arg}]}) ->
    ?MANGO_ERROR({bad_arg, '$size', Arg});

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
norm_ops({Props}) when length(Props) > 1 ->
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
% do the same through the '$all' and '$elemMatch'
% operators but we can apply the same algorithm to the
% arguments of those operators.
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
norm_fields({[{<<"$all">>, Args}]}, _Path) ->
    {[{<<"$all">>, [norm_fields(A) || A <- Args]}]};

norm_fields({[{<<"$elemMatch">>, Arg}]}, _Path) ->
    {[{<<"$elemMatch">>, norm_fields(Arg)}]};

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

norm_negations({[{<<"$all">>, Args}]}) ->
    {[{<<"$all">>, [norm_negations(A) || A <- Args]}]};

norm_negations({[{<<"$elemMatch">>, Arg}]}) ->
    {[{<<"$elemMatch">>, norm_negations(Arg)}]};

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

% All other operators are currently not indexable.
% This is also a subtle assertion that we don't
% call indexable/1 on a field name.
indexable({[{<<"$", _/binary>>, _}]}) ->
    false.


% Adjust Low and High based on values found for the
% givend Index in Selector.
range({[{<<"$and">>, Args}]}, Index, LCmp, Low, HCmp, High) ->
    lists:foldl(fun
        (Arg, {LC, L, HC, H}) ->
            range(Arg, Index, LC, L, HC, H);
        (_Arg, empty) ->
            empty
    end, {LCmp, Low, HCmp, High}, Args);

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
% narrowed our range. HCmp is set the the '$lt' operator that was
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


match({[{<<"$and">>, Args}]}, Value, Cmp) ->
    Pred = fun(SubSel) -> match(SubSel, Value, Cmp) end,
    lists:all(Pred, Args);

match({[{<<"$or">>, Args}]}, Value, Cmp) ->
    Pred = fun(SubSel) -> match(SubSel, Value, Cmp) end,
    lists:any(Pred, Args);

match({[{<<"$not">>, Arg}]}, Value, Cmp) ->
    not match(Arg, Value, Cmp);

% One of the elements in the array Value must
% match all conditions in Args.
match({[{<<"$all">>, Args}]}, Values, Cmp) when is_list(Values) ->
    Pred = fun
        (Val, false) ->
            SelPred = fun(A) -> match(A, Val, Cmp) end,
            lists:all(SelPred, Args);
        (_, true) ->
            % Short circuit
            true
    end,
    lists:foldl(Pred, false, Values);

% MongoDB docs also say we can use '$all' against
% a non-array value. We just cheat by wrapping the
% value in a list and recursing to use the logic
% in the previous clause.
match({[{<<"$all">>, _}]} = Cond, Value, Cmp) ->
    match(Cond, [Value], Cmp);

% The '$elemMatch' seems like a silly operator. I'm
% guessing it exists to mask over some previous behavior
% that I haven't seen yet. I'm guessing that this is
% probably wrong and that '$all' wraps objects with
% the '$eq' operator. Hopefully any behavior disparity
% will be caught during testing.
match({[{<<"$elemMatch">>, Arg}]}, Value, Cmp) ->
    match(Arg, Value, Cmp);

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

match({[{<<"$in">>, Args}]}, Value, Cmp) ->
    Pred = fun(Arg) -> Cmp(Value, Arg) == 0 end,
    lists:any(Pred, Args);

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

match({[{<<"$regex">>, Regex}, {<<"$options">>, _O}]}, Value, _Cmp)
        when is_binary(Value) ->
    % TODO: write translate_options(O),
    Opts = [{capture, none}],
    try
        match == re:run(Value, Regex, Opts)
    catch _:_ ->
        false
    end;
match({[{<<"$regex">>, _}, {<<"$options">>, _}]}, _Value, _Cmp) ->
    false;

match({[{<<"$size">>, Arg}]}, Values, _Cmp) when is_list(Values) ->
    length(Values) == Arg;
match({[{<<"$size">>, _}]}, _Value, _Cmp) ->
    false;

% All other operators are internal assertion errors for
% matching because we either should've removed them during
% normalization or something else broke.
match({[{<<"$", _/binary>>=Op, _}]}, _, _) ->
    erlang:error({bad_operator, Op});

% We need to traverse value to find field. The call to
% mango_doc:get_field/2 may return either not_found or
% bad_path in which case matching fails.
match({[{Field, Cond}]}, Value, Cmp) ->
    case mango_doc:get_field(Value, Field) of
        not_found ->
            false;
        bad_path ->
            false;
        SubValue when Field == <<"_id">> ->
            match(Cond, SubValue, fun mango_json:cmp_raw/2);
        SubValue ->
            match(Cond, SubValue, Cmp)
    end;

match({Props} = Sel, _Value, _Cmp) when length(Props) > 1 ->
    erlang:error({unnormalized_selector, Sel}).