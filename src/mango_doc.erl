-module(mango_doc).


-export([
    open/3,
    save/3,

    from_bson/1,

    matches/2,
    apply_update/2,
    has_operators/1,

    get_field/2,
    get_field/3,
    rem_field/2,
    set_field/3
]).


-include_lib("couch/include/couch_db.hrl").


open(DbName, DocId, Ctx) ->
    Opts = [deleted, {user_ctx, mango_ctx:get_auth(Ctx)}],
    try mango_util:defer(fabric, open_doc, [DbName, DocId, Opts]) of
        {ok, Doc} ->
            {ok, Doc};
        {not_found, _} ->
            not_found;
        {error, Reason} ->
            throw(Reason);
        Error ->
            throw(Error)
    catch error:database_does_not_exist ->
        not_found
    end.


save(DbName, #doc{}=Doc, Ctx) ->
    save(DbName, [Doc], Ctx);
save(DbName, Docs, Ctx) when is_list(Docs) ->
    Opts = [{user_ctx, mango_ctx:get_auth(Ctx)}],
    mango_util:defer(fabric, update_docs, [DbName, Docs, Opts]).


from_bson({Props}) ->
    DocProps = case lists:keytake(<<"_id">>, 1, Props) of
        {value, {<<"_id">>, DocId0}, RestProps} ->
            DocId = case DocId0 of
                {[{<<"$id">>, Id}]} ->
                    Id;
                Else ->
                    Else
            end,
            [{<<"_id">>, DocId} | RestProps];
        false ->
            Props
    end,
    Doc = couch_doc:from_json_obj({DocProps}),
    case Doc#doc.id of
        <<"">> ->
            Doc#doc{id=couch_uuids:new(), revs={0, []}};
        _ ->
            Doc
    end.


matches(#doc{body=Body}, FieldConditions) ->
    matches(Body, FieldConditions);
matches(_, {[]}) ->
    true;
matches(Value, {[{Field, Cond} | Rest]}) ->
    case do_compare(Value, Field, Cond) of
        true ->
            matches(Value, {Rest});
        false ->
            false
    end;
matches(_, Cond) ->
    throw({invalid_conditional, Cond}).


apply_update(#doc{body={Props}}=Doc, {Update}) ->
    Result = do_update(Props, Update),
    case has_operators(Result) of
        true ->
            throw(update_leaves_operators);
        false ->
            ok
    end,
    Doc#doc{body={Result}}.


has_operators(#doc{body=Body}) ->
    has_operators(Body);
has_operators({Props}) when is_list(Props) ->
    has_operators_obj(Props);
has_operators(Arr) when is_list(Arr) ->
    has_operators_arr(Arr);
has_operators(Val) when is_atom(Val) ->
    false;
has_operators(Val) when is_number(Val) ->
    false;
has_operators(Val) when is_binary(Val) ->
    false.


has_operators_obj([]) ->
    false;
has_operators_obj([{K, V} | Rest]) ->
    case K of
        <<"$", _/binary>> ->
            true;
        _ ->
            case has_operators(V) of
                true ->
                    true;
                false ->
                    has_operators_obj(Rest)
            end
    end.


has_operators_arr([]) ->
    false;
has_operators_arr([V | Rest]) ->
    case has_operators(V) of
        true ->
            true;
        false ->
            has_operators_arr(Rest)
    end.


do_compare(Value, <<"$", _/binary>> = Op, Arg) ->
    CompareFuns = [
        % Comparison operators
        {<<"$gt">>, fun do_compare_gt/2},
        {<<"$gte">>, fun do_compare_gte/2},
        {<<"$in">>, fun do_compare_in/2},
        {<<"$lt">>, fun do_compare_lt/2},
        {<<"$lte">>, fun do_compare_lte/2},
        {<<"$ne">>, fun do_compare_ne/2},
        {<<"$nin">>, fun do_compare_nin/2},

        % Logical operators
        {<<"$or">>, fun do_compare_or/2},
        {<<"$and">>, fun do_compare_and/2},
        {<<"$not">>, fun do_compare_not/2},
        {<<"$nor">>, fun do_compare_nor/2},

        % Element operators
        {<<"$exists">>, fun do_compare_exists/2},
        {<<"$type">>, fun do_compare_type/2},

        % Evaluation operators
        {<<"$mod">>, fun do_compare_mod/2},
        {<<"$regex">>, fun do_compare_regex/2},
        %{<<"$where">>, fun do_compare_where/2},

        % Geospatial operators
        %{<<"$geoWithin">>, fun do_compare_geo_within/2},
        %{<<"$geoIntersects">>, fun do_compare_geo_intersects/2},
        %{<<"$near">>, fun do_compare_near/2},
        %{<<"$nearSphere">>, fun do_compare_near_sphere/2},

        % Array operators
        {<<"$all">>, fun do_compare_all/2},
        {<<"$elemMatch">>, fun do_compare_elem_match/2},
        {<<"$size">>, fun do_compare_size/2}
    ],
    case lists:keyfind(Op, 1, CompareFuns) of
        {Op, Fun} ->
            Fun(Value, Arg);
        false ->
            throw({comparison_operator_not_supported, Op})
    end;
do_compare(Value, Key, Arg) ->
    SubValue = case get_field(Key, Value) of
        not_found ->
            undefined;
        bad_path ->
            % This doesn't seem right...
            undefined;
        Value ->
            Value
    end,
    case Arg of
        {_} ->
            % If the argument is an object we
            % have to recurse to apply all of our
            % logic rules.
            matches(SubValue, Arg);
        Value ->
            % If its not an object we can just do
            % a straight up equality comparison.
            cmp_json(SubValue, Arg) == 0
    end.


do_compare_gt(Value, Arg) ->
    cmp_json(Value, Arg) == 1.


do_compare_gte(Value, Arg) ->
    cmp_json(Value, Arg) >= 0.


do_compare_in(Value, Arg) when is_list(Arg) ->
    lists:member(Value, Arg);
do_compare_in(_, _) ->
    false.


do_compare_lt(Value, Arg) ->
    cmp_json(Value, Arg) < 0.


do_compare_lte(Value, Arg) ->
    cmp_json(Value, Arg) =< 0.


do_compare_ne(Value, Arg) ->
    cmp_json(Value, Arg) /= 0.


do_compare_nin(Value, Arg) when is_list(Arg) ->
    not lists:member(Value, Arg);
do_compare_nin(_, _) ->
    false.


do_compare_or(_, []) ->
    false;
do_compare_or(Value, [Cond | Rest]) ->
    case matches(Value, Cond) of
        true ->
            true;
        false ->
            do_compare_or(Value, Rest)
    end;
do_compare_or(_, _) ->
    false.


do_compare_and(_, []) ->
    true;
do_compare_and(Value, [Cond | Rest]) ->
    case matches(Value, Cond) of
        true ->
            do_compare_and(Value, Rest);
        false ->
            false
    end;
do_compare_and(_, _) ->
    false.


do_compare_not(Value, Cond) ->
    not matches(Value, Cond).


do_compare_nor(_, []) ->
    true;
do_compare_nor(Value, [Cond | Rest]) ->
    case matches(Value, Cond) of
        true ->
            false;
        false ->
            do_compare_nor(Value, Rest)
    end;
do_compare_nor(_, _) ->
    false.



do_compare_exists(Value, ShouldExist) when is_boolean(ShouldExist) ->
    Exists = Value /= undefined,
    Exists == ShouldExist;
do_compare_exists(_, _) ->
    false.



do_compare_type(_Value, _Arg) ->
    % Fill this in later. Arg will be an integer
    % referencing the BSON type so we'll need to
    % write a mango_bson:type/1 function that takes
    % EJSON and returns the integer.
    throw(not_supported_yet_but_maybe_soon).


do_compare_mod(Value, [Div, Rem])
        when is_integer(Value), is_integer(Div), is_integer(Rem) ->
    Value rem Div == Rem;
do_compare_mod(_, _) ->
    % Should this throw?
    false.


do_compare_regex(Value, {[{<<"$regex">>, R}, {<<"$options">>, O}]})
        when is_binary(R), is_binary(O) ->
    Opts = [{capture, none}], % TODO: write translate_options(O),
    case is_binary(Value) of
        true ->
            try
                match == re:run(Value, R, Opts)
            catch _:_ ->
                % Shoudl we raise here? Or not even bother
                % catching this in the first place?
                false
            end;
        false ->
            false
    end;
do_compare_regex(Value, {[{<<"$options">>, O}, {<<"$regex">>, R}]}) ->
    do_compare_regex(Value, {[{<<"$regex">>, R}, {<<"$options">>, O}]});
do_compare_regex(Value, {[{<<"$regex">>, R}]}) ->
    % Add this clause out of an overabundance of caution.
    do_compare_regex(Value, {[{<<"$regex">>, R}, {<<"$options">>, <<>>}]});
do_compare_regex(_, _) ->
    % Shoudl this throw?
    false.


do_compare_all([], _) ->
    true;
do_compare_all([Value | Rest], Arg) ->
    case matches(Value, Arg) of
        true ->
            do_compare_all(Rest, Arg);
        false ->
            false
    end;
do_compare_all(_, _) ->
    false.


do_compare_elem_match([], _) ->
    false;
do_compare_elem_match([Value | Rest], Arg) ->
    case matches(Value, Arg) of
        true ->
            true;
        false ->
            do_compare_elem_match(Rest, Arg)
    end;
do_compare_elem_match(_, _) ->
    false.


do_compare_size(Values, Arg) when is_list(Values), is_integer(Arg) ->
    length(Values) == Arg;
do_compare_size(_, _) ->
    false.


do_update(Props, [{Op, Value} | Rest]) ->
    UpdateFun = update_operator_fun(Op),
    NewProps = case UpdateFun of
        undefined ->
            lists:keystore(Op, 1, Props, {Op, Value});
        Fun when is_function(Fun, 2) ->
            case Value of
                {ValueProps} ->
                    Fun(Props, ValueProps);
                _ ->
                    throw({invalid_operand, Op, Value})
            end
    end,
    do_update(NewProps, Rest).


update_operator_fun(<<"$", _/binary>> = Op) ->
    OperatorFuns = [
        % Object operators
        {<<"$inc">>, fun do_update_inc/2},
        {<<"$rename">>, fun do_update_rename/2},
        %{<<"$setOnInsert">>, fun do_update_set_on_insert/2},
        {<<"$set">>, fun do_update_set/2},
        {<<"$unsert">>, fun do_update_unset/2},

        % Array opereators
        {<<"$addToSet">>, fun do_update_add_to_set/2},
        {<<"$pop">>, fun do_update_pop/2},
        {<<"$pullAll">>, fun do_update_pull_all/2},
        {<<"$pull">>, fun do_update_pull/2},
        {<<"$pushAll">>, fun do_update_push_all/2},
        {<<"$push">>, fun do_update_push/2},

        % Bitwise Operators
        {<<"$bit">>, fun do_update_bitwise/2}
    ],
    case lists:keyfind(Op, 1, OperatorFuns) of
        {Op, Fun} ->
            Fun;
        false ->
            throw({update_operator_not_supported, Op})
    end;
update_operator_fun(_) ->
    undefined.


do_update_inc(Props, []) ->
    Props;
do_update_inc(Props, [{Field, Incr} | Rest]) ->
    if is_number(Incr) -> ok; true ->
        throw({invalid_increment, Incr})
    end,
    NewProps = case get_field(Props, Field, fun is_number/1) of
        Value when is_number(Value) ->
            set_field(Props, Field, Value + Incr);
        not_found ->
            set_field(Props, Field, Incr);
        _ ->
            Props
    end,
    do_update_inc(NewProps, Rest).


do_update_rename(Props, []) ->
    Props;
do_update_rename(Props, [{OldField, NewField} | Rest]) ->
    NewProps = case rem_field(Props, OldField) of
        {RemProps, OldValue} ->
            set_field(RemProps, NewField, OldValue);
        _ ->
            Props
    end,
    do_update_rename(NewProps, Rest).


do_update_set(Props, []) ->
    Props;
do_update_set(Props, [{Field, Value} | Rest]) ->
    NewProps = set_field(Props, Field, Value),
    do_update_set(NewProps, Rest).


do_update_unset(Props, []) ->
    Props;
do_update_unset(Props, [{Field, _} | Rest]) ->
    NewProps = case rem_field(Props, Field) of
        {RemProps, _} ->
            RemProps;
        _ ->
            Props
    end,
    do_update_unset(NewProps, Rest).


do_update_add_to_set(Props, []) ->
    Props;
do_update_add_to_set(Props, [{Field, NewValue} | Rest]) ->
    ToAdd = case NewValue of
        {[{<<"$each">>, NewValues}]} when is_list(NewValues) ->
            NewValues;
        {[{<<"$each">>, NewValue}]} ->
            [NewValue];
        Else ->
            [Else]
    end,
    NewProps = case get_field(Props, Field) of
        OldValues when is_list(OldValues) ->
            FinalValues = lists:foldl(fun(V, Acc) ->
                lists:append(Acc, [V])
            end, OldValues, ToAdd),
            set_field(Props, Field, FinalValues);
        _ ->
            Props
    end,
    do_update_add_to_set(NewProps, Rest).


do_update_pop(Props, []) ->
    Props;
do_update_pop(Props, [{Field, Pos} | Rest]) ->
    NewProps = case get_field(Props, Field) of
        OldValues when is_list(OldValues) ->
            NewValues = case Pos > 0 of
                true ->
                    lists:sublist(OldValues, 1, length(OldValues) - 1);
                false ->
                    lists:sublist(OldValues, 2, length(OldValues) - 1)
            end,
            set_field(Props, Field, NewValues);
        _ ->
            Props
    end,
    do_update_pop(NewProps, Rest).


do_update_pull_all(Props, []) ->
    Props;
do_update_pull_all(Props, [{Field, Values} | Rest]) ->
    ToRem = case is_list(Values) of
        true -> Values;
        false -> [Values]
    end,
    NewProps = case get_field(Props, Field) of
        OldValues when is_list(OldValues) ->
            NewValues = lists:foldl(fun(ValToRem, Acc) ->
                % The logic in these filter functions is a bit
                % subtle. The way to think of this is that we
                % return true for all elements we want to keep.
                FilterFun = case has_operators(ValToRem) of
                    true ->
                        fun(A) -> not matches(A, ValToRem) end;
                    false ->
                        fun(A) -> A /= ValToRem end
                end,
                lists:filter(FilterFun, Acc)
            end, OldValues, ToRem),
            set_field(Props, Field, NewValues);
        _ ->
            Props
    end,
    do_update_add_to_set(NewProps, Rest).


do_update_pull(Props, []) ->
    Props;
do_update_pull(Props, [{Field, Value} | Rest]) ->
    ToRem = case Value of
        {[{<<"$each">>, Values}]} when is_list(Values) ->
            Values;
        {[{<<"$each">>, Value}]} ->
            [Value];
        Else ->
            [Else]
    end,
    NewProps = do_update_pull_all(Props, [{Field, ToRem}]),
    do_update_pull(NewProps, Rest).


do_update_push_all(_, []) ->
    [];
do_update_push_all(Props, [{Field, Values} | Rest]) ->
    ToAdd = case is_list(Values) of
        true -> Values;
        false -> [Values]
    end,
    NewProps = case get_field(Props, Field) of
        OldValues when is_list(OldValues) ->
            NewValues = OldValues ++ ToAdd,
            set_field(Props, Field, NewValues);
        _ ->
            Props
    end,
    do_update_push_all(NewProps, Rest).


do_update_push(Props, []) ->
    Props;
do_update_push(Props, [{Field, Value} | Rest]) ->
    ToAdd = case Value of
        {[{<<"$each">>, Values}]} when is_list(Values) ->
            Values;
        {[{<<"$each">>, Value}]} ->
            [Value];
        Else ->
            [Else]
    end,
    NewProps = do_update_push_all(Props, [{Field, ToAdd}]),
    do_update_push(NewProps, Rest).



do_update_bitwise(Props, []) ->
    Props;
do_update_bitwise(Props, [{Field, Value} | Rest]) ->
    DoOp = case Value of
        {[{<<"and">>, Val}]} when is_integer(Val) ->
            fun(V) -> V band Val end;
        {[{<<"or">>, Val}]} when is_integer(Val) ->
            fun(V) -> V bor Val end;
        _ ->
            fun(V) -> V end
    end,
    NewProps = case get_field(Props, Field, fun is_number/1) of
        Value when is_number(Value) ->
            NewValue = DoOp(Value),
            set_field(Props, Field, NewValue);
        _ ->
            Props
    end,
    do_update_bitwise(NewProps, Rest).


get_field(Props, Field) ->
    get_field(Props, Field, no_validation).


get_field(Props, Field, Validator) when is_binary(Field) ->
    Path = re:split(Field, <<"\\.">>),
    lists:foreach(fun(P) ->
        if P /= <<>> -> ok; true ->
            throw({invalid_field_name, Field})
        end
    end, Path),
    get_field(Props, Path, Validator);
get_field(Props, [], no_validation) ->
    Props;
get_field(Props, [], Validator) ->
    case (catch Validator(Props)) of
        true ->
            Props;
        _ ->
            invalid_value
    end;
get_field({Props}, [Name | Rest], Validator) ->
    case lists:keyfind(Name, 1, Props) of
        {Name, Value} ->
            get_field(Value, Rest, Validator);
        false ->
            not_found
    end;
get_field(Values, [Name | Rest], Validator) when is_list(Values) ->
    % Name might be an integer index into an array
    try
        Pos = list_to_integer(binary_to_list(Name)),
        case Pos >= 0 andalso Pos < length(Values) of
            true ->
                % +1 because Erlang uses 1 based list indices
                Value = lists:nth(Pos + 1, Values),
                get_field(Value, Rest, Validator);
            false ->
                bad_path
        end
    catch error:badarg ->
        bad_path
    end;
get_field(_, [_|_], _) ->
    bad_path.


rem_field(Props, Field) when is_binary(Field) ->
    Path = re:split(Field, <<"\\.">>),
    lists:foreach(fun(P) ->
        if P /= <<>> -> ok; true ->
            throw({invalid_field_name, Field})
        end
    end, Path),
    rem_field(Props, Path);
rem_field({Props}, [Name]) ->
    case lists:keytake(Name, 1, Props) of
        {value, Value, NewProps} ->
            {NewProps, Value};
        false ->
            not_found
    end;
rem_field({Props}, [Name | Rest]) ->
    case lists:keyfind(Name, 1, Props) of
        {Name, Value} ->
            case rem_field(Value, Rest) of
                {NewValue, Ret} ->
                    NewObj = {lists:keystore(Name, 1, Props, {Name, NewValue})},
                    {NewObj, Ret};
                Else ->
                    Else
            end;
        false ->
            not_found
    end;
rem_field(Values, [Name]) when is_list(Values) ->
    % Name might be an integer index into an array
    try
        Pos = list_to_integer(binary_to_list(Name)),
        case Pos >= 0 andalso Pos < length(Values) of
            true ->
                % +1 because Erlang uses 1 based list indices
                rem_elem(Pos + 1, Values);
            false ->
                bad_path
        end
    catch error:badarg ->
        bad_path
    end;
rem_field(Values, [Name | Rest]) when is_list(Values) ->
    % Name might be an integer index into an array
    try
        Pos = list_to_integer(binary_to_list(Name)),
        case Pos >= 0 andalso Pos < length(Values) of
            true ->
                % +1 because Erlang uses 1 based list indices
                Value = lists:nth(Pos + 1, Values),
                case rem_field(Value, Rest) of
                    {NewValue, Ret} ->
                        {set_elem(Pos + 1, Values, NewValue), Ret};
                    Else ->
                        Else
                end;
            false ->
                bad_path
        end
    catch error:badarg ->
        bad_path
    end;
rem_field(_, [_|_]) ->
    bad_path.


set_field(Props, Field, Value) when is_binary(Field) ->
    Path = re:split(Field, <<"\\.">>),
    lists:foreach(fun(P) ->
        if P /= <<>> -> ok; true ->
            throw({invalid_field_name, Field})
        end
    end, Path),
    set_field(Props, Path, Value);
set_field({Props}, [Name], Value) ->
    {lists:keystore(Name, 1, Props, {Name, Value})};
set_field({Props}, [Name | Rest], Value) ->
    case lists:keyfind(Name, 1, Props) of
        {Name, Elem} ->
            Result = set_field(Elem, Rest, Value),
            {lists:keystore(Name, 1, Props, {Name, Result})};
        false ->
            Nested = make_nested(Rest, Value),
            {lists:keystore(Name, 1, Props, Nested)}
    end;
set_field(Values, [Name], Value) when is_list(Values) ->
    % Name might be an integer index into an array
    try
        Pos = list_to_integer(binary_to_list(Name)),
        case Pos >= 0 andalso Pos < length(Values) of
            true ->
                % +1 because Erlang uses 1 based list indices
                set_elem(Pos, Values, Value);
            false ->
                Values
        end
    catch error:badarg ->
        Values
    end;
set_field(Values, [Name | Rest], Value) when is_list(Values) ->
    % Name might be an integer index into an array
    try
        Pos = list_to_integer(binary_to_list(Name)),
        case Pos >= 0 andalso Pos < length(Values) of
            true ->
                % +1 because Erlang uses 1 based list indices
                Elem = lists:nth(Pos + 1, Values),
                Result = set_field(Elem, Rest, Value),
                set_elem(Pos, Values, Result);
            false ->
                Values
        end
    catch error:badarg ->
        Values
    end;
set_field(Value, [_|_], _) ->
    Value.


make_nested([], Value) ->
    Value;
make_nested([Name | Rest], Value) ->
    {[{Name, make_nested(Rest, Value)}]}.


rem_elem(1, [Value | Rest]) ->
    {Rest, Value};
rem_elem(I, [Item | Rest]) when I > 1 ->
    {Tail, Value} = rem_elem(I+1, Rest),
    {[Item | Tail], Value}.


set_elem(1, [_ | Rest], Value) ->
    [Value | Rest];
set_elem(I, [Item | Rest], Value) when I > 1 ->
    [Item | set_elem(I-1, Rest, Value)].


% Make sure anything compared to undefined returns
% false. Kinda like nulls in SQL.
cmp_json(undefined, _) ->
    false;
cmp_json(_, undefined) ->
    false;
cmp_json(A, B) ->
    couch_view:cmp_json(A, B).

