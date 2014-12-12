-module(mango_doc).


-export([
    from_bson/1,

    apply_update/2,
    update_as_insert/1,
    has_operators/1,

    get_field/2,
    get_field/3,
    parse_field/1,
    rem_field/2,
    set_field/3
]).


-include_lib("couch/include/couch_db.hrl").
-include("mango.hrl").


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


apply_update(#doc{body={Props}}=Doc, Update) ->
    NewProps = apply_update(Props, Update),
    Doc#doc{body={NewProps}};
apply_update({Props}, {Update}) ->
    Result = do_update({Props}, Update),
    case has_operators(Result) of
        true ->
            ?MANGO_ERROR(update_leaves_operators);
        false ->
            ok
    end,
    Result.


update_as_insert({Update}) ->
    NewProps = do_update_to_insert(Update, {[]}),
    apply_update(NewProps, {Update}).


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


do_update(Props, []) ->
    Props;
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
                    ?MANGO_ERROR({invalid_operand, Op, Value})
            end
    end,
    do_update(NewProps, Rest).


update_operator_fun(<<"$", _/binary>> = Op) ->
    OperatorFuns = [
        % Object operators
        {<<"$inc">>, fun do_update_inc/2},
        {<<"$rename">>, fun do_update_rename/2},
        {<<"$setOnInsert">>, fun do_update_set_on_insert/2},
        {<<"$set">>, fun do_update_set/2},
        {<<"$unset">>, fun do_update_unset/2},

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
            ?MANGO_ERROR({update_operator_not_supported, Op})
    end;
update_operator_fun(_) ->
    undefined.


do_update_inc(Props, []) ->
    Props;
do_update_inc(Props, [{Field, Incr} | Rest]) ->
    if is_number(Incr) -> ok; true ->
        ?MANGO_ERROR({invalid_increment, Incr})
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


do_update_set_on_insert(Props, _) ->
    % This is only called during calls to apply_update/2
    % which means this isn't an insert, so drop it on
    % the floor.
    Props.


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
                        fun(A) ->
                            Sel = mango_selector:normalize(ValToRem),
                            not mango_selector:match(A, Sel)
                        end;
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


do_update_to_insert([], Doc) ->
    Doc;
do_update_to_insert([{<<"$setOnInsert">>, {FieldProps}}], Doc) ->
    lists:foldl(fun({Field, Value}, DocAcc) ->
        set_field(DocAcc, Field, Value)
    end, Doc, FieldProps);
do_update_to_insert([{_, _} | Rest], Doc) ->
    do_update_to_insert(Rest, Doc).


get_field(Props, Field) ->
    get_field(Props, Field, no_validation).


get_field(Props, Field, Validator) when is_binary(Field) ->
    {ok, Path} = parse_field(Field),
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
    {ok, Path} = parse_field(Field),
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
    {ok, Path} = parse_field(Field),
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
            {lists:keystore(Name, 1, Props, {Name, Nested})}
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

parse_field(Field) ->
    case binary:match(Field, <<"\\">>, []) of
        nomatch ->
            % Fast path, no regex required
            {ok, check_non_empty(Field, binary:split(Field, <<".">>, [global]))};
        _ ->
            parse_field_slow(Field)
    end.

parse_field_slow(Field) ->
    Path = lists:map(fun
        (P) when P =:= <<>> ->
            ?MANGO_ERROR({invalid_field_name, Field});
        (P) ->
            re:replace(P, <<"\\\\">>, <<>>, [global, {return, binary}])
    end, re:split(Field, <<"(?<!\\\\)\\.">>)),
    {ok, Path}.

check_non_empty(Field, Parts) ->
    case lists:member(<<>>, Parts) of
        true ->
            ?MANGO_ERROR({invalid_field_name, Field});
        false ->
            Parts
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

parse_field_test() ->
    ?assertEqual({ok, [<<"ab">>]}, parse_field(<<"ab">>)),
    ?assertEqual({ok, [<<"a">>, <<"b">>]}, parse_field(<<"a.b">>)),
    ?assertEqual({ok, [<<"a.b">>]}, parse_field(<<"a\\.b">>)),
    ?assertEqual({ok, [<<"a">>, <<"b">>, <<"c">>]}, parse_field(<<"a.b.c">>)),
    ?assertEqual({ok, [<<"a">>, <<"b.c">>]}, parse_field(<<"a.b\\.c">>)),
    Exception = {mango_error, ?MODULE, {invalid_field_name, <<"a..b">>}},
    ?assertThrow(Exception, parse_field(<<"a..b">>)).

-endif.