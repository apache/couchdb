-module(mango_doc).


-export([
    from_bson/1,
    
    apply_update/2,
    
    has_operators/1
]).


-include_lib("couch/include/couch_db.hrl").


prepare_docs([]) ->
    [];
prepare_docs([Doc | Rest]) ->
    [prepare_doc(Doc) | prepare_docs(Rest)].


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


apply_update(#doc{body={Props}}=Doc, {Update}) ->
    do_update(Props, Update).


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


% Object ops
do_update(Props, [{Op, Value} | Rest]) ->
    UpdateFun = update_operator_fun(Op),
    NewProps = case UpdateFun of
        undefined ->
            do_replace(Props, Op, Value);
        Fun when is_function(Fun, 2) ->
            case Value of
                {ValueProps} ->
                    Fun(Props, ValueProps);
                _ ->
                    throw({invalid_operand, Op, Value})
            end
    end,
    do_update(NewProps, Rest);


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
do_update_inc(Props0, [{Field, Incr} | Rest]) ->
    if is_number(Incr) -> ok; true ->
        throw({invalid_increment, Incr})
    end,
    Props = do_update_inc(Props0, Rest),
    Value = case get_field(Props, Field, fun is_number/1, <<"$inc">>) of
        Value when is_number(Value) ->
            set_field(Props, Field, Value + Incr);
        not_found ->
            set_field(Props, Field, Incr);
        _ ->
            Props
    end.


do_update_rename(Props, Fields) ->
    Props.


do_update_set(Props, Fields) ->
    Props.


do_update_unset(Props, Fields) ->
    Props.


do_update_add_to_set(Props, Fields) ->
    Props.


do_update_pop(Props, Fields) ->
    Props.


do_update_pull_all(Props, Fields) ->
    Props.


do_update_pull(Props, Fields) ->
    Props.


do_update_push_all(Props, Fields) ->
    Props.


do_update_push(Props, Fields) ->
    Props.


do_update_bitwise(Props, Fields) ->
    Props.


get_field(Props, Field, Validator) when is_binary(Field) ->
    Path = re:split(Field, <<"\\.">>),
    lists:foreach(fun(P) ->
        if P /= <<>> -> ok; true ->
            throw({invalid_field_name, Field})
        end
    end, Path),
    get_field(Props, Path, Validator);
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
    