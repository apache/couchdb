-module(mango_opts).

-export([
    validate/2,

    format_error/1,

    is_boolean/1,
    is_pos_integer/1
]).


-include("mango.hrl").


validate(Props, Opts) ->
    case mango_util:assert_ejson({Props}) of
        true ->
            ok;
        false ->
            ?MANGO_ERROR({invalid_ejson, {Props}})
    end,
    {Rest, Acc} = validate_opts(Opts, Props, []),
    case Rest of
        [] ->
            ok;
        [{BadKey, _} | _] ->
            ?MANGO_ERROR({invalid_key, BadKey})
    end,
    {ok, Acc}.


format_error({invalid_ejson, Val}) ->
    mango_util:fmt("Invalid JSON value: ~w", [Val]);
format_error({invalid_option, Key}) ->
    mango_util:fmt("Invalid or duplicate action key: ~s", [Key]);
format_error({missing_required_key, Key}) ->
    mango_util:fmt("Missing required key: ~s", [Key]);
format_error({invalid_value, Name, Expect, Found}) ->
    mango_util:fmt("Value for ~s is ~w, should be ~w", [Name, Found, Expect]);
format_error({invalid_value, Name, Value}) ->
    mango_util:fmt("Invalid value for ~s: ~w", [Name, Value]);
format_error({invalid_boolean, Val}) ->
    mango_util:fmt("Invalid boolean value: ~w", [Val]);
format_error(Else) ->
    mango_util:fmt("Unknown error: ~w", [Else]).


is_boolean(true) ->
    {ok, true};
is_boolean(false) ->
    {ok, false};
is_boolean(Else) ->
    ?MANGO_ERROR({invalid_boolean, Else}).


is_pos_integer(V) when is_integer(V), V > 0 ->
    {ok, V};
is_pos_integer(Else) ->
    ?MANGO_ERROR({invalid_pos_integer, Else}).


validate_opts([], Props, Acc) ->
    {Props, lists:reverse(Acc)};
validate_opts([{Name, Desc} | Rest], Props, Acc) ->
    case lists:keytake(Name, 1, Props) of
        {value, {Name, Prop}, RestProps} ->
            NewAcc = [validate_opt(Name, Desc, Prop) | Acc],
            validate_opts(Rest, RestProps, NewAcc);
        false ->
            NewAcc = [validate_opt(Name, Desc, undefined) | Acc],
            validate_opts(Rest, Props, NewAcc)
    end.


validate_opt(_Name, [], Value) ->
    Value;
validate_opt(Name, Desc0, undefined) ->
    case lists:keytake(optional, 1, Desc0) of
        {value, {optional, true}, Desc1} ->
            {value, {default, Value}, Desc2} = lists:keytake(default, 1, Desc1),
            false = (Value == undefined),
            validate_opt(Name, Desc2, Value);
        _ ->
            ?MANGO_ERROR({missing_required_key, Name})
    end;
validate_opt(Name, [{assert, Value} | Rest], Value) ->
    validate_opt(Name, Rest, Value);
validate_opt(Name, [{assert, Expect} | _], Found) ->
    ?MANGO_ERROR({invalid_value, Name, Expect, Found});
validate_opt(Name, [{validator, Fun} | Rest], Value) ->
    case Fun(Value) of
        {ok, Validated} ->
            validate_opt(Name, Rest, Validated);
        false ->
            ?MANGO_ERROR({invalid_value, Name, Value})
    end.


