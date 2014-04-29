-module(mango_writer).


-export([
    new/3,
    close/1,
    is_writer/1,
    script/2,

    obj_open/1,
    obj_key/2,
    obj_pair/3,
    obj_close/1,

    arr_open/1,
    arr_close/1,

    add_value/2
]).


-record(w, {
    state,
    mfa
}).


new(Mod, Fun, Arg) ->
    {ok, #w{
        state = [value],
        mfa = {Mod, Fun, Arg}
    }}.


close(#w{state = [], mfa = {_Mod, _Fun, Arg}}) ->
    {ok, Arg};
close(#w{state = [{obj, _N} | _]} = W) ->
    {ok, NewW} = obj_close(W),
    close(NewW);
close(#w{state = [value, {arr, _N} | _]} = W) ->
    {ok, NewW} = arr_close(W),
    close(NewW).


is_writer(#w{}) ->
    true;
is_writer(_) ->
    false.


script(#w{} = W, []) ->
    {ok, W};
script(#w{} = W, [Fun | Rest]) when is_atom(Fun) ->
    {ok, NewW} = ?MODULE:Fun(W),
    script(NewW, Rest);
script(#w{} = W, [{Fun, Arg1} | Rest]) ->
    {ok, NewW} = ?MODULE:Fun(W, Arg1),
    script(NewW, Rest);
script(#w{} = W, [{Fun, Arg1, Arg2} | Rest]) ->
    {ok, NewW} = ?MODULE:Fun(W, Arg1, Arg2),
    script(NewW, Rest).


obj_open(#w{state = [value | Rest]} = W) ->
    NewW = W#w{state = [{obj, 0} | Rest]},
    write(NewW, "{").


obj_key(#w{state=[{obj, N} | Rest]} = W, Key) when is_binary(Key) ->
    NewW = W#w{state=[value, {obj, N} | Rest]},
    JsonKey = encode(Key),
    case N > 0 of
        true ->
            write(NewW, [",", JsonKey, ":"]);
        false ->
            write(NewW, [JsonKey, ":"])
    end.


obj_pair(#w{state=[{obj, N} | Rest]} = W, Key, Val) when is_binary(Key) ->
    NewW = W#w{state=[{obj, N+1} | Rest]},
    JsonKey = encode(Key),
    JsonVal = encode(Val),
    case N > 0 of
        true ->
            write(NewW, [",", JsonKey, ":", JsonVal]);
        false ->
            write(NewW, [JsonKey, ":", JsonVal])
    end.


obj_close(#w{state=[{obj, _N} | Rest]} = W) ->
    State = case Rest of
        [{arr, _} | _] ->
            [value | Rest];
        Else ->
            Else
    end,
    write(W#w{state = State}, "}").


arr_open(#w{state=[value | Rest]} = W) ->
    NewW = W#w{state = [value, {arr, 0} | Rest]},
    write(NewW, "[").


arr_close(#w{state=[value, {arr, _N} | Rest]} = W) ->
    State = case Rest of
        [{arr, _} | _] ->
            [value | Rest];
        Else ->
            Else
    end,
    NewW = W#w{state = State},
    write(NewW, "]").


add_value(#w{state=[value, {arr, N} | Rest]} = W, Value) ->
    NewW = W#w{state=[value, {arr, N+1} | Rest]},
    case N > 0 of
        true ->
            write(NewW, [",", encode(Value)]);
        false ->
            write(NewW, encode(Value))
    end;
add_value(#w{state = [value, {obj, N} | Rest]} = W, Value) ->
    NewW = W#w{state = [{obj, N+1} | Rest]},
    write(NewW, [encode(Value)]);
add_value(#w{state = [value]} = W, Value) ->
    NewW = W#w{state = []},
    write(NewW, encode(Value)).


write(#w{mfa = {Mod, Fun, Arg}} = W, IoData) ->
    {ok, NewArg} = Mod:Fun(Arg, IoData),
    %twig:log(err, "WS: ~p", [W#w.state]),
    {ok, W#w{mfa = {Mod, Fun, NewArg}}}.


encode(Value) ->
    try
        jiffy:encode(Value)
    catch _:_ ->
        throw({invalid_json, Value})
    end.
