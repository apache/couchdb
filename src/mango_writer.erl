-module(mango_writer).


-export([
    new/3,
    close/1,

    obj_open/1,
    obj_key/2,
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


close(#w{state = [], mfa = {_Mod, _Fun, Arg}} = W) ->
    {ok, Arg};
close(#w{state = [{obj, _N} | Rest]} = W) ->
    close(obj_close(W));
close(#w{state = [value, {arr, _N} | Rest]} = W) ->
    close(arr_close(W)).


obj_open(#w{state = [value | _] = St} = W) ->
    NewW = W#w{state = [{obj, 0} | St]},
    write(NewW, "{").


obj_key(#w{state=[{obj, N} | Rest]} = W, Key) when is_binary(Key) ->
    NewW = W#w{state=[value, {obj, N} | Rest]},
    case N > 0 of
        true ->
            write(NewW, [",", Key]);
        false ->
            write(NewW, Key)
    end.


obj_close(#w{state=[{obj, _N} | Rest]} = W) ->
    write(W#w{state = Rest}, "}").


arr_open(#w{state=[value | _] = St} = W) ->
    NewW = W#w{state = [value, {arr, 0} | St]},
    write(NewW, "[").


arr_close(#w{state=[value, {arr, _N} | Rest]} = W) ->
    NewW = W#w{state = Rest},
    write(NewW, "]").


add_value(#w{state=[value, {arr, N} | Rest]} = W) ->
    NewW = W#w{state=[value, {arr, N+1} | Rest]},
    case N > 0 of
        true ->
            write(NewW, [",", jiffy:encode(Value)]);
        false ->
            write(NewW, jiffy:encode(Value))
    end;
add_value(#w{state = [value, {obj, N} | Rest]} = W) ->
    NewW = W#w{state = [{obj, N+1} | Rest]},
    write(NewW, [":", jiffy:encode(Value)]);
add_value(#w{state = [value]} = W) ->
    NewW = W#w{state = []},
    write(NewW, jiffy:encode(Value)).


write(#w{mfa = {Mod, Fun, Arg}} = W, IoData) ->
    {ok, NewArg} = Mod:Fun(Arg, IoData),
    W#w{mfa = {Mod, Fun, Arg}}.
