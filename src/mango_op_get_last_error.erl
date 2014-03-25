-module(mango_op_get_last_error).

-export([
    run/2
]).


run(_Msg, Ctx) ->
    Msg = case mango_ctx:last_error(Ctx) of
        undefined ->
            null;
        {Error, _} ->
            mango_error:format(Error)
    end,
    {ok, {[
        {<<"ok">>, 1},
        {<<"err">>, Msg}
    ]}, Ctx}.
