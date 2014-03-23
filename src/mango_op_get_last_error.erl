-module(mango_op_get_last_error).

-export([
    run/2
]).


run(_Msg, Ctx) ->
    {ok, mango_error:format(mango_ctx:last_error(Ctx))}.
