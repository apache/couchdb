-module(mango_op_unsupported).

-export([
    run/2
]).


-include("mango.hrl").


run(_Msg, Ctx) ->
    {ok, {[{<<"$err">>, <<"unsupported operation">>}]}, Ctx}.
