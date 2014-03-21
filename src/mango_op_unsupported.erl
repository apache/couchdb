-module(mango_op_unsupported).

-export([
    run/2
]).


-include("mango.hrl").


run(_Props, _Ctx) ->
    ok.
