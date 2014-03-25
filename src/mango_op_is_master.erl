-module(mango_op_is_master).

-export([
    run/2
]).


run(_Msg, Ctx) ->
    Reply = {[
        {<<"ismaster">>, true},
        {<<"maxBsonObjectSize">>, 16777216},
        {<<"localTime">>, mango_bson:current_time()},
        {<<"ok">>, 1}
    ]},
    {ok, Reply, Ctx}.

