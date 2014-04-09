-module(mango_op_kill_cursors).

-export([
    run/2
]).


-include("mango.hrl").


run(Msg, Ctx) ->
    CursorIds = mango_msg:prop(cursors, Msg),
    lists:foreach(fun(CI) ->
        catch mango_cursor:close(CI)
    end, CursorIds),
    {ok, Msg, Ctx}.
