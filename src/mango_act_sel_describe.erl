-module(mango_act_sel_describe).

-export([
    init/2,
    run/3,
    
    format_error/1
]).


init(_Db, {_Props}) ->
    ok.


run(_Resp, _Db, _St) ->
    ok.


format_error(Else) ->
    mango_util:fmt("Unknown error: ~p", [Else]).