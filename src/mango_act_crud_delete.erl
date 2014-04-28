-module(mango_act_crud_delete).

-export([
    init/1,
    run/3,
    
    format_error/1
]).


init({_Props}) ->
    ok.


run(_Resp, _Db, _St) ->
    ok.


format_error(Else) ->
    mango_util:fmt("Unknown error: ~p", [Else]).
