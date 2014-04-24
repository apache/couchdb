-module(mango_action).

-export([
    new/1,
    run/3,
    
    format_error/1
]).


-include("mango.hrl").


new({Props}) ->
    ok;
new(_) ->
    ?MANGO_ERROR(action_must_be_an_object).


run(Resp, Db, Action) ->
    ok.


format_error(action_must_be_an_object) ->
    <<"Each action must be a JSON object.">>;
format_error(Else) ->
    mango_util:fmt("Unknown error: ~p", [Else]).

