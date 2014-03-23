-module(mango_error).

-export([
    format/1
]).


format({authorization_required, Stack}) ->
    fmterr(<<"Unauthorized: Not logged in">>, Stack);
format({{unauthorized, Reason}, Stack}) ->
    fmterr(<<"Unauthorized: ", Reason/binary>>, Stack);
format(_E) ->
    fmterr(<<"Unknown error">>, null).


fmterr(Reason, null) ->
    {[{<<"$err">>, Reason}]};
fmterr(Reason, Stack) ->
    StackId = erlang:phash2(Stack),
    {[
        {<<"$err">>, Reason},
        {<<"$stackid">>, StackId}
    ]}.