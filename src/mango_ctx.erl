
-module(mango_ctx).


-export([
    new/0,
    is_authed/1,
    last_error/1,
    add_error/2
]).


-record(mango_ctx, {
    user_ctx,
    errors = []
}).


new() ->
    #mango_ctx{}.


is_authed(#mango_ctx{user_ctx=UC}) when UC /= undefined ->
    true;
is_authed(#mango_ctx{}) ->
    false.


last_error(#mango_ctx{errors=E}) ->
    case E of
        [] ->
            undefined;
        [Error | _] ->
            Error
    end.


add_error(#mango_ctx{errors=Errors}=Ctx, Error) ->
    Ctx#mango_ctx{errors=[Error | Errors]}.
