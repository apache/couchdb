
-module(mango_ctx).


-export([
    new/0,
    is_authed/1,
    set_auth/3,
    last_error/1,
    add_error/2,
    add_error/3
]).


% Copied from couch_db.hrl
-record(user_ctx, {
    name=null,
    roles=[],
    handler
}).

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


set_auth(#mango_ctx{user_ctx=undefined}=Ctx, User, Roles) ->
    UC = #user_ctx{
        name = User,
        roles = Roles,
        handler = mango
    },
    Ctx#mango_ctx{user_ctx=UC};
set_auth(_, _, _) ->
    throw(already_authenticated).


last_error(#mango_ctx{errors=E}) ->
    case E of
        [] ->
            undefined;
        [Error | _] ->
            Error
    end.


add_error(Ctx, Error) ->
    add_error(Ctx, Error, null).


add_error(#mango_ctx{errors=Errors}=Ctx, Error, Stack) ->
    twig:log(error, "Mango error: ~p~n    ~p", [Error, Stack]),
    Ctx#mango_ctx{errors=[{Error, Stack} | Errors]}.
