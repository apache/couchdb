
-module(mango_ctx).


-export([
    new/0,
    
    is_authed/1,
    set_auth/3,
    username/1,

    clear_error/1,
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
    errors = [],
    last_error
}).


new() ->
    #mango_ctx{}.


is_authed(#mango_ctx{user_ctx=UC}) when UC /= undefined ->
    true;
is_authed(#mango_ctx{}) ->
    false.


set_auth(#mango_ctx{user_ctx=undefined}=Ctx, User, Roles) ->
    twig:log(error, "User auth: ~s ~w", [User, Roles]),
    UC = #user_ctx{
        name = User,
        roles = Roles,
        handler = mango
    },
    Ctx#mango_ctx{user_ctx=UC};
set_auth(_, _, _) ->
    throw(already_authenticated).


username(#mango_ctx{user_ctx=UC}) when UC /= undefined ->
    UC#user_ctx.name;
username(_) ->
    throw(authorization_required).


clear_error(Ctx) ->
    Ctx#mango_ctx{last_error=undefined}.


last_error(#mango_ctx{last_error=Error}) ->
    Error.


add_error(Ctx, Error) ->
    add_error(Ctx, Error, null).


add_error(#mango_ctx{errors=Errors}=Ctx, Error, Stack) ->
    twig:log(error, "Mango error: ~p~n    ~p", [Error, Stack]),
    Ctx#mango_ctx{
        errors=[{Error, Stack} | Errors],
        last_error = {Error, Stack}
    }.
