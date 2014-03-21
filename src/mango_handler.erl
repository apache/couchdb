-module(mango_handler).


-export([
    dispatch/3
]).


dispatch(Type, Props, Ctx) ->
    try
        check_auth(Type, Props, Ctx),
        Mod = get_handler_mod(Type, Props),
        Mod:run(Props, Ctx)
    catch
        throw:Reason ->
            {ok, mango_ctx:add_error(Ctx, Reason)};
        error:Reason ->
            {ok, mango_ctx:add_error(Ctx, Reason)}
    end.


check_auth('query', Props, Ctx) ->
    case is_cmd(Props) of
        true ->
            {'query', Doc} = list:keyfind('query', 1, Props),
            IsGetNonce = get_bool(<<"getnonce">>, Doc),
            IsAuthenticate = get_bool(<<"authenticate">>, Doc),
            case IsGetNonce orelse IsAuthenticate of
                true ->
                    true;
                false ->
                    ensure_authed(Ctx)
            end;
        false ->
            ensure_authed(Ctx)
    end;
check_auth(_, _, Ctx) ->
    ensure_authed(Ctx).


ensure_authed(Ctx) ->
    case mango_ctx:is_authed(Ctx) of
        true ->
            ok;
        false ->
            throw(authorization_required)
    end.


get_handler_mod(update, _) ->
    mango_op_update;
get_handler_mod(insert, _) ->
    mango_op_insert;
get_handler_mod('query', Props) ->
    case is_cmd(Props) of
        true ->
            {'query', {QProps}} = lists:keyfind('query', 1, Props),
            try
                lists:foreach(fun({Member, Cmd}) ->
                    case lists:keyfind(Member, 1, QProps) of
                        {Member, _} ->
                            throw({found, Cmd});
                        _ ->
                            ok
                    end
                end, cmd_list()),

                mango_op_unsupported

            catch throw:{found, Cmd} ->
                Cmd
            end;
        false ->
            mango_op_query
    end;
get_handler_mod(get_more, _) ->
    mango_op_get_more;
get_handler_mod(kill_cursors, _) ->
    mango_op_kill_cursors;
get_handler_mod(_, _) ->
    throw(invalid_protocol_operation).


is_cmd(Props) ->
    {collection, C} = lists:keyfind(collection, 1, Props),
    Opts = [{capture, all_but_first, binary}],
    case re:run(C, <<"^[^.]+\\.(.*)$">>, Opts) of
        {match, [<<"$cmd">>]} ->
            true;
        _ ->
            false
    end.


get_bool(Name, {Props}) ->
    case lists:keyfind(Name, 1, Props) of
        {_, true} -> true;
        {_, 1} -> true;
        _ -> false
    end.


cmd_list() ->
    [
        {<<"getnonce">>, mango_op_get_nonce},
        {<<"authenticate">>, mango_op_login},
        {<<"logout">>, mango_op_logout},

        {<<"buildinfo">>, mango_op_build_info},
        {<<"status">>, mango_op_stats},

        {<<"listdatabases">>, mango_op_db_list},
        {<<"dropdatabase">>, mango_op_db_drop},

        {<<"create">>, mango_op_coll_create},
        {<<"drop">>, mango_op_coll_drop},
        {<<"renamecollection">>, mango_op_coll_rename},
        {<<"iscapped">>, mango_op_coll_is_capped},
        {<<"collstats">>, mango_op_coll_stats},

        {<<"count">>, mango_op_count},
        {<<"distinct">>, mango_op_distinct},

        {<<"findandmodify">>, mango_op_find_and_modify},
    
        {<<"getlasterror">>, mango_op_get_last_error}
    ].
