-module(mango_handler).


-export([
    dispatch/2
]).


dispatch(Msg, CtxIn) ->
    try
        check_auth(Msg, CtxIn),
        Mod = get_handler_mod(mango_msg:type(Msg), Msg),
        Ctx = case Mod of
            mango_op_get_last_error ->
                CtxIn;
            _ ->
                mango_ctx:clear_error(CtxIn)
        end,
        case Mod:run(Msg, Ctx) of
            {ok, Reply, NewCtx} ->
                {ok, to_msg(Msg, Reply), NewCtx};
            {error, HandlerError} ->
                NewCtx = mango_ctx:add_error(Ctx, HandlerError),
                {ok, Msg, NewCtx}
        end
    catch
        throw:Reason ->
            Stack = erlang:get_stacktrace(),
            {ok, Msg, mango_ctx:add_error(CtxIn, Reason, Stack)};
        error:Reason ->
            Stack = erlang:get_stacktrace(),
            {ok, Msg, mango_ctx:add_error(CtxIn, Reason, Stack)}
    end.


to_msg(Msg, Reply) ->
    case mango_msg:is_msg(Reply) of
        true ->
            Reply;
        false ->
            mango_msg:set_reply(Msg, docs, Reply)
    end.


check_auth(Msg, Ctx) ->
    case mango_ctx:is_authed(Ctx) of
        true ->
            ok;
        false ->
            case mango_msg:requires_auth(Msg) of
                true ->
                    throw(authorization_required);
                false ->
                    ok
            end
    end.


get_handler_mod(update, _) ->
    mango_op_update;
get_handler_mod(insert, _) ->
    mango_op_insert;
get_handler_mod('query', Msg) ->
    case mango_msg:is_cmd(Msg) of
        true ->
            {QProps} = mango_msg:prop('query', Msg),
            QKeys = [mango_util:to_lower(K) || {K, _} <- QProps],
            try
                lists:foreach(fun({KeyName, Cmd}) ->
                    case lists:member(KeyName, QKeys) of
                        true ->
                            throw({found, Cmd});
                        false ->
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


cmd_list() ->
    [
        {<<"ismaster">>, mango_op_is_master},

        {<<"saslstart">>, mango_op_auth_sasl_start},
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
