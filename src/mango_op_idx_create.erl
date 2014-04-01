-module(mango_op_idx_create).


-export([
    run/2
]).


run(Msg, Ctx) ->
    DbName = cloudant_dbname(Msg, Ctx),
    {ok, _} = mango_util:maybe_create_db(DbName),
    {QProps} = mango_msg:prop('query', Msg),
    Indexes = case couch_util:get_value(<<"indexes">>, QProps) of
        Indexes0 when is_list(Indexes0) ->
            Indexes0;
        Indexes0 ->
            throw({invalid_indexes, Indexes0})
    end,
    case create_indexes(Ctx, DbName, Indexes) of
        ok ->
            {ok, {[{<<"ok">>, 1}]}, Ctx};
        {error, NewCtx} ->
            {ok, Msg, NewCtx}
    end.


create_indexes(_, _, []) ->
    ok;
create_indexes(Ctx, DbName, [Index | Rest]) ->
    case mango_index:create(DbName, Index, Ctx) of
        ok ->
            create_indexes(Ctx, DbName, Rest);
        {error, Error} ->
            {error, mango_ctx:add_error(Ctx, Error)}
    end.


cloudant_dbname(Msg, Ctx) ->
    Username = mango_ctx:username(Ctx),
    DbName = mango_msg:dbname(Msg),
    {QProps} = mango_msg:prop('query', Msg),
    CollName = case couch_util:get_value(<<"createIndexes">>, QProps) of
        CollName0 when is_binary(CollName0) ->
            CollName0;
        CollName0 ->
            throw({invalid_index_collection, CollName0})
    end,
    RawDbName = <<Username/binary, "/", DbName/binary, ".", CollName/binary>>,
    mango_util:enc_dbname(RawDbName).    