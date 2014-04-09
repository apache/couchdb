-module(mango_op_query).

-export([
    run/2
]).


-include_lib("couch/include/couch_db.hrl").
-include("mango.hrl").


run(Msg, Ctx) ->
    Opts = read_flags(Msg),
    DbName = mango_util:cloudant_dbname(Msg, Ctx),
    Selector = mango_msg:prop('query', Msg),
    % Need to add skip/limit and all the other
    % bits to opts.
    {ok, CursorId, FirstDocs} = create_cursor(DbName, Selector, Opts, Ctx),
    Reply = [
        {flags, 0},
        {cursor_id, CursorId},
        {docs, FirstDocs}
    ],
    {ok, mango_msg:set_reply(Msg, Reply), Ctx}.

    
create_cursor(DbName, Selector, Opts, Ctx) ->
    {ok, CursorId} = mango_cursor:create(DbName, Selector, Opts, Ctx),
    case mango_cursor:next(CursorId) of
        {ok, Docs} ->
            {ok, CursorId, Docs};
        Error ->
            throw(Error)
    end.


read_flags(Msg) ->
    Flags = mango_msg:prop(flags, Msg),
    case <<Flags:32/integer>> of
            <<
                0:24/integer, % Reserved
                PartialResult:1/integer,
                ExhaustStream:1/integer,
                AwaitData:1/integer,
                NoCursorTimeout:1/integer,
                OplogReplay:1/integer,
                SlaveOk:1/integer,
                Tailable:1/integer,
                0:1/integer % Reserved
            >> ->
        
        if Tailable == 0 -> ok; true ->
            throw({not_supported, tailable_cursors})
        end,
        if AwaitData == 0 -> ok; true ->
            throw({not_supported, tailable_cursors})
        end,
        if SlaveOk == 0 -> ok; true ->
            throw({not_supported, slave_ok})
        end,
        if OplogReplay == 0 -> ok; true ->
            throw({not_supported, oplog_replay})
        end,
        
        Timeout = case NoCursorTimeout of
            0 -> [];
            1 -> [{timeout, infinity}]
        end,
        Exhaust = case ExhaustStream of
            0 -> [];
            1 -> [{stream, true}]
        end,
        Partial = case PartialResult of
            0 -> [];
            1 -> [{partial, true}]
        end,

        Timeout ++ Exhaust ++ Partial;
    _ ->
        throw({invalid_flags, Flags})
    end.
