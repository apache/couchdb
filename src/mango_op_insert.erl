-module(mango_op_insert).

-export([
    run/2
]).


-include_lib("couch/include/couch_db.hrl").


run(Msg, Ctx) ->
    {ok, DbName} = mango_util:maybe_create_db(Msg, Ctx),
    Docs = [mango_doc:from_bson(Doc) || Doc <- mango_msg:prop(docs, Msg)],
    % If ContinueOnError is set we can just use a bulk
    % update. If not we have to try and insert one at
    % a time so we can stop on the first error.
    NewCtx = case continue_on_error(Msg) of
        true ->
            batch_update(DbName, Docs, Ctx);
        false ->
            linear_update(DbName, Docs, Ctx)
    end,
    {ok, Msg, NewCtx}.


continue_on_error(Msg) ->
    case mango_msg:prop(flags, Msg) of
        Flags when is_integer(Flags), Flags band 1 == 1 ->
            true;
        _ ->
            false
    end.


batch_update(DbName, Docs, Ctx) ->
    Results = case mango_util:defer(fabric, update_docs, [DbName, Docs, []]) of
        {ok, Results0} ->
            Results0;
        {accepted, Results0} ->
            Results0;
        {aborted, Errors} ->
            Errors
    end,
    Pairs = lists:zip(Docs, Results),
    lists:foldl(fun handle_result/2, Ctx, Pairs).


linear_update(_DbName, [], Ctx) ->
    Ctx;
linear_update(DbName, [Doc | Rest], Ctx) ->
    case batch_update(DbName, [Doc], Ctx) of
        Ctx ->
            linear_update(DbName, Rest, Ctx);
        NewCtx ->
            NewCtx
    end.


handle_result({_Doc, {ok, _Rev}}, Ctx) ->
    Ctx;
handle_result({#doc{id=Id}, Error}, Ctx) ->
    mango_ctx:add_error(Ctx, {doc_update_error, Id, Error}).
