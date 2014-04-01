-module(mango_op_update).

-export([
    run/2
]).


-include_lib("couch/include/couch_db.hrl").
-include("mango.hrl").


run(Msg, Ctx) ->
    ok = check_not_multi_update(Msg),
    DbName = mango_util:cloudant_dbname(Msg, Ctx),
    OldDoc = find_doc(DbName, mango_msg:prop(selector, Msg), Ctx),
    Update = mango_msg:prop(update, Msg),
    NewDoc = case {OldDoc, is_upsert(Msg)} of
        {not_found, true} ->
            case mango_doc:has_operators(Update) of
                true ->
                    throw(invalid_update_with_operators);
                false ->
                    ok
            end,
            mango_doc:from_bson(Update);
        {not_found, false} ->
            throw(doc_not_found);
        {#doc{}, _} ->
            mango_doc:apply_update(OldDoc, Update)
    end,
    Results = case mango_doc:save(DbName, NewDoc, Ctx) of
        {ok, Results0} ->
            Results0;
        {accepted, Results0} ->
            Results0;
        {error, Errors} ->
            Errors
    end,
    FinalCtx = lists:foldl(fun
        ({_, {ok, _Rev}}, Acc) ->
            Acc;
        ({#doc{id=Id}, Error}, Acc) ->
            mango_ctx:add_error(Acc, {doc_update_error, Id, Error})
    end, Ctx, Results),
    {ok, Msg, FinalCtx}.


check_not_multi_update(Msg) ->
    % We don't (yet) support multi-updates. We'll just
    % throw an error for now.
    Flags = mango_msg:prop(flags, Msg),
    if Flags band 16#00000002 == 0 -> ok; true ->
        throw(multiupdate_not_supported)
    end.


is_upsert(Msg) ->
    Flags = mango_msg:props(flags, Msg),
    Flags band 16#00000001.


find_doc(DbName, Selector, Ctx) ->
    case Selector of
        {[{<<"_id">>, DocId}]} ->
            mango_doc:open(DbName, DocId, Ctx);
        _ ->
            throw(unsupported_doc_selector)
    end.



