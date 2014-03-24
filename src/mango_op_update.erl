-module(mango_op_update).

-export([
    run/2
]).


-include("mango.hrl").


run(Msg, Ctx) ->
    ok = check_not_multi_update(Msg),
    DbName = mango_util:cloudant_dbname(Msg, Ctx),
    OldDoc = find_doc(DbName, mango_msg:prop(selector, Msg)),
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
    ok.


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


find_doc(DbName, Selector) ->
    case Selector of
        {[{<<"_id">>, DocId}]} ->
            find_doc_by_id(DbName, DocId);
        _ ->
            throw(unsupported_doc_selector)
    end.


find_doc_by_id(DbName, DocId) ->
    try mango_util:defer(fabric, open_doc, [DbName, DocId, []]) of
        {ok, Doc} ->
            {ok, Doc};
        {not_found, _} ->
            not_found;
        {error, Reason} ->
            throw(Reason)
        Error ->
            throw(Error)
    catch error:database_does_not_exist ->
        not_found
    end.
