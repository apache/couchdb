-module(mango_op_insert).

-export([
    run/2
]).


-include_lib("couch/include/couch_db.hrl").


run(Msg, Ctx) ->
    {ok, DbName} = maybe_create_db(Msg, Ctx),
    Docs = prepare_docs(mango_msg:prop(docs, Msg)),
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


maybe_create_db(Msg, Ctx) ->
    Username = mango_ctx:username(Ctx),
    Collection = mango_msg:prop(collection, Msg),
    DbName = mango_util:enc_dbname(<<Username/binary, "/", Collection/binary>>),
    try
        mem3:shards(DbName),
        {ok, DbName}
    catch
        error:database_does_not_exist ->        
            case mango_util:defer(fabric, create_db, [DbName]) of
                ok ->
                    {ok, DbName};
                accepted ->
                    {ok, DbName};
                Error ->
                    throw(Error)
            end
    end.


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


handle_result({_Doc, {ok, _Rev}}, Ctx) ->
    Ctx;
handle_result({#doc{id=Id}, Error}, Ctx) ->
    mango_ctx:add_error(Ctx, {doc_update_error, Id, Error}).


linear_update(_DbName, [], Ctx) ->
    Ctx;
linear_update(DbName, [Doc | Rest], Ctx) ->
    case batch_update(DbName, [Doc], Ctx) of
        Ctx ->
            linear_update(DbName, Rest, Ctx);
        NewCtx ->
            NewCtx
    end.


prepare_docs([]) ->
    [];
prepare_docs([Doc | Rest]) ->
    [prepare_doc(Doc) | prepare_docs(Rest)].


prepare_doc({Props}) ->
    DocProps = case lists:keytake(<<"_id">>, 1, Props) of
        {value, {<<"_id">>, DocId0}, RestProps} ->
            DocId = maybe_extract_docid(DocId0),
            [{<<"_id">>, DocId} | RestProps];
        false ->
            Props
    end,
    Doc = couch_doc:from_json_obj({DocProps}),
    case Doc#doc.id of
        <<"">> ->
            Doc#doc{id=couch_uuids:new(), revs={0, []}};
        _ ->
            Doc
    end.


maybe_extract_docid({[{<<"$id">>, Id}]}) ->
    Id;
maybe_extract_docid(Id) ->
    Id.

