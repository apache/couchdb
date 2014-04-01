-module(mango_msg).

-export([
    is_msg/1,

    new/1,
    reply/2,

    set_reply/2,
    set_reply/3,

    type/1,
    dbname/1,
    collection/1,
    prop/2,

    is_cmd/1,
    requires_auth/1
]).


% Message op codes
-define(OP_REPLY, 1).
-define(OP_MSG, 1000).
-define(OP_UPDATE, 2001).
-define(OP_INSERT, 2002).
-define(OP_QUERY, 2004).
-define(OP_GET_MORE, 2005).
-define(OP_DELETE, 2006).
-define(OP_KILL_CURSORS, 2007).


-record(mango_msg, {
    type,
    req_id,
    resp_to,
    dbname,
    collection,
    reply,
    props
}).

-record(mango_reply, {
    flags = 0,
    cursor_id = 0,
    offset = 0,
    docs = []
}).


is_msg(#mango_msg{}) ->
    true;
is_msg(_) ->
    false.


new(<<Size:32/little-signed-integer, Rest/binary>>) when Size-4 =< size(Rest) ->
    NumBytes = Size - 4,
    <<Msg:NumBytes/binary, NonMsg/binary>> = Rest,
    case parse(Msg) of
        {ok, Type, ReqId, RespTo, Props} ->
            {ok, new_msg(Type, ReqId, RespTo, Props), NonMsg};
        Error ->
            {error, Error}
    end;
new(_) ->
    undefined.


reply(#mango_msg{type='query', reply=undefined}=Msg, Ctx) ->
    ReqId = req_id(),
    RespTo = Msg#mango_msg.req_id,
    Flags = 16#00000002, % Query failure
    CursorId = 0,
    Offset = 0,
    NumDocs = 1,
    ErrorDoc = mango_error:as_doc(Ctx),
    DocBin = mango_bson:from_ejson(ErrorDoc),
    Size = 36 + size(DocBin),
    <<
        Size:32/little-signed-integer,
        ReqId:32/little-signed-integer,
        RespTo:32/little-signed-integer,
        ?OP_REPLY:32/little-signed-integer,
        Flags:32/little-signed-integer,
        CursorId:64/little-signed-integer,
        Offset:32/little-signed-integer,
        NumDocs:32/little-signed-integer,
        DocBin/binary
    >>;
reply(#mango_msg{type=Type, reply=Reply}=Msg, _) when Reply /= undefined ->
    if Type /= 'query' andalso Type /= get_more -> undefined; true ->
        ReqId = req_id(),
        RespTo = Msg#mango_msg.req_id,
        Flags = Reply#mango_reply.flags,
        CursorId = Reply#mango_reply.cursor_id,
        Offset = Reply#mango_reply.offset,
        NumDocs = case Reply#mango_reply.docs of
            Docs when is_list(Docs) ->
                length(Docs);
            Doc when tuple_size(Doc) == 1 ->
                1
        end,
        DocsBinary = mango_bson:from_ejson(Reply#mango_reply.docs),
        Size = 36 + size(DocsBinary),
        <<
            Size:32/little-signed-integer,
            ReqId:32/little-signed-integer,
            RespTo:32/little-signed-integer,
            ?OP_REPLY:32/little-signed-integer,
            Flags:32/little-signed-integer,
            CursorId:64/little-signed-integer,
            Offset:32/little-signed-integer,
            NumDocs:32/little-signed-integer,
            DocsBinary/binary
        >>
    end;
reply(_, _) ->
    undefined.


set_reply(Msg, []) ->
    Msg;
set_reply(Msg, [{Key, Val} | Rest]) ->
    NewMsg = set_reply(Msg, Key, Val),
    set_reply(NewMsg, Rest).


set_reply(Msg, flags, Val) ->
    R0 = get_reply(Msg),
    R1 = R0#mango_reply{flags=Val},
    Msg#mango_msg{reply=R1};
set_reply(Msg, cursor_id, Val) ->
    R0 = get_reply(Msg),
    R1 = R0#mango_reply{cursor_id=Val},
    Msg#mango_msg{reply=R1};
set_reply(Msg, offset, Val) ->
    R0 = get_reply(Msg),
    R1 = R0#mango_reply{offset=Val},
    Msg#mango_msg{reply=R1};
set_reply(Msg, docs, Val) ->
    R0 = get_reply(Msg),
    R1 = R0#mango_reply{docs=Val},
    Msg#mango_msg{reply=R1}.


type(#mango_msg{type=Type}) ->
    Type.


dbname(#mango_msg{dbname=DbName}) ->
    DbName.


collection(#mango_msg{collection=Collection}) ->
    Collection.


prop(Name, Msg) ->
    {Name, Value} = lists:keyfind(Name, 1, Msg#mango_msg.props),
    Value.


is_cmd(#mango_msg{type='query', collection= <<"$cmd">>}) ->
    true;
is_cmd(_) ->
    false.


requires_auth(Msg) ->
    case is_cmd(Msg) of
        true ->
            {QProps} = prop('query', Msg),
            Keys = [mango_util:to_lower(K) || {K, _} <- QProps],
            not has_authless_key(Keys);
        false ->
            true
    end.


new_msg(Type, ReqId, RespTo, Props) ->
    {collection, FullCollection} = lists:keyfind(collection, 1, Props),
    Opts = [{capture, all_but_first, binary}],
    case re:run(FullCollection, <<"^([^.]+)\\.(.*)$">>, Opts) of
        {match, [DbName, Collection]} ->
            #mango_msg{
                type = Type,
                req_id = ReqId,
                resp_to = RespTo,
                dbname = DbName,
                collection = Collection,
                props = Props
            };
        _ ->
            throw({invalid_collection, FullCollection})
    end.


parse(Data) ->
    % Grab our values from the message header
    case Data of
        <<
            ReqId:32/little-signed-integer,
            RespTo:32/little-signed-integer,
            OpCode:32/little-signed-integer,
            Body/binary
        >> ->
            parse(ReqId, RespTo, OpCode, Body);
        _ ->
            {error, invalid_msg_header}
    end.


parse(ReqId, RespTo, OpCode, Body) ->
    case shape(OpCode) of
        {Type, Props} when is_list(Props) ->
            case parse_bin(Props, Body, []) of
                {ok, ParsedProps} ->
                    {ok, Type, ReqId, RespTo, ParsedProps};
                Error ->
                    Error
            end;
        Error ->
            Error
    end.


shape(?OP_UPDATE) ->
    {update, [
        {zero, int32},
        {collection, cstring},
        {flags, int32},
        {selector, doc},
        {update, doc}
    ]};
shape(?OP_INSERT) ->
    {insert, [
        {flags, int32},
        {collection, cstring},
        {docs, docs}
    ]};
shape(?OP_QUERY) ->
    {'query', [
        {flags, int32},
        {collection, cstring},
        {skip, int32},
        {limit, int32},
        {'query', doc},
        {fields, docs}
    ]};
shape(?OP_GET_MORE) ->
    {get_more, [
        {zero, int32},
        {collection, cstring},
        {limit, int32},
        {cursor_id, int64}
    ]};
shape(?OP_DELETE) ->
    {delete, [
        {zero, int32},
        {collection, cstring},
        {flags, int32},
        {selector, doc}
    ]};
shape(?OP_KILL_CURSORS) ->
    {kill_cursors, [
        {zero, int32},
        {cursors, cursors}
    ]};
shape(Op) ->
    {error, {invalid_opcode, Op}}.


parse_bin([], <<>>, Acc) ->
    {ok, lists:reverse(Acc)};
parse_bin([], _, _) ->
    {error, trailing_data};
parse_bin([{Name, int32} | Rest], Data, Acc) ->
    case Data of
        <<Val:32/little-signed-integer, R/binary>> ->
            parse_bin(Rest, R, [{Name, Val} | Acc]);
        _ ->
            {error, {truncated_data, Name}}
    end;
parse_bin([{Name, int64} | Rest], Data, Acc) ->
    case Data of
        <<Val:64/little-signed-integer, R/binary>> ->
            parse_bin(Rest, R, [{Name, Val} | Acc]);
        _ ->
            {error, {truncated_data, Name}}
    end;
parse_bin([{Name, cstring} | Rest], Data, Acc) ->
    case parse_cstring(Data, 0) of
        {Val, R} ->
            parse_bin(Rest, R, [{Name, Val} | Acc]);
        _ ->
            {error, {unterminated_cstring, Name}}
    end;
parse_bin([{Name, doc} | Rest], Data, Acc) ->
    case mango_bson:to_ejson(Data, [return_rest]) of
        {ok, Val, R} ->
            parse_bin(Rest, R, [{Name, Val} | Acc]);
        {error, Reason} ->
            {error, {invalid_bson, Reason}}
    end;
parse_bin([{Name, docs}], Data, Acc) ->
    % This has to be the last element of the message
    % so we assert that in the pattern match.
    case parse_docs(Data, []) of
        {ok, Docs} ->
            {ok, lists:reverse(Acc, [{Name, Docs}])};
        Error ->
            Error
    end;
parse_bin([{Name, cursors} | Rest], Data, Acc) ->
    case Data of
        <<Num:32/little-signed-integer, CursorData/binary>> ->
            case parse_cursors(Num, CursorData, []) of
                {ok, Cursors, R} ->
                    parse_bin(Rest, R, [{Name, Cursors} | Acc]);
                Error ->
                    Error
            end;
        _ ->
            {error, truncated_cursors_size}
    end.


parse_cstring(Bin, O) ->
    case Bin of
        <<Val:O/binary, 0, Rest/binary>> ->
            {Val, Rest};
        <<_:O/binary>> ->
            error;
        _ ->
            parse_cstring(Bin, O+1)
    end.


parse_docs(<<>>, Acc) ->
    {ok, lists:reverse(Acc)};
parse_docs(Data, Acc) ->
    case mango_bson:to_ejson(Data, [return_rest]) of
        {ok, Doc, R} ->
            parse_docs(R, [Doc | Acc]);
        {error, Reason} ->
            {error, {invalid_bson, Reason}}
    end.


parse_cursors(0, Rest, Acc) ->
    {ok, lists:reverse(Acc), Rest};
parse_cursors(N, <<Cursor:64/little-signed-integer, Rest/binary>>, Acc) ->
    parse_cursors(N-1, Rest, [Cursor | Acc]);
parse_cursors(_, _, _) ->
    {error, truncated_cursors}.


get_reply(#mango_msg{reply=undefined}) ->
    #mango_reply{};
get_reply(#mango_msg{reply=R}) ->
    R.


req_id() ->
    maybe_seed(),
    random:uniform(16#FFFFFFFF) - 16#80000000.


maybe_seed() ->
    case get(random_seed) of
        undefined ->
            random:seed(os:timestamp());
        _ ->
            ok
    end.


has_authless_key([]) ->
    false;
has_authless_key([K | Rest]) ->
    twig:log(error, "Checking: ~s", [K]),
    case lists:member(K, authless_keys()) of
        true ->
            true;
        false ->
            has_authless_key(Rest)
    end.
    

authless_keys() ->
    [
        <<"ismaster">>,
        <<"getnonce">>,
        <<"authenticate">>,
        <<"saslstart">>,
        <<"getlasterror">>
    ].
