-module(mango_msg).

-export([
    new/0
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

-define(LI, little-integer).


-record(mreq_update, {
    req_id,
    resp_to,
    collection,
    flags,
    selector,
    update
}).


new(<<Size:4/?LI, Rest/binary>>=All) ->
    case (Size-4) =< size(Rest) of
        true ->
            <<Msg:(Size-4)/binary, NonMsg/binary>> = Rest,
            {parse(Msg), NonMsg};
        false ->
            {undefined, All}
    end.


parse(Data) ->
    % Grab our values from the message header
    case Data of
        <<ReqId:4/?LI, RespTo:4/?LI, OpCode:4/?LI, Body/binary>> ->
            parse(ReqId, RespTo, OpCode, Body);
        _ ->
            {error, invalid_msg_header}
    end.


parse(ReqId, RespTo, OpCode, Body) ->
    Base = [{resp_to, RespTo}, {req_id, ReqId}],
    case shape(OpCode) of
        {Type, Props} when is_list(Props) ->
            case parse_bin(Props, Body, Base) of
                {ok, Props} ->
                    {ok, {Type, Props}};
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
    {query, [
        {flags, int32},
        {collection, cstring},
        {skip, int32},
        {return, 32},
        {query, doc},
        {fields, docs}
    ]};
shape(?OP_GET_MORE) ->
    {get_more, [
        {zero, int32},
        {collection, cstring},
        {return, int32},
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
    {ok, lists:reverse(Acc), Data};
parse_bin([], _, _) ->
    {error, trailing_data};
parse_bin([{Name, int32} | Rest], Data, Acc) ->
    case Data of
        <<Val:4/?LI, R/binary>> ->
            parse_bin(Rest, R, [{Name, Val} | Acc]);
        _ ->
            {error, {truncated_data, Name}}
    end;
parse_bin([{Name, int64} | Rest], Data, Acc) ->
    case Data of
        <<Val:8/?LI, R/binary>> ->
            parse_bin(Rest, R, [{Name, Val} | Acc]);
        _ ->
            {error, {truncated_data, Name}}
    end;
parse_bin([{Name, cstring} | Rest], Data, Acc) ->
    case parse_cstring(Rest, 0) of
        {Val, R} ->
            parse_bin(Rest, R, [{Name, Val} | Acc]);
        _ ->
            {error, {unterminated_cstring, Name}}
    end;
parse_bin([{Name, doc} | Rest], Data, Acc) ->
    case mango_bson:to_ejson(Rest, [return_rest]) of
        {ok, Value, R} ->
            parse_bin(Rest, R, [{Name, Val} | Acc]);
        {error, Reason} ->
            {error, {invalid_bson, Reason}}
    end;
parse_bin([{Name, docs}], Data, Acc)
    % This has to be the last element of the message
    % so we assert that in the pattern match.
    case parse_docs(Data) of
        {ok, Docs} ->
            {ok, lists:reverse(Acc, [{docs, Docs}]), <<>>};
        Error ->
            Error
    end;
parse_bin([{Name, cursors}], Data, Acc) ->
    % This has to be the last element of the message
    % so we assert that in the pattern match.
    case Data of
        <<Num:4/?LI, Rest/binary>> ->
            case parse_cursors(Num, Rest, []) of
                {ok, Cursors} ->
                    {ok, lists:reverse(Acc, [{Name, Cursors}]), <<>>};
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
    case mongo_bson:to_ejson(Data, [return_rest]) of
        {ok, Doc, R} ->
            parse_docs(R, [Doc | Acc]);
        {error, Reason} ->
            {error, {invalid_bson, Reason}}
    end.


parse_cursors(0, <<>>, Acc) ->
    {ok, lists:reverse(Acc)};
parse_cursors(N, <<Cursor:8/little-integer, Rest/binary>>, Acc) ->
    parse_cursors(N-1, Rest, [Cursor | Acc]);
parse_cursors(_, _, _) ->
    {error, truncated_cursors}.
