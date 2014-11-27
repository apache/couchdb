-module(couch_httpd_multipart).

-export([doc_from_multi_part_stream/2, doc_from_multi_part_stream/3]).

-export([abort_multi_part_stream/1]).

-include_lib("couch/include/couch_db.hrl").

doc_from_multi_part_stream(ContentType, DataFun) ->
    doc_from_multi_part_stream(ContentType, DataFun, make_ref()).


doc_from_multi_part_stream(ContentType, DataFun, Ref) ->
    case parse_multipart_stream(ContentType, DataFun, Ref) of
    {{started_open_doc_revs, NewRef}, Parser, _ParserRef} ->
        couch_doc:restart_open_doc_revs(Parser, Ref, NewRef);
    {{doc_bytes, Ref, DocBytes}, Parser, ParserRef} ->
        Doc = couch_doc:from_json_obj(?JSON_DECODE(DocBytes)),
        % we'll send the Parser process ID to the remote nodes so they can
        % retrieve their own copies of the attachment data
        WithParser = fun(follows) -> {follows, Parser, Ref}; (D) -> D end,
        Atts = [couch_att:transform(data, WithParser, A) || A <- Doc#doc.atts],
        WaitFun = fun() ->
            receive {'DOWN', ParserRef, _, _, _} -> ok end,
            erlang:put(mochiweb_request_recv, true)
        end,
        {ok, Doc#doc{atts=Atts}, WaitFun, Parser};
    ok -> ok
    end.

parse_multipart_stream(ContentType, DataFun, Ref) ->
    Parent = self(),
    NumMpWriters = num_mp_writers(),
    {Parser, ParserRef} = spawn_monitor(fun() ->
        ParentRef = erlang:monitor(process, Parent),
        put(mp_parent_ref, ParentRef),
        num_mp_writers(NumMpWriters),
        {<<"--",_/binary>>, _, _} = couch_httpd:parse_multipart_request(
            ContentType, DataFun,
            fun(Next) -> mp_parse_doc(Next, []) end),
        unlink(Parent)
        end),
    Parser ! {get_doc_bytes, Ref, self()},
    receive
    {started_open_doc_revs, NewRef} ->
        %% FIXME: How to remove the knowledge about this message?
        {{started_open_doc_revs, NewRef}, Parser, ParserRef};
    {doc_bytes, Ref, DocBytes}  ->
        {{doc_bytes, Ref, DocBytes}, Parser, ParserRef};
    {'DOWN', ParserRef, _, _, normal} ->
        ok;
    {'DOWN', ParserRef, process, Parser, {{nocatch, {Error, Msg}}, _}} ->
        couch_log:error("Multipart streamer ~p died with reason ~p",
                        [ParserRef, Msg]),
        throw({Error, Msg});
    {'DOWN', ParserRef, _, _, Reason} ->
        couch_log:error("Multipart streamer ~p died with reason ~p",
                        [ParserRef, Reason]),
        throw({error, Reason})
    end.


mp_parse_doc({headers, H}, []) ->
    case couch_util:get_value("content-type", H) of
    {"application/json", _} ->
        fun (Next) ->
            mp_parse_doc(Next, [])
        end;
    _ ->
        throw({bad_ctype, <<"Content-Type must be application/json">>})
    end;
mp_parse_doc({body, Bytes}, AccBytes) ->
    fun (Next) ->
        mp_parse_doc(Next, [Bytes | AccBytes])
    end;
mp_parse_doc(body_end, AccBytes) ->
    receive {get_doc_bytes, Ref, From} ->
        From ! {doc_bytes, Ref, lists:reverse(AccBytes)}
    end,
    fun(Next) ->
        mp_parse_atts(Next, {Ref, [], 0, orddict:new(), []})
    end.

mp_parse_atts({headers, _}, Acc) ->
    fun(Next) -> mp_parse_atts(Next, Acc) end;
mp_parse_atts(body_end, Acc) ->
    fun(Next) -> mp_parse_atts(Next, Acc) end;
mp_parse_atts({body, Bytes}, {Ref, Chunks, Offset, Counters, Waiting}) ->
    case maybe_send_data({Ref, Chunks++[Bytes], Offset, Counters, Waiting}) of
        abort_parsing ->
            fun(Next) -> mp_abort_parse_atts(Next, nil) end;
        NewAcc ->
            fun(Next) -> mp_parse_atts(Next, NewAcc) end
    end;
mp_parse_atts(eof, {Ref, Chunks, Offset, Counters, Waiting}) ->
    N = num_mp_writers(),
    M = length(Counters),
    case (M == N) andalso Chunks == [] of
    true ->
        ok;
    false ->
        ParentRef = get(mp_parent_ref),
        receive
        abort_parsing ->
            ok;
        {get_bytes, Ref, From} ->
            C2 = orddict:update_counter(From, 1, Counters),
            NewAcc = maybe_send_data({Ref, Chunks, Offset, C2, [From|Waiting]}),
            mp_parse_atts(eof, NewAcc);
        {'DOWN', ParentRef, _, _, _} ->
            exit(mp_reader_coordinator_died)
        after 3600000 ->
            ok
        end
    end.

mp_abort_parse_atts(eof, _) ->
    ok;
mp_abort_parse_atts(_, _) ->
    fun(Next) -> mp_abort_parse_atts(Next, nil) end.

maybe_send_data({Ref, Chunks, Offset, Counters, Waiting}) ->
    receive {get_bytes, Ref, From} ->
        NewCounters = orddict:update_counter(From, 1, Counters),
        maybe_send_data({Ref, Chunks, Offset, NewCounters, [From|Waiting]})
    after 0 ->
        % reply to as many writers as possible
        NewWaiting = lists:filter(fun(Writer) ->
            WhichChunk = orddict:fetch(Writer, Counters),
            ListIndex = WhichChunk - Offset,
            if ListIndex =< length(Chunks) ->
                Writer ! {bytes, Ref, lists:nth(ListIndex, Chunks)},
                false;
            true ->
                true
            end
        end, Waiting),

        % check if we can drop a chunk from the head of the list
        case Counters of
        [] ->
            SmallestIndex = 0;
        _ ->
            SmallestIndex = lists:min(element(2, lists:unzip(Counters)))
        end,
        Size = length(Counters),
        N = num_mp_writers(),
        if Size == N andalso SmallestIndex == (Offset+1) ->
            NewChunks = tl(Chunks),
            NewOffset = Offset+1;
        true ->
            NewChunks = Chunks,
            NewOffset = Offset
        end,

        % we should wait for a writer if no one has written the last chunk
        LargestIndex = lists:max([0|element(2, lists:unzip(Counters))]),
        if LargestIndex  >= (Offset + length(Chunks)) ->
            % someone has written all possible chunks, keep moving
            {Ref, NewChunks, NewOffset, Counters, NewWaiting};
        true ->
            ParentRef = get(mp_parent_ref),
            receive
            abort_parsing ->
                abort_parsing;
            {'DOWN', ParentRef, _, _, _} ->
                exit(mp_reader_coordinator_died);
            {get_bytes, Ref, X} ->
                C2 = orddict:update_counter(X, 1, Counters),
                maybe_send_data({Ref, NewChunks, NewOffset, C2, [X|NewWaiting]})
            end
        end
    end.


num_mp_writers(N) ->
    erlang:put(mp_att_writers, N).


num_mp_writers() ->
    case erlang:get(mp_att_writers) of
        undefined -> 1;
        Count -> Count
    end.


abort_multi_part_stream(Parser) ->
    MonRef = erlang:monitor(process, Parser),
    Parser ! abort_parsing,
    receive
        {'DOWN', MonRef, _, _, _} -> ok
    after 60000 ->
        % One minute is quite on purpose for this timeout. We
        % want to try and read data to keep the socket open
        % when possible but we also don't want to just make
        % this a super long timeout because people have to
        % wait this long to see if they just had an error
        % like a validate_doc_update failure.
        throw(multi_part_abort_timeout)
    end.
