% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(couch_httpd_multipart).

-export([
    abort_multipart_stream/1,
    decode_multipart_stream/3,
    encode_multipart_stream/5,
    length_multipart_stream/3,
    num_mp_writers/0,
    num_mp_writers/1
]).

-include_lib("couch/include/couch_db.hrl").

decode_multipart_stream(ContentType, DataFun, Ref) ->
    Parent = self(),
    NumMpWriters = num_mp_writers(),
    {Parser, ParserRef} = spawn_monitor(fun() ->
        ParentRef = erlang:monitor(process, Parent),
        put(mp_parent_ref, ParentRef),
        num_mp_writers(NumMpWriters),
        {<<"--", _/binary>>, _, _} = couch_httpd:parse_multipart_request(
            ContentType,
            DataFun,
            fun(Next) -> mp_parse_doc(Next, []) end
        ),
        unlink(Parent)
    end),
    Parser ! {get_doc_bytes, Ref, self()},
    receive
        {started_open_doc_revs, NewRef} ->
            %% FIXME: How to remove the knowledge about this message?
            {{started_open_doc_revs, NewRef}, Parser, ParserRef};
        {doc_bytes, Ref, DocBytes} ->
            {{doc_bytes, Ref, DocBytes}, Parser, ParserRef};
        {'DOWN', ParserRef, _, _, normal} ->
            ok;
        {'DOWN', ParserRef, process, Parser, {{nocatch, {Error, Msg}}, _}} ->
            couch_log:error(
                "Multipart streamer ~p died with reason ~p",
                [ParserRef, Msg]
            ),
            throw({Error, Msg});
        {'DOWN', ParserRef, _, _, Reason} ->
            couch_log:error(
                "Multipart streamer ~p died with reason ~p",
                [ParserRef, Reason]
            ),
            throw({error, Reason})
    end.

mp_parse_doc({headers, H}, []) ->
    case couch_util:get_value("content-type", H) of
        {"application/json", _} ->
            fun(Next) ->
                mp_parse_doc(Next, [])
            end;
        _ ->
            throw({bad_ctype, <<"Content-Type must be application/json">>})
    end;
mp_parse_doc({body, Bytes}, AccBytes) ->
    fun(Next) ->
        mp_parse_doc(Next, [Bytes | AccBytes])
    end;
mp_parse_doc(body_end, AccBytes) ->
    receive
        {get_doc_bytes, Ref, From} ->
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
    case maybe_send_data({Ref, Chunks ++ [Bytes], Offset, Counters, Waiting}) of
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
                {hello_from_writer, Ref, WriterPid} ->
                    NewCounters = handle_hello(WriterPid, Counters),
                    mp_parse_atts(eof, {Ref, Chunks, Offset, NewCounters, Waiting});
                {get_bytes, Ref, From} ->
                    C2 = update_writer(From, Counters),
                    case maybe_send_data({Ref, Chunks, Offset, C2, [From | Waiting]}) of
                        abort_parsing ->
                            ok;
                        NewAcc ->
                            mp_parse_atts(eof, NewAcc)
                    end;
                {'DOWN', ParentRef, _, _, _} ->
                    exit(mp_reader_coordinator_died);
                {'DOWN', WriterRef, _, WriterPid, _} ->
                    case remove_writer(WriterPid, WriterRef, Counters) of
                        abort_parsing ->
                            ok;
                        C2 ->
                            NewAcc = {Ref, Chunks, Offset, C2, Waiting -- [WriterPid]},
                            mp_parse_atts(eof, NewAcc)
                    end
            after att_writer_timeout() ->
                ok
            end
    end.

mp_abort_parse_atts(eof, _) ->
    ok;
mp_abort_parse_atts(_, _) ->
    fun(Next) -> mp_abort_parse_atts(Next, nil) end.

maybe_send_data({Ref, Chunks, Offset, Counters, Waiting}) ->
    receive
        {hello_from_writer, Ref, WriterPid} ->
            NewCounters = handle_hello(WriterPid, Counters),
            maybe_send_data({Ref, Chunks, Offset, NewCounters, Waiting});
        {get_bytes, Ref, From} ->
            NewCounters = update_writer(From, Counters),
            maybe_send_data({Ref, Chunks, Offset, NewCounters, [From | Waiting]})
    after 0 ->
        % reply to as many writers as possible
        NewWaiting = lists:filter(
            fun(Writer) ->
                {_, WhichChunk} = orddict:fetch(Writer, Counters),
                ListIndex = WhichChunk - Offset,
                if
                    ListIndex =< length(Chunks) ->
                        Writer ! {bytes, Ref, lists:nth(ListIndex, Chunks)},
                        false;
                    true ->
                        true
                end
            end,
            Waiting
        ),

        % check if we can drop a chunk from the head of the list
        SmallestIndex =
            case Counters of
                [] ->
                    0;
                _ ->
                    lists:min([C || {_WPid, {_WRef, C}} <- Counters])
            end,
        Size = length(Counters),
        N = num_mp_writers(),
        if
            Size == N andalso SmallestIndex == (Offset + 1) ->
                NewChunks = tl(Chunks),
                NewOffset = Offset + 1;
            true ->
                NewChunks = Chunks,
                NewOffset = Offset
        end,

        % we should wait for a writer if no one has written the last chunk
        LargestIndex = lists:max([0] ++ [C || {_WPid, {_WRef, C}} <- Counters]),
        if
            LargestIndex >= (Offset + length(Chunks)) ->
                % someone has written all possible chunks, keep moving
                {Ref, NewChunks, NewOffset, Counters, NewWaiting};
            true ->
                ParentRef = get(mp_parent_ref),
                receive
                    abort_parsing ->
                        abort_parsing;
                    {'DOWN', ParentRef, _, _, _} ->
                        exit(mp_reader_coordinator_died);
                    {'DOWN', WriterRef, _, WriterPid, _} ->
                        case remove_writer(WriterPid, WriterRef, Counters) of
                            abort_parsing ->
                                abort_parsing;
                            C2 ->
                                RestWaiting = NewWaiting -- [WriterPid],
                                NewAcc = {Ref, NewChunks, NewOffset, C2, RestWaiting},
                                maybe_send_data(NewAcc)
                        end;
                    {hello_from_writer, Ref, WriterPid} ->
                        C2 = handle_hello(WriterPid, Counters),
                        maybe_send_data({Ref, NewChunks, NewOffset, C2, NewWaiting});
                    {get_bytes, Ref, X} ->
                        C2 = update_writer(X, Counters),
                        maybe_send_data({Ref, NewChunks, NewOffset, C2, [X | NewWaiting]})
                after att_writer_timeout() ->
                    abort_parsing
                end
        end
    end.

handle_hello(WriterPid, Counters) ->
    WriterRef = erlang:monitor(process, WriterPid),
    orddict:store(WriterPid, {WriterRef, 0}, Counters).

update_writer(WriterPid, Counters) ->
    case orddict:find(WriterPid, Counters) of
        {ok, {WriterRef, Count}} ->
            orddict:store(WriterPid, {WriterRef, Count + 1}, Counters);
        error ->
            WriterRef = erlang:monitor(process, WriterPid),
            orddict:store(WriterPid, {WriterRef, 1}, Counters)
    end.

remove_writer(WriterPid, WriterRef, Counters) ->
    case orddict:find(WriterPid, Counters) of
        {ok, {WriterRef, _}} ->
            case num_mp_writers() of
                N when N > 1 ->
                    num_mp_writers(N - 1),
                    orddict:erase(WriterPid, Counters);
                _ ->
                    abort_parsing
            end;
        {ok, _} ->
            % We got a different ref fired for a known worker
            abort_parsing;
        error ->
            % Unknown worker pid?
            abort_parsing
    end.

num_mp_writers(N) ->
    erlang:put(mp_att_writers, N).

num_mp_writers() ->
    case erlang:get(mp_att_writers) of
        undefined -> 1;
        Count -> Count
    end.

att_writer_timeout() ->
    config:get_integer("couchdb", "attachment_writer_timeout", 300000).

encode_multipart_stream(_Boundary, JsonBytes, [], WriteFun, _AttFun) ->
    WriteFun(JsonBytes);
encode_multipart_stream(Boundary, JsonBytes, Atts, WriteFun, AttFun) ->
    WriteFun([
        <<"--", Boundary/binary, "\r\nContent-Type: application/json\r\n\r\n">>,
        JsonBytes,
        <<"\r\n--", Boundary/binary>>
    ]),
    atts_to_mp(Atts, Boundary, WriteFun, AttFun).

atts_to_mp([], _Boundary, WriteFun, _AttFun) ->
    WriteFun(<<"--">>);
atts_to_mp(
    [{Att, Name, Len, Type, Encoding} | RestAtts],
    Boundary,
    WriteFun,
    AttFun
) ->
    LengthBin = list_to_binary(integer_to_list(Len)),
    % write headers
    WriteFun(<<"\r\nContent-Disposition: attachment; filename=\"", Name/binary, "\"">>),
    WriteFun(<<"\r\nContent-Type: ", Type/binary>>),
    WriteFun(<<"\r\nContent-Length: ", LengthBin/binary>>),
    case Encoding of
        identity ->
            ok;
        _ ->
            EncodingBin = atom_to_binary(Encoding, latin1),
            WriteFun(<<"\r\nContent-Encoding: ", EncodingBin/binary>>)
    end,

    % write data
    WriteFun(<<"\r\n\r\n">>),
    AttFun(Att, fun(Data, _) -> WriteFun(Data) end, ok),
    WriteFun(<<"\r\n--", Boundary/binary>>),
    atts_to_mp(RestAtts, Boundary, WriteFun, AttFun).

length_multipart_stream(Boundary, JsonBytes, Atts) ->
    AttsSize = lists:foldl(
        fun({_Att, Name, Len, Type, Encoding}, AccAttsSize) ->
            AccAttsSize +
                % "\r\n\r\n"
                4 +
                length(integer_to_list(Len)) +
                Len +
                % "\r\n--"
                4 +
                size(Boundary) +
                % attachment headers
                % (the length of the Content-Length has already been set)
                size(Name) +
                size(Type) +
                length("\r\nContent-Disposition: attachment; filename=\"\"") +
                length("\r\nContent-Type: ") +
                length("\r\nContent-Length: ") +
                case Encoding of
                    identity ->
                        0;
                    _ ->
                        length(atom_to_list(Encoding)) +
                            length("\r\nContent-Encoding: ")
                end
        end,
        0,
        Atts
    ),
    if
        AttsSize == 0 ->
            {<<"application/json">>, iolist_size(JsonBytes)};
        true ->
            {
                <<"multipart/related; boundary=\"", Boundary/binary, "\"">>,
                % "--"
                2 +
                    size(Boundary) +
                    % "\r\ncontent-type: application/json\r\n\r\n"
                    36 +
                    iolist_size(JsonBytes) +
                    % "\r\n--"
                    4 +
                    size(Boundary) +
                    +AttsSize +
                    % "--"
                    2
            }
    end.

abort_multipart_stream(Parser) ->
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
