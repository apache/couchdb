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

-module(couch_doc).

-export([to_doc_info/1,to_doc_info_path/1,parse_rev/1,parse_revs/1,rev_to_str/1,revs_to_strs/1]).
-export([from_json_obj/1,to_json_obj/2,has_stubs/1, merge_stubs/2]).
-export([validate_docid/1, get_validate_doc_fun/1]).
-export([doc_from_multi_part_stream/2, doc_from_multi_part_stream/3]).
-export([doc_to_multi_part_stream/5, len_doc_to_multi_part_stream/4]).
-export([abort_multi_part_stream/1, restart_open_doc_revs/3]).
-export([to_path/1]).
-export([mp_parse_doc/2]).
-export([with_ejson_body/1]).
-export([is_deleted/1]).
-export([num_mp_writers/1]).

-include_lib("couch/include/couch_db.hrl").

-spec to_path(#doc{}) -> path().
to_path(#doc{revs={Start, RevIds}}=Doc) ->
    [Branch] = to_branch(Doc, lists:reverse(RevIds)),
    {Start - length(RevIds) + 1, Branch}.

-spec to_branch(#doc{}, [RevId::binary()]) -> [branch()].
to_branch(Doc, [RevId]) ->
    [{RevId, Doc, []}];
to_branch(Doc, [RevId | Rest]) ->
    [{RevId, ?REV_MISSING, to_branch(Doc, Rest)}].

% helpers used by to_json_obj
to_json_rev(0, []) ->
    [];
to_json_rev(Start, [FirstRevId|_]) ->
    [{<<"_rev">>, ?l2b([integer_to_list(Start),"-",revid_to_str(FirstRevId)])}].

to_json_body(true, {Body}) ->
    Body ++ [{<<"_deleted">>, true}];
to_json_body(false, {Body}) ->
    Body.

to_json_revisions(Options, Start, RevIds) ->
    case lists:member(revs, Options) of
    false -> [];
    true ->
        [{<<"_revisions">>, {[{<<"start">>, Start},
                {<<"ids">>, [revid_to_str(R) ||R <- RevIds]}]}}]
    end.

revid_to_str(RevId) when size(RevId) =:= 16 ->
    ?l2b(couch_util:to_hex(RevId));
revid_to_str(RevId) ->
    RevId.

rev_to_str({Pos, RevId}) ->
    ?l2b([integer_to_list(Pos),"-",revid_to_str(RevId)]).


revs_to_strs([]) ->
    [];
revs_to_strs([{Pos, RevId}| Rest]) ->
    [rev_to_str({Pos, RevId}) | revs_to_strs(Rest)].

to_json_meta(Meta) ->
    lists:flatmap(
        fun({revs_info, Start, RevsInfo}) ->
            {JsonRevsInfo, _Pos}  = lists:mapfoldl(
                fun({RevId, Status}, PosAcc) ->
                    JsonObj = {[{<<"rev">>, rev_to_str({PosAcc, RevId})},
                        {<<"status">>, ?l2b(atom_to_list(Status))}]},
                    {JsonObj, PosAcc - 1}
                end, Start, RevsInfo),
            [{<<"_revs_info">>, JsonRevsInfo}];
        ({local_seq, Seq}) ->
            [{<<"_local_seq">>, Seq}];
        ({conflicts, Conflicts}) ->
            [{<<"_conflicts">>, revs_to_strs(Conflicts)}];
        ({deleted_conflicts, DConflicts}) ->
            [{<<"_deleted_conflicts">>, revs_to_strs(DConflicts)}];
        (_) ->
            []
        end, Meta).

to_json_attachments(Attachments, Options) ->
    to_json_attachments(
        Attachments,
        lists:member(attachments, Options),
        lists:member(follows, Options),
        lists:member(att_encoding_info, Options)
    ).

to_json_attachments([], _OutputData, _Follows, _ShowEnc) ->
    [];
to_json_attachments(Atts, OutputData, Follows, ShowEnc) ->
    Props = [couch_att:to_json(A, OutputData, Follows, ShowEnc) || A <- Atts],
    [{<<"_attachments">>, {Props}}].

to_json_obj(Doc, Options) ->
    doc_to_json_obj(with_ejson_body(Doc), Options).

doc_to_json_obj(#doc{id=Id,deleted=Del,body=Body,revs={Start, RevIds},
            meta=Meta}=Doc,Options)->
    {[{<<"_id">>, Id}]
        ++ to_json_rev(Start, RevIds)
        ++ to_json_body(Del, Body)
        ++ to_json_revisions(Options, Start, RevIds)
        ++ to_json_meta(Meta)
        ++ to_json_attachments(Doc#doc.atts, Options)
    }.

from_json_obj({Props}) ->
    transfer_fields(Props, #doc{body=[]});

from_json_obj(_Other) ->
    throw({bad_request, "Document must be a JSON object"}).

parse_revid(RevId) when size(RevId) =:= 32 ->
    RevInt = erlang:list_to_integer(?b2l(RevId), 16),
     <<RevInt:128>>;
parse_revid(RevId) when length(RevId) =:= 32 ->
    RevInt = erlang:list_to_integer(RevId, 16),
     <<RevInt:128>>;
parse_revid(RevId) when is_binary(RevId) ->
    RevId;
parse_revid(RevId) when is_list(RevId) ->
    ?l2b(RevId).


parse_rev(Rev) when is_binary(Rev) ->
    parse_rev(?b2l(Rev));
parse_rev(Rev) when is_list(Rev) ->
    SplitRev = lists:splitwith(fun($-) -> false; (_) -> true end, Rev),
    case SplitRev of
        {Pos, [$- | RevId]} ->
            IntPos = try list_to_integer(Pos) of
                Val -> Val
            catch
                error:badarg -> throw({bad_request, <<"Invalid rev format">>})
            end,
            {IntPos, parse_revid(RevId)};
        _Else -> throw({bad_request, <<"Invalid rev format">>})
    end;
parse_rev(_BadRev) ->
    throw({bad_request, <<"Invalid rev format">>}).

parse_revs([]) ->
    [];
parse_revs([Rev | Rest]) ->
    [parse_rev(Rev) | parse_revs(Rest)].


validate_docid(<<"">>) ->
    throw({bad_request, <<"Document id must not be empty">>});
validate_docid(Id) when is_binary(Id) ->
    case couch_util:validate_utf8(Id) of
        false -> throw({bad_request, <<"Document id must be valid UTF-8">>});
        true -> ok
    end,
    case Id of
    <<"_design/", _/binary>> -> ok;
    <<"_local/", _/binary>> -> ok;
    <<"_", _/binary>> ->
        throw({bad_request, <<"Only reserved document ids may start with underscore.">>});
    _Else -> ok
    end;
validate_docid(Id) ->
    couch_log:debug("Document id is not a string: ~p", [Id]),
    throw({bad_request, <<"Document id must be a string">>}).

transfer_fields([], #doc{body=Fields}=Doc) ->
    % convert fields back to json object
    Doc#doc{body={lists:reverse(Fields)}};

transfer_fields([{<<"_id">>, Id} | Rest], Doc) ->
    validate_docid(Id),
    transfer_fields(Rest, Doc#doc{id=Id});

transfer_fields([{<<"_rev">>, Rev} | Rest], #doc{revs={0, []}}=Doc) ->
    {Pos, RevId} = parse_rev(Rev),
    transfer_fields(Rest,
            Doc#doc{revs={Pos, [RevId]}});

transfer_fields([{<<"_rev">>, _Rev} | Rest], Doc) ->
    % we already got the rev from the _revisions
    transfer_fields(Rest,Doc);

transfer_fields([{<<"_attachments">>, {JsonBins}} | Rest], Doc) ->
    Atts = [couch_att:from_json(Name, Props) || {Name, {Props}} <- JsonBins],
    transfer_fields(Rest, Doc#doc{atts=Atts});

transfer_fields([{<<"_revisions">>, {Props}} | Rest], Doc) ->
    RevIds = couch_util:get_value(<<"ids">>, Props),
    Start = couch_util:get_value(<<"start">>, Props),
    if not is_integer(Start) ->
        throw({doc_validation, "_revisions.start isn't an integer."});
    not is_list(RevIds) ->
        throw({doc_validation, "_revisions.ids isn't a array."});
    true ->
        ok
    end,
    [throw({doc_validation, "RevId isn't a string"}) ||
            RevId <- RevIds, not is_binary(RevId)],
    RevIds2 = [parse_revid(RevId) || RevId <- RevIds],
    transfer_fields(Rest, Doc#doc{revs={Start, RevIds2}});

transfer_fields([{<<"_deleted">>, B} | Rest], Doc) when is_boolean(B) ->
    transfer_fields(Rest, Doc#doc{deleted=B});

% ignored fields
transfer_fields([{<<"_revs_info">>, _} | Rest], Doc) ->
    transfer_fields(Rest, Doc);
transfer_fields([{<<"_local_seq">>, _} | Rest], Doc) ->
    transfer_fields(Rest, Doc);
transfer_fields([{<<"_conflicts">>, _} | Rest], Doc) ->
    transfer_fields(Rest, Doc);
transfer_fields([{<<"_deleted_conflicts">>, _} | Rest], Doc) ->
    transfer_fields(Rest, Doc);

% special fields for replication documents
transfer_fields([{<<"_replication_state">>, _} = Field | Rest],
    #doc{body=Fields} = Doc) ->
    transfer_fields(Rest, Doc#doc{body=[Field|Fields]});
transfer_fields([{<<"_replication_state_time">>, _} = Field | Rest],
    #doc{body=Fields} = Doc) ->
    transfer_fields(Rest, Doc#doc{body=[Field|Fields]});
transfer_fields([{<<"_replication_state_reason">>, _} = Field | Rest],
    #doc{body=Fields} = Doc) ->
    transfer_fields(Rest, Doc#doc{body=[Field|Fields]});
transfer_fields([{<<"_replication_id">>, _} = Field | Rest],
    #doc{body=Fields} = Doc) ->
    transfer_fields(Rest, Doc#doc{body=[Field|Fields]});
transfer_fields([{<<"_replication_stats">>, _} = Field | Rest],
    #doc{body=Fields} = Doc) ->
    transfer_fields(Rest, Doc#doc{body=[Field|Fields]});

% unknown special field
transfer_fields([{<<"_",Name/binary>>, _} | _], _) ->
    throw({doc_validation,
            ?l2b(io_lib:format("Bad special document member: _~s", [Name]))});

transfer_fields([Field | Rest], #doc{body=Fields}=Doc) ->
    transfer_fields(Rest, Doc#doc{body=[Field|Fields]}).

to_doc_info(FullDocInfo) ->
    {DocInfo, _Path} = to_doc_info_path(FullDocInfo),
    DocInfo.

max_seq(Tree, UpdateSeq) ->
    FoldFun = fun({_Pos, _Key}, Value, _Type, MaxOldSeq) ->
        case Value of
            {_Deleted, _DiskPos, OldTreeSeq} ->
                % Older versions didn't track data sizes.
                erlang:max(MaxOldSeq, OldTreeSeq);
            {_Deleted, _DiskPos, OldTreeSeq, _Size} -> % necessary clause?
                % Older versions didn't store #leaf records.
                erlang:max(MaxOldSeq, OldTreeSeq);
            #leaf{seq=OldTreeSeq} ->
                erlang:max(MaxOldSeq, OldTreeSeq);
            _ ->
                MaxOldSeq
        end
    end,
    couch_key_tree:fold(FoldFun, UpdateSeq, Tree).

to_doc_info_path(#full_doc_info{id=Id,rev_tree=Tree,update_seq=FDISeq}) ->
    RevInfosAndPath = [
        {#rev_info{
            deleted = Leaf#leaf.deleted,
            body_sp = Leaf#leaf.ptr,
            seq = Leaf#leaf.seq,
            rev = {Pos, RevId}
        }, Path} || {Leaf, {Pos, [RevId | _]} = Path} <-
            couch_key_tree:get_all_leafs(Tree)
    ],
    SortedRevInfosAndPath = lists:sort(
            fun({#rev_info{deleted=DeletedA,rev=RevA}, _PathA},
                {#rev_info{deleted=DeletedB,rev=RevB}, _PathB}) ->
            % sort descending by {not deleted, rev}
            {not DeletedA, RevA} > {not DeletedB, RevB}
        end, RevInfosAndPath),
    [{_RevInfo, WinPath}|_] = SortedRevInfosAndPath,
    RevInfos = [RevInfo || {RevInfo, _Path} <- SortedRevInfosAndPath],
    {#doc_info{id=Id, high_seq=max_seq(Tree, FDISeq), revs=RevInfos}, WinPath}.


is_deleted(#full_doc_info{rev_tree=Tree}) ->
    is_deleted(Tree);
is_deleted(Tree) ->
    Leafs = couch_key_tree:get_all_leafs(Tree),
    try
        lists:foldl(fun
            ({#leaf{deleted=false},_}, _) ->
                throw(not_deleted);
            ({#doc{deleted=false},_}, _) ->
                throw(not_deleted);
            (_, Acc) ->
                Acc
        end, nil, Leafs),
        true
    catch throw:not_deleted ->
        false
    end.


get_validate_doc_fun({Props}) ->
    get_validate_doc_fun(couch_doc:from_json_obj({Props}));
get_validate_doc_fun(#doc{body={Props}}=DDoc) ->
    case couch_util:get_value(<<"validate_doc_update">>, Props) of
    undefined ->
        nil;
    _Else ->
        fun(EditDoc, DiskDoc, Ctx, SecObj) ->
            couch_query_servers:validate_doc_update(DDoc, EditDoc, DiskDoc, Ctx, SecObj)
        end
    end.


has_stubs(#doc{atts=Atts}) ->
    lists:any(fun couch_att:is_stub/1, Atts);
has_stubs(Atts) ->
    lists:any(fun couch_att:is_stub/1, Atts).

merge_stubs(#doc{id = Id}, nil) ->
    throw({missing_stub, <<"Previous revision missing for document ", Id/binary>>});
merge_stubs(#doc{id=Id,atts=MemBins}=StubsDoc, #doc{atts=DiskBins}) ->
    case couch_att:merge_stubs(MemBins, DiskBins) of
        {ok, MergedBins} ->
            StubsDoc#doc{atts = MergedBins};
        {missing, Name} ->
            throw({missing_stub,
                <<"Invalid attachment stub in ", Id/binary, " for ", Name/binary>>
            })
    end.

len_doc_to_multi_part_stream(Boundary, JsonBytes, Atts, SendEncodedAtts) ->
    AttsSize = lists:foldl(fun(Att, AccAttsSize) ->
            [Data, Name, AttLen, DiskLen, Type, Encoding] =
                 couch_att:fetch([data, name, att_len, disk_len, type, encoding], Att),
            case Data of
            stub ->
                AccAttsSize;
            _ ->
                AccAttsSize +
                4 + % "\r\n\r\n"
                case SendEncodedAtts of
                true ->
                    % header
                    length(integer_to_list(AttLen)) +
                    AttLen;
                _ ->
                    % header
                    length(integer_to_list(DiskLen)) +
                    DiskLen
                end +
                4 + % "\r\n--"
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
            end
        end, 0, Atts),
    if AttsSize == 0 ->
        {<<"application/json">>, iolist_size(JsonBytes)};
    true ->
        {<<"multipart/related; boundary=\"", Boundary/binary, "\"">>,
            2 + % "--"
            size(Boundary) +
            36 + % "\r\ncontent-type: application/json\r\n\r\n"
            iolist_size(JsonBytes) +
            4 + % "\r\n--"
            size(Boundary) +
            + AttsSize +
            2 % "--"
            }
    end.

doc_to_multi_part_stream(Boundary, JsonBytes, Atts, WriteFun,
    SendEncodedAtts) ->
    case lists:any(fun(Att)-> couch_att:fetch(data, Att) /= stub end, Atts) of
    true ->
        WriteFun([<<"--", Boundary/binary,
                "\r\nContent-Type: application/json\r\n\r\n">>,
                JsonBytes, <<"\r\n--", Boundary/binary>>]),
        atts_to_mp(Atts, Boundary, WriteFun, SendEncodedAtts);
    false ->
        WriteFun(JsonBytes)
    end.

atts_to_mp([], _Boundary, WriteFun, _SendEncAtts) ->
    WriteFun(<<"--">>);
atts_to_mp([Att | RestAtts], Boundary, WriteFun, SendEncodedAtts)  ->
    case couch_att:is_stub(Att) of
        true ->
            atts_to_mp(RestAtts, Boundary, WriteFun, SendEncodedAtts);
        false ->
            [Name, AttLen, DiskLen, Type, Encoding] =
                couch_att:fetch([name, att_len, disk_len, type, encoding], Att),
            % write headers
            LengthBin = case SendEncodedAtts of
                true  -> list_to_binary(integer_to_list(AttLen));
                false -> list_to_binary(integer_to_list(DiskLen))
            end,
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
            AttFun = case SendEncodedAtts of
                false -> fun couch_att:foldl_decode/3;
                true  -> fun couch_att:foldl/3
            end,
            AttFun(Att, fun(Data, _) -> WriteFun(Data) end, ok),
            WriteFun(<<"\r\n--", Boundary/binary>>),
            atts_to_mp(RestAtts, Boundary, WriteFun, SendEncodedAtts)
    end.


doc_from_multi_part_stream(ContentType, DataFun) ->
    doc_from_multi_part_stream(ContentType, DataFun, make_ref()).


doc_from_multi_part_stream(ContentType, DataFun, Ref) ->
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
        restart_open_doc_revs(Parser, Ref, NewRef);
    {doc_bytes, Ref, DocBytes} ->
        Doc = from_json_obj(?JSON_DECODE(DocBytes)),
        % we'll send the Parser process ID to the remote nodes so they can
        % retrieve their own copies of the attachment data
        WithParser = fun(follows) -> {follows, Parser, Ref}; (D) -> D end,
        Atts = [couch_att:transform(data, WithParser, A) || A <- Doc#doc.atts],
        WaitFun = fun() ->
            receive {'DOWN', ParserRef, _, _, _} -> ok end,
            erlang:put(mochiweb_request_recv, true)
        end,
        {ok, Doc#doc{atts=Atts}, WaitFun, Parser};
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


restart_open_doc_revs(Parser, Ref, NewRef) ->
    unlink(Parser),
    exit(Parser, kill),
    flush_parser_messages(Ref),
    erlang:error({restart_open_doc_revs, NewRef}).


flush_parser_messages(Ref) ->
    receive
        {headers, Ref, _} ->
            flush_parser_messages(Ref);
        {body_bytes, Ref, _} ->
            flush_parser_messages(Ref);
        {body_done, Ref} ->
            flush_parser_messages(Ref);
        {done, Ref} ->
            flush_parser_messages(Ref)
    after 0 ->
        ok
    end.


with_ejson_body(#doc{body = Body} = Doc) when is_binary(Body) ->
    Doc#doc{body = couch_compress:decompress(Body)};
with_ejson_body(#doc{body = {_}} = Doc) ->
    Doc.
