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
-export([att_foldl/3,range_att_foldl/5,att_foldl_decode/3,get_validate_doc_fun/1]).
-export([from_json_obj/1,to_json_obj/2,has_stubs/1, merge_stubs/2]).
-export([validate_docid/1]).
-export([doc_from_multi_part_stream/2]).
-export([doc_to_multi_part_stream/5, len_doc_to_multi_part_stream/4]).
-export([abort_multi_part_stream/1]).
-export([to_path/1]).
-export([mp_parse_doc/2]).
-export([with_ejson_body/1]).

-include("couch_db.hrl").

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
    lists:map(
        fun({revs_info, Start, RevsInfo}) ->
            {JsonRevsInfo, _Pos}  = lists:mapfoldl(
                fun({RevId, Status}, PosAcc) ->
                    JsonObj = {[{<<"rev">>, rev_to_str({PosAcc, RevId})},
                        {<<"status">>, ?l2b(atom_to_list(Status))}]},
                    {JsonObj, PosAcc - 1}
                end, Start, RevsInfo),
            {<<"_revs_info">>, JsonRevsInfo};
        ({local_seq, Seq}) ->
            {<<"_local_seq">>, Seq};
        ({conflicts, Conflicts}) ->
            {<<"_conflicts">>, revs_to_strs(Conflicts)};
        ({deleted_conflicts, DConflicts}) ->
            {<<"_deleted_conflicts">>, revs_to_strs(DConflicts)}
        end, Meta).

to_json_attachments(Attachments, Options) ->
    to_json_attachments(
        Attachments,
        lists:member(attachments, Options),
        lists:member(follows, Options),
        lists:member(att_encoding_info, Options)
    ).

to_json_attachments([], _OutputData, _DataToFollow, _ShowEncInfo) ->
    [];
to_json_attachments(Atts, OutputData, DataToFollow, ShowEncInfo) ->
    AttProps = lists:map(
        fun(#att{disk_len=DiskLen, att_len=AttLen, encoding=Enc}=Att) ->
            {Att#att.name, {[
                {<<"content_type">>, Att#att.type},
                {<<"revpos">>, Att#att.revpos}] ++
                case Att#att.md5 of
                    <<>> ->
                        [];
                    Md5 ->
                        EncodedMd5 = base64:encode(Md5),
                        [{<<"digest">>, <<"md5-",EncodedMd5/binary>>}]
                end ++
                if not OutputData orelse Att#att.data == stub ->
                    [{<<"length">>, DiskLen}, {<<"stub">>, true}];
                true ->
                    if DataToFollow ->
                        [{<<"length">>, DiskLen}, {<<"follows">>, true}];
                    true ->
                        AttData = case Enc of
                        gzip ->
                            zlib:gunzip(att_to_bin(Att));
                        identity ->
                            att_to_bin(Att)
                        end,
                        [{<<"data">>, base64:encode(AttData)}]
                    end
                end ++
                    case {ShowEncInfo, Enc} of
                    {false, _} ->
                        [];
                    {true, identity} ->
                        [];
                    {true, _} ->
                        [
                            {<<"encoding">>, couch_util:to_binary(Enc)},
                            {<<"encoded_length">>, AttLen}
                        ]
                    end
            }}
        end, Atts),
    [{<<"_attachments">>, {AttProps}}].

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
        {Pos, [$- | RevId]} -> {list_to_integer(Pos), parse_revid(RevId)};
        _Else -> throw({bad_request, <<"Invalid rev format">>})
    end;
parse_rev(_BadRev) ->
    throw({bad_request, <<"Invalid rev format">>}).

parse_revs([]) ->
    [];
parse_revs([Rev | Rest]) ->
    [parse_rev(Rev) | parse_revs(Rest)].


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
    ?LOG_DEBUG("Document id is not a string: ~p", [Id]),
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
    Atts = lists:map(fun({Name, {BinProps}}) ->
        Md5 = case couch_util:get_value(<<"digest">>, BinProps) of
            <<"md5-",EncodedMd5/binary>> ->
                base64:decode(EncodedMd5);
            _ ->
               <<>>
        end,
        case couch_util:get_value(<<"stub">>, BinProps) of
        true ->
            Type = couch_util:get_value(<<"content_type">>, BinProps),
            RevPos = couch_util:get_value(<<"revpos">>, BinProps, nil),
            DiskLen = couch_util:get_value(<<"length">>, BinProps),
            {Enc, EncLen} = att_encoding_info(BinProps),
            #att{name=Name, data=stub, type=Type, att_len=EncLen,
                disk_len=DiskLen, encoding=Enc, revpos=RevPos, md5=Md5};
        _ ->
            Type = couch_util:get_value(<<"content_type">>, BinProps,
                    ?DEFAULT_ATTACHMENT_CONTENT_TYPE),
            RevPos = couch_util:get_value(<<"revpos">>, BinProps, 0),
            case couch_util:get_value(<<"follows">>, BinProps) of
            true ->
                DiskLen = couch_util:get_value(<<"length">>, BinProps),
                {Enc, EncLen} = att_encoding_info(BinProps),
                #att{name=Name, data=follows, type=Type, encoding=Enc,
                    att_len=EncLen, disk_len=DiskLen, revpos=RevPos, md5=Md5};
            _ ->
                Value = couch_util:get_value(<<"data">>, BinProps),
                Bin = base64:decode(Value),
                LenBin = size(Bin),
                #att{name=Name, data=Bin, type=Type, att_len=LenBin,
                        disk_len=LenBin, revpos=RevPos}
            end
        end
    end, JsonBins),
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

att_encoding_info(BinProps) ->
    DiskLen = couch_util:get_value(<<"length">>, BinProps),
    case couch_util:get_value(<<"encoding">>, BinProps) of
    undefined ->
        {identity, DiskLen};
    Enc ->
        EncodedLen = couch_util:get_value(<<"encoded_length">>, BinProps, DiskLen),
        {list_to_existing_atom(?b2l(Enc)), EncodedLen}
    end.

to_doc_info(FullDocInfo) ->
    {DocInfo, _Path} = to_doc_info_path(FullDocInfo),
    DocInfo.

max_seq(Tree, UpdateSeq) ->
    FoldFun = fun({_Pos, _Key}, Value, _Type, MaxOldSeq) ->
        case Value of
            {_Deleted, _DiskPos, OldTreeSeq} ->
                % Older versions didn't track data sizes.
                erlang:max(MaxOldSeq, OldTreeSeq);
            {_Deleted, _DiskPos, OldTreeSeq, _Size} ->
                erlang:max(MaxOldSeq, OldTreeSeq);
            _ ->
                MaxOldSeq
        end
    end,
    couch_key_tree:fold(FoldFun, UpdateSeq, Tree).

to_doc_info_path(#full_doc_info{id=Id,rev_tree=Tree,update_seq=Seq}) ->
    RevInfosAndPath = [
        {#rev_info{
            deleted = element(1, LeafVal),
            body_sp = element(2, LeafVal),
            seq = element(3, LeafVal),
            rev = {Pos, RevId}
        }, Path} || {LeafVal, {Pos, [RevId | _]} = Path} <-
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
    {#doc_info{id=Id, high_seq=max_seq(Tree, Seq), revs=RevInfos}, WinPath}.




att_foldl(#att{data=Bin}, Fun, Acc) when is_binary(Bin) ->
    Fun(Bin, Acc);
att_foldl(#att{data={Fd,Sp},md5=Md5}, Fun, Acc) ->
    couch_stream:foldl(Fd, Sp, Md5, Fun, Acc);
att_foldl(#att{data=DataFun,att_len=Len}, Fun, Acc) when is_function(DataFun) ->
   fold_streamed_data(DataFun, Len, Fun, Acc).

range_att_foldl(#att{data={Fd,Sp}}, From, To, Fun, Acc) ->
   couch_stream:range_foldl(Fd, Sp, From, To, Fun, Acc).

att_foldl_decode(#att{data={Fd,Sp},md5=Md5,encoding=Enc}, Fun, Acc) ->
    couch_stream:foldl_decode(Fd, Sp, Md5, Enc, Fun, Acc);
att_foldl_decode(#att{data=Fun2,att_len=Len, encoding=identity}, Fun, Acc) ->
       fold_streamed_data(Fun2, Len, Fun, Acc).

att_to_bin(#att{data=Bin}) when is_binary(Bin) ->
    Bin;
att_to_bin(#att{data=Iolist}) when is_list(Iolist) ->
    iolist_to_binary(Iolist);
att_to_bin(#att{data={_Fd,_Sp}}=Att) ->
    iolist_to_binary(
        lists:reverse(att_foldl(
                Att,
                fun(Bin,Acc) -> [Bin|Acc] end,
                []
        ))
    );
att_to_bin(#att{data=DataFun, att_len=Len}) when is_function(DataFun)->
    iolist_to_binary(
        lists:reverse(fold_streamed_data(
            DataFun,
            Len,
            fun(Data, Acc) -> [Data | Acc] end,
            []
        ))
    ).

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
    has_stubs(Atts);
has_stubs([]) ->
    false;
has_stubs([#att{data=stub}|_]) ->
    true;
has_stubs([_Att|Rest]) ->
    has_stubs(Rest).

merge_stubs(#doc{id = Id}, nil) ->
    throw({missing_stub, <<"Previous revision missing for document ", Id/binary>>});
merge_stubs(#doc{id=Id,atts=MemBins}=StubsDoc, #doc{atts=DiskBins}) ->
    BinDict = dict:from_list([{Name, Att} || #att{name=Name}=Att <- DiskBins]),
    MergedBins = lists:map(
        fun(#att{name=Name, data=stub, revpos=StubRevPos}) ->
            case dict:find(Name, BinDict) of
            {ok, #att{revpos=DiskRevPos}=DiskAtt} 
                    when DiskRevPos == StubRevPos orelse StubRevPos == nil ->
                DiskAtt;
            _ ->
                throw({missing_stub,
                        <<"id:", Id/binary, ", name:", Name/binary>>})
            end;
        (Att) ->
            Att
        end, MemBins),
    StubsDoc#doc{atts= MergedBins}.

fold_streamed_data(_RcvFun, 0, _Fun, Acc) ->
    Acc;
fold_streamed_data(RcvFun, LenLeft, Fun, Acc) when LenLeft > 0->
    Bin = RcvFun(),
    ResultAcc = Fun(Bin, Acc),
    fold_streamed_data(RcvFun, LenLeft - size(Bin), Fun, ResultAcc).

len_doc_to_multi_part_stream(Boundary, JsonBytes, Atts, SendEncodedAtts) ->
    AttsSize = lists:foldl(fun(#att{data=Data} = Att, AccAttsSize) ->
            case Data of
            stub ->
                AccAttsSize;
            _ ->
                AccAttsSize +
                4 + % "\r\n\r\n"
                case SendEncodedAtts of
                true ->
                    Att#att.att_len;
                _ ->
                    Att#att.disk_len
                end +
                4 + % "\r\n--"
                size(Boundary)
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
    case lists:any(fun(#att{data=Data})-> Data /= stub end, Atts) of
    true ->
        WriteFun([<<"--", Boundary/binary,
                "\r\ncontent-type: application/json\r\n\r\n">>,
                JsonBytes, <<"\r\n--", Boundary/binary>>]),
        atts_to_mp(Atts, Boundary, WriteFun, SendEncodedAtts);
    false ->
        WriteFun(JsonBytes)
    end.

atts_to_mp([], _Boundary, WriteFun, _SendEncAtts) ->
    WriteFun(<<"--">>);
atts_to_mp([#att{data=stub} | RestAtts], Boundary, WriteFun,
        SendEncodedAtts) ->
    atts_to_mp(RestAtts, Boundary, WriteFun, SendEncodedAtts);
atts_to_mp([Att | RestAtts], Boundary, WriteFun,
        SendEncodedAtts)  ->
    WriteFun(<<"\r\n\r\n">>),
    AttFun = case SendEncodedAtts of
    false ->
        fun att_foldl_decode/3;
    true ->
        fun att_foldl/3
    end,
    AttFun(Att, fun(Data, _) -> WriteFun(Data) end, ok),
    WriteFun(<<"\r\n--", Boundary/binary>>),
    atts_to_mp(RestAtts, Boundary, WriteFun, SendEncodedAtts).


doc_from_multi_part_stream(ContentType, DataFun) ->
    Parent = self(),
    Parser = spawn_link(fun() ->
        {<<"--">>, _, _} = couch_httpd:parse_multipart_request(
            ContentType, DataFun,
            fun(Next) -> mp_parse_doc(Next, []) end),
        unlink(Parent),
        Parent ! {self(), finished}
        end),
    Ref = make_ref(),
    Parser ! {get_doc_bytes, Ref, self()},
    receive 
    {doc_bytes, Ref, DocBytes} ->
        Doc = from_json_obj(?JSON_DECODE(DocBytes)),
        % go through the attachments looking for 'follows' in the data,
        % replace with function that reads the data from MIME stream.
        ReadAttachmentDataFun = fun() ->
            Parser ! {get_bytes, Ref, self()},
            receive {bytes, Ref, Bytes} -> Bytes end
        end,
        Atts2 = lists:map(
            fun(#att{data=follows}=A) ->
                A#att{data=ReadAttachmentDataFun};
            (A) ->
                A
            end, Doc#doc.atts),
        WaitFun = fun() ->
            receive {Parser, finished} -> ok end,
            erlang:put(mochiweb_request_recv, true)
        end,
        {ok, Doc#doc{atts=Atts2}, WaitFun, Parser}
    end.

mp_parse_doc({headers, H}, []) ->
    case couch_util:get_value("content-type", H) of
    {"application/json", _} ->
        fun (Next) ->
            mp_parse_doc(Next, [])
        end
    end;
mp_parse_doc({body, Bytes}, AccBytes) ->
    fun (Next) ->
        mp_parse_doc(Next, [Bytes | AccBytes])
    end;
mp_parse_doc(body_end, AccBytes) ->
    receive {get_doc_bytes, Ref, From} ->
        From ! {doc_bytes, Ref, lists:reverse(AccBytes)}
    end,
    fun mp_parse_atts/1.

mp_parse_atts(eof) ->
    ok;
mp_parse_atts({headers, _H}) ->
    fun mp_parse_atts/1;
mp_parse_atts({body, Bytes}) ->
    receive {get_bytes, Ref, From} ->
        From ! {bytes, Ref, Bytes}
    end,
    fun mp_parse_atts/1;
mp_parse_atts(body_end) ->
    fun mp_parse_atts/1.


abort_multi_part_stream(Parser) ->
    abort_multi_part_stream(Parser, erlang:monitor(process, Parser)).

abort_multi_part_stream(Parser, MonRef) ->
    case is_process_alive(Parser) of
    true ->
        Parser ! {get_bytes, nil, self()},
        receive
        {bytes, nil, _Bytes} ->
             abort_multi_part_stream(Parser, MonRef);
        {'DOWN', MonRef, _, _, _} ->
             ok
        end;
    false ->
        erlang:demonitor(MonRef, [flush])
    end.


with_ejson_body(#doc{body = Body} = Doc) when is_binary(Body) ->
    Doc#doc{body = couch_compress:decompress(Body)};
with_ejson_body(#doc{body = {_}} = Doc) ->
    Doc.
