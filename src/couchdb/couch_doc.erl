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
-export([att_foldl/3,get_validate_doc_fun/1]).
-export([from_json_obj/1,to_json_obj/2,has_stubs/1, merge_stubs/2]).
-export([validate_docid/1]).
-export([doc_from_multi_part_stream/2]).
-export([doc_to_multi_part_stream/5, len_doc_to_multi_part_stream/4]).

-include("couch_db.hrl").

% helpers used by to_json_obj
to_json_rev(0, []) ->
    [];
to_json_rev(Start, [FirstRevId|_]) ->
    [{<<"_rev">>, ?l2b([integer_to_list(Start),"-",revid_to_str(FirstRevId)])}].

to_json_body(true, _Body) ->
    [{<<"_deleted">>, true}];
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
    case lists:member(attachments, Options) of
    true -> % return all the binaries
        to_json_attachments(Attachments, 0, lists:member(follows, Options));
    false ->
        % note the default is [], because this sorts higher than all numbers.
        % and will return all the binaries.
        RevPos = proplists:get_value(atts_after_revpos, Options, []),
        to_json_attachments(Attachments, RevPos, lists:member(follows, Options))
    end.

to_json_attachments([], _RevPosIncludeAfter, _DataToFollow) ->
    [];
to_json_attachments(Atts, RevPosIncludeAfter, DataToFollow) ->
    AttProps = lists:map(
        fun(#att{len=Len}=Att) ->
            {Att#att.name, {[
                {<<"content_type">>, Att#att.type},
                {<<"revpos">>, Att#att.revpos}
                ] ++
                if Att#att.revpos > RevPosIncludeAfter ->    
                    if DataToFollow ->
                        [{<<"length">>, Len}, {<<"follows">>, true}];
                    true ->
                        [{<<"data">>, 
                            couch_util:encodeBase64(att_to_iolist(Att))}]
                    end;
                true ->
                    [{<<"length">>, Len}, {<<"stub">>, true}]
                end
            }}
        end, Atts),
    [{<<"_attachments">>, {AttProps}}].

to_json_obj(#doc{id=Id,deleted=Del,body=Body,revs={Start, RevIds},
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
        case proplists:get_value(<<"stub">>, BinProps) of
        true ->
            Type = proplists:get_value(<<"content_type">>, BinProps),
            Length = proplists:get_value(<<"length">>, BinProps),
            RevPos = proplists:get_value(<<"revpos">>, BinProps, 0),
            #att{name=Name, data=stub, type=Type, len=Length, revpos=RevPos};
        _ ->
            Type = proplists:get_value(<<"content_type">>, BinProps,
                    ?DEFAULT_ATTACHMENT_CONTENT_TYPE),
            RevPos = proplists:get_value(<<"revpos">>, BinProps, 0),
            case proplists:get_value(<<"follows">>, BinProps) of
            true ->
                #att{name=Name, data=follows, type=Type, 
                    len=proplists:get_value(<<"length">>, BinProps),
                        revpos=RevPos};
            _ ->
                Value = proplists:get_value(<<"data">>, BinProps),
                Bin = couch_util:decodeBase64(Value),
                #att{name=Name, data=Bin, type=Type, len=size(Bin),
                        revpos=RevPos}
            end
        end
    end, JsonBins),
    transfer_fields(Rest, Doc#doc{atts=Atts});

transfer_fields([{<<"_revisions">>, {Props}} | Rest], Doc) ->
    RevIds = proplists:get_value(<<"ids">>, Props),
    Start = proplists:get_value(<<"start">>, Props),
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

% unknown special field
transfer_fields([{<<"_",Name/binary>>, _} | _], _) ->
    throw({doc_validation,
            ?l2b(io_lib:format("Bad special document member: _~s", [Name]))});

transfer_fields([Field | Rest], #doc{body=Fields}=Doc) ->
    transfer_fields(Rest, Doc#doc{body=[Field|Fields]}).

to_doc_info(FullDocInfo) ->
    {DocInfo, _Path} = to_doc_info_path(FullDocInfo),
    DocInfo.

max_seq([], Max) ->
    Max;
max_seq([#rev_info{seq=Seq}|Rest], Max) ->
    max_seq(Rest, if Max > Seq -> Max; true -> Seq end).

to_doc_info_path(#full_doc_info{id=Id,rev_tree=Tree}) ->
    RevInfosAndPath =
        [{#rev_info{deleted=Del,body_sp=Bp,seq=Seq,rev={Pos,RevId}}, Path} ||
            {{Del, Bp, Seq},{Pos, [RevId|_]}=Path} <-
            couch_key_tree:get_all_leafs(Tree)],
    SortedRevInfosAndPath = lists:sort(
            fun({#rev_info{deleted=DeletedA,rev=RevA}, _PathA},
                {#rev_info{deleted=DeletedB,rev=RevB}, _PathB}) ->
            % sort descending by {not deleted, rev}
            {not DeletedA, RevA} > {not DeletedB, RevB}
        end, RevInfosAndPath),
    [{_RevInfo, WinPath}|_] = SortedRevInfosAndPath,
    RevInfos = [RevInfo || {RevInfo, _Path} <- SortedRevInfosAndPath],
    {#doc_info{id=Id, high_seq=max_seq(RevInfos, 0), revs=RevInfos}, WinPath}.




att_foldl(#att{data=Bin}, Fun, Acc) when is_binary(Bin) ->
    Fun(Bin, Acc);
att_foldl(#att{data={Fd,Sp},len=Len}, Fun, Acc) when is_tuple(Sp) orelse Sp == null ->
    % 09 UPGRADE CODE
    couch_stream:old_foldl(Fd, Sp, Len, Fun, Acc);
att_foldl(#att{data={Fd,Sp},md5=Md5}, Fun, Acc) ->
    couch_stream:foldl(Fd, Sp, Md5, Fun, Acc);
att_foldl(#att{data=DataFun,len=Len}, Fun, Acc) when is_function(DataFun) ->
   fold_streamed_data(DataFun, Len, Fun, Acc).


att_to_iolist(#att{data=Bin}) when is_binary(Bin) ->
    Bin;
att_to_iolist(#att{data=Iolist}) when is_list(Iolist) ->
    Iolist;
att_to_iolist(#att{data={Fd,Sp},md5=Md5}) ->
    lists:reverse(couch_stream:foldl(Fd, Sp, Md5, 
            fun(Bin,Acc) -> [Bin|Acc] end, []));
att_to_iolist(#att{data=DataFun, len=Len}) when is_function(DataFun)->
    lists:reverse(fold_streamed_data(DataFun, Len,
            fun(Data, Acc) -> [Data | Acc] end, [])).

get_validate_doc_fun(#doc{body={Props}}) ->
    Lang = proplists:get_value(<<"language">>, Props, <<"javascript">>),
    case proplists:get_value(<<"validate_doc_update">>, Props) of
    undefined ->
        nil;
    FunSrc ->
        fun(EditDoc, DiskDoc, Ctx) ->
            couch_query_servers:validate_doc_update(
                    Lang, FunSrc, EditDoc, DiskDoc, Ctx)
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

merge_stubs(#doc{id=Id,atts=MemBins}=StubsDoc, #doc{atts=DiskBins}) ->
    BinDict = dict:from_list([{Name, Att} || #att{name=Name}=Att <- DiskBins]),
    MergedBins = lists:map(
        fun(#att{name=Name, data=stub, revpos=RevPos}) ->
            case dict:find(Name, BinDict) of
            {ok, #att{revpos=RevPos}=DiskAtt} ->
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

len_doc_to_multi_part_stream(Boundary,JsonBytes,Atts,AttsSinceRevPos) ->
    2 + % "--"
    size(Boundary) +
    36 + % "\r\ncontent-type: application/json\r\n\r\n"
    iolist_size(JsonBytes) +
    4 + % "\r\n--"
    size(Boundary) +
    + lists:foldl(fun(#att{revpos=RevPos,len=Len}, AccAttsSize) ->
            if RevPos > AttsSinceRevPos ->
                AccAttsSize +  
                4 + % "\r\n\r\n"
                Len +
                4 + % "\r\n--"
                size(Boundary);
            true ->
                AccAttsSize
            end
        end, 0, Atts) +
    2. % "--"
    
doc_to_multi_part_stream(Boundary,JsonBytes,Atts,AttsSinceRevPos,WriteFun) ->
    WriteFun([<<"--", Boundary/binary,
            "\r\ncontent-type: application/json\r\n\r\n">>,
            JsonBytes, <<"\r\n--", Boundary/binary>>]),
    atts_to_mp(Atts, Boundary, WriteFun, AttsSinceRevPos).

atts_to_mp([], _Boundary, WriteFun, _AttsSinceRevPos) ->
    WriteFun(<<"--">>);
atts_to_mp([#att{revpos=RevPos} = Att | RestAtts], Boundary, WriteFun, 
        AttsSinceRevPos) when RevPos > AttsSinceRevPos ->
    WriteFun(<<"\r\n\r\n">>),
    att_foldl(Att, fun(Data, ok) -> WriteFun(Data) end, ok),
    WriteFun(<<"\r\n--", Boundary/binary>>),
    atts_to_mp(RestAtts, Boundary, WriteFun, AttsSinceRevPos);
atts_to_mp([_ | RestAtts], Boundary, WriteFun, AttsSinceRevPos) ->
    atts_to_mp(RestAtts, Boundary, WriteFun, AttsSinceRevPos).


doc_from_multi_part_stream(ContentType, DataFun) ->
    Parser = spawn_link(fun() -> 
        couch_httpd:parse_multipart_request(ContentType, DataFun,
                fun(Next)-> mp_parse_doc(Next, []) end)
        end),
    Parser ! {get_doc_bytes, self()},
    receive {doc_bytes, DocBytes} -> ok end,
    Doc = from_json_obj(?JSON_DECODE(DocBytes)),
    % go through the attachments looking for 'follows' in the data,
    % replace with function that reads the data from MIME stream.
    ReadAttachmentDataFun = fun() ->
        Parser ! {get_bytes, self()},
        receive {bytes, Bytes} -> Bytes end
    end,
    Atts2 = lists:map(
        fun(#att{data=follows}=A) ->
            A#att{data=ReadAttachmentDataFun};
        (A) ->
            A
        end, Doc#doc.atts),
    Doc#doc{atts=Atts2}.

mp_parse_doc({headers, H}, []) ->
    {"application/json", _} = proplists:get_value("content-type", H),
    fun (Next) ->
        mp_parse_doc(Next, [])
    end;
mp_parse_doc({body, Bytes}, AccBytes) ->
    fun (Next) ->
        mp_parse_doc(Next, [Bytes | AccBytes])
    end;
mp_parse_doc(body_end, AccBytes) ->
    receive {get_doc_bytes, From} ->
        From ! {doc_bytes, lists:reverse(AccBytes)}
    end,
    fun (Next) ->
        mp_parse_atts(Next)
    end.

mp_parse_atts(eof) ->
    ok;
mp_parse_atts({headers, _H}) ->
    fun (Next) ->
        mp_parse_atts(Next)
    end;
mp_parse_atts({body, Bytes}) ->
    receive {get_bytes, From} ->
        From ! {bytes, Bytes}
    end,
    fun (Next) ->
        mp_parse_atts(Next)
    end;
mp_parse_atts(body_end) ->
    fun (Next) ->
        mp_parse_atts(Next)
    end.


