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
-export([from_json_obj/1, from_json_obj_validate/1, to_json_obj/2,has_stubs/1, merge_stubs/2]).
-export([validate_docid/1, get_validate_doc_fun/1]).
-export([doc_from_multi_part_stream/2, doc_from_multi_part_stream/3]).
-export([doc_from_multi_part_stream/4]).
-export([doc_to_multi_part_stream/5, len_doc_to_multi_part_stream/4]).
-export([restart_open_doc_revs/3]).
-export([to_path/1]).

-export([with_ejson_body/1]).
-export([is_deleted/1]).


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

to_json_revisions(Options, Start, RevIds0) ->
    RevIds = case proplists:get_value(revs, Options) of
        true ->
            RevIds0;
        Num when is_integer(Num), Num > 0 ->
            lists:sublist(RevIds0, Num);
        _ ->
           []
    end,
    if RevIds == [] -> []; true ->
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

from_json_obj_validate(EJson) ->
    MaxSize = config:get_integer("couchdb", "max_document_size", 4294967296),
    Doc = from_json_obj(EJson),
    case couch_ejson_size:encoded_size(Doc#doc.body) =< MaxSize of
        true ->
             validate_attachment_sizes(Doc#doc.atts),
             Doc;
        false ->
            throw({request_entity_too_large, Doc#doc.id})
    end.


validate_attachment_sizes([]) ->
    ok;
validate_attachment_sizes(Atts) ->
    MaxAttSize = couch_att:max_attachment_size(),
    lists:foreach(fun(Att) ->
         AttName = couch_att:fetch(name, Att),
         AttSize = couch_att:fetch(att_len, Att),
         couch_att:validate_attachment_size(AttName, AttSize, MaxAttSize)
    end, Atts).


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
    [parse_rev(Rev) | parse_revs(Rest)];
parse_revs(_) ->
    throw({bad_request, "Invalid list of revisions"}).


validate_docid(<<"">>) ->
    throw({illegal_docid, <<"Document id must not be empty">>});
validate_docid(<<"_design/">>) ->
    throw({illegal_docid, <<"Illegal document id `_design/`">>});
validate_docid(<<"_local/">>) ->
    throw({illegal_docid, <<"Illegal document id `_local/`">>});
validate_docid(Id) when is_binary(Id) ->
    MaxLen = case config:get("couchdb", "max_document_id_length", "infinity") of
        "infinity" -> infinity;
        IntegerVal -> list_to_integer(IntegerVal)
    end,
    case MaxLen > 0 andalso byte_size(Id) > MaxLen of
        true -> throw({illegal_docid, <<"Document id is too long">>});
        false -> ok
    end,
    case couch_util:validate_utf8(Id) of
        false -> throw({illegal_docid, <<"Document id must be valid UTF-8">>});
        true -> ok
    end,
    case Id of
    <<"_design/", _/binary>> -> ok;
    <<"_local/", _/binary>> -> ok;
    <<"_", _/binary>> ->
        case couch_db_plugin:validate_docid(Id) of
            true ->
                ok;
            false ->
                throw(
                  {illegal_docid,
                   <<"Only reserved document ids may start with underscore.">>})
        end;
    _Else -> ok
    end;
validate_docid(Id) ->
    couch_log:debug("Document id is not a string: ~p", [Id]),
    throw({illegal_docid, <<"Document id must be a string">>}).

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
        {rev_info(Node), Path} || {_Leaf, Path} = Node <-
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

rev_info({#leaf{} = Leaf, {Pos, [RevId | _]}}) ->
    #rev_info{
        deleted = Leaf#leaf.deleted,
        body_sp = Leaf#leaf.ptr,
        seq = Leaf#leaf.seq,
        rev = {Pos, RevId}
    };
rev_info({#doc{} = Doc, {Pos, [RevId | _]}}) ->
    #rev_info{
        deleted = Doc#doc.deleted,
        body_sp = undefined,
        seq = undefined,
        rev = {Pos, RevId}
    }.

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
    AttsToInclude = lists:filter(fun(Att) -> not couch_att:is_stub(Att) end, Atts),
    AttsDecoded = decode_attributes(AttsToInclude, SendEncodedAtts),
    couch_httpd_multipart:length_multipart_stream(Boundary, JsonBytes, AttsDecoded).


doc_to_multi_part_stream(Boundary, JsonBytes, Atts, WriteFun,
    SendEncodedAtts) ->
    AttsToInclude = lists:filter(fun(Att)-> couch_att:fetch(data, Att) /= stub end, Atts),
    AttsDecoded = decode_attributes(AttsToInclude, SendEncodedAtts),
    AttFun = case SendEncodedAtts of
        false -> fun couch_att:foldl_decode/3;
        true  -> fun couch_att:foldl/3
    end,
    couch_httpd_multipart:encode_multipart_stream(
      Boundary, JsonBytes, AttsDecoded, WriteFun, AttFun).

decode_attributes(Atts, SendEncodedAtts) ->
    lists:map(fun(Att) ->
        [Name, AttLen, DiskLen, Type, Encoding] =
           couch_att:fetch([name, att_len, disk_len, type, encoding], Att),
        Len = case SendEncodedAtts of
            true -> AttLen;
            false -> DiskLen
          end,
        {Att, Name, Len, Type, Encoding}
      end, Atts).

doc_from_multi_part_stream(ContentType, DataFun) ->
    doc_from_multi_part_stream(ContentType, DataFun, make_ref()).

doc_from_multi_part_stream(ContentType, DataFun, Ref) ->
    doc_from_multi_part_stream(ContentType, DataFun, Ref, true).

doc_from_multi_part_stream(ContentType, DataFun, Ref, ValidateDocLimits) ->
    case couch_httpd_multipart:decode_multipart_stream(ContentType, DataFun, Ref) of
    {{started_open_doc_revs, NewRef}, Parser, _ParserRef} ->
        restart_open_doc_revs(Parser, Ref, NewRef);
    {{doc_bytes, Ref, DocBytes}, Parser, ParserRef} ->
        Doc = case ValidateDocLimits of
            true ->
                from_json_obj_validate(?JSON_DECODE(DocBytes));
            false ->
                from_json_obj(?JSON_DECODE(DocBytes))
        end,
        erlang:put(mochiweb_request_recv, true),
        % we'll send the Parser process ID to the remote nodes so they can
        % retrieve their own copies of the attachment data
        WithParser = fun(follows) -> {follows, Parser, Ref}; (D) -> D end,
        Atts = [couch_att:transform(data, WithParser, A) || A <- Doc#doc.atts],
        WaitFun = fun() ->
            receive {'DOWN', ParserRef, _, _, _} -> ok end
        end,
        {ok, Doc#doc{atts=Atts}, WaitFun, Parser};
    ok -> ok
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
