% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License.  You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
% License for the specific language governing permissions and limitations under
% the License.

-module(couch_doc).

-export([to_doc_info/1,to_doc_info_path/1,parse_rev/1,parse_revs/1,rev_to_str/1,rev_to_strs/1]).
-export([bin_foldl/3,bin_size/1,bin_to_binary/1,get_validate_doc_fun/1]).
-export([from_json_obj/1,to_json_obj/2,has_stubs/1, merge_stubs/2]).
-export([validate_docid/1]).

-include("couch_db.hrl").

% helpers used by to_json_obj
to_json_rev(0, []) ->
    [];
to_json_rev(Start, [FirstRevId|_]) ->
    [{<<"_rev">>, ?l2b([integer_to_list(Start),"-",FirstRevId])}].

to_json_body(true, _Body) ->
    [{<<"_deleted">>, true}];
to_json_body(false, {Body}) ->
    Body.

to_json_revisions(Options, Start, RevIds) ->
    case lists:member(revs, Options) of
    false -> [];
    true ->
        [{<<"_revisions">>, {[{<<"start">>, Start}, 
                        {<<"ids">>, RevIds}]}}]
    end.

rev_to_str({Pos, RevId}) ->
    ?l2b([integer_to_list(Pos),"-",RevId]).

rev_to_strs([]) ->
    [];
rev_to_strs([{Pos, RevId}| Rest]) ->
    [rev_to_str({Pos, RevId}) | rev_to_strs(Rest)].

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
            {<<"_conflicts">>, rev_to_strs(Conflicts)};
        ({deleted_conflicts, DConflicts}) ->
            {<<"_deleted_conflicts">>, rev_to_strs(DConflicts)}
        end, Meta).

to_json_attachment_stubs(Attachments) ->
    BinProps = lists:map(
        fun({Name, {Type, {_RcvFun, Length}}}) ->
            {Name, {[
                {<<"stub">>, true},
                {<<"content_type">>, Type},
                {<<"length">>, Length}
            ]}};
        ({Name, {Type, BinValue}}) ->
            {Name, {[
                {<<"stub">>, true},
                {<<"content_type">>, Type},
                {<<"length">>, bin_size(BinValue)}
            ]}}
        end,
        Attachments),
    case BinProps of
        [] -> [];
        _ -> [{<<"_attachments">>, {BinProps}}]
    end.

to_json_attachments(Attachments) ->
    BinProps = lists:map(
        fun({Name, {Type, {RcvFun, Length}}}) ->
            Data = read_streamed_attachment(RcvFun, Length, _Acc = []),
            {Name, {[
                {<<"content_type">>, Type},
                {<<"data">>, couch_util:encodeBase64(Data)}
            ]}};
        ({Name, {Type, BinValue}}) ->
            {Name, {[
                {<<"content_type">>, Type},
                {<<"data">>, couch_util:encodeBase64(bin_to_binary(BinValue))}
            ]}}
        end,
        Attachments),
    case BinProps of
    [] -> [];
    _ -> [{<<"_attachments">>, {BinProps}}]
    end.

to_json_attachments(Attachments, Options) ->
    case lists:member(attachments, Options) of
    true -> % return the full rev list and the binaries as strings.
        to_json_attachments(Attachments);
    false ->
        to_json_attachment_stubs(Attachments)
    end.

to_json_obj(#doc{id=Id,deleted=Del,body=Body,revs={Start, RevIds},
            meta=Meta}=Doc,Options)->
    {[{<<"_id">>, Id}] 
        ++ to_json_rev(Start, RevIds)
        ++ to_json_body(Del, Body)
        ++ to_json_revisions(Options, Start, RevIds) 
        ++ to_json_meta(Meta)
        ++ to_json_attachments(Doc#doc.attachments, Options)
    }.

from_json_obj({Props}) ->
    transfer_fields(Props, #doc{body=[]});

from_json_obj(_Other) ->
    throw({bad_request, "Document must be a JSON object"}).

parse_rev(Rev) when is_binary(Rev) ->
    parse_rev(?b2l(Rev));
parse_rev(Rev) when is_list(Rev) ->
    SplitRev = lists:splitwith(fun($-) -> false; (_) -> true end, Rev),
    case SplitRev of 
        {Pos, [$- | RevId]} -> {list_to_integer(Pos), ?l2b(RevId)};
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
    Bins = lists:flatmap(fun({Name, {BinProps}}) ->
        case proplists:get_value(<<"stub">>, BinProps) of
        true ->
            Type = proplists:get_value(<<"content_type">>, BinProps),
            Length = proplists:get_value(<<"length">>, BinProps),
            [{Name, {stub, Type, Length}}];
        _ ->
            Value = proplists:get_value(<<"data">>, BinProps),
            Type = proplists:get_value(<<"content_type">>, BinProps,
                    ?DEFAULT_ATTACHMENT_CONTENT_TYPE),
            [{Name, {Type, couch_util:decodeBase64(Value)}}]
        end
    end, JsonBins),
    transfer_fields(Rest, Doc#doc{attachments=Bins});
    
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
    transfer_fields(Rest, Doc#doc{revs={Start, RevIds}});
    
transfer_fields([{<<"_deleted">>, B} | Rest], Doc) when (B==true) or (B==false) ->
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




bin_foldl(Bin, Fun, Acc) when is_binary(Bin) ->
    Fun(Bin, Acc);
bin_foldl({Fd, Sp, Len}, Fun, Acc) when is_tuple(Sp) orelse Sp == null ->
    % 09 UPGRADE CODE
    couch_stream:old_foldl(Fd, Sp, Len, Fun, Acc);
bin_foldl({Fd, Sp, _Len}, Fun, Acc) ->
    couch_stream:foldl(Fd, Sp, Fun, Acc).

bin_size(Bin) when is_binary(Bin) ->
    size(Bin);
bin_size({_Fd, _Sp, Len}) ->
    Len.

bin_to_binary(Bin) when is_binary(Bin) ->
    Bin;
bin_to_binary({Fd, Sp, _Len}) ->
    couch_stream:foldl(Fd, Sp, fun(Bin, Acc) -> [Bin|Acc] end, []).

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
        

has_stubs(#doc{attachments=Bins}) ->
    has_stubs(Bins);
has_stubs([]) ->
    false;
has_stubs([{_Name, {stub, _, _}}|_]) ->
    true;
has_stubs([_Bin|Rest]) ->
    has_stubs(Rest).

merge_stubs(#doc{attachments=MemBins}=StubsDoc, #doc{attachments=DiskBins}) ->
    BinDict = dict:from_list(DiskBins),
    MergedBins = lists:map(
        fun({Name, {stub, _, _}}) ->
            {Name, dict:fetch(Name, BinDict)};
        ({Name, Value}) ->
            {Name, Value}
        end, MemBins),
    StubsDoc#doc{attachments= MergedBins}.

read_streamed_attachment(_RcvFun, 0, Acc) ->
    list_to_binary(lists:reverse(Acc));
read_streamed_attachment(RcvFun, LenLeft, Acc) ->
    Bin = RcvFun(),
    read_streamed_attachment(RcvFun, LenLeft - size(Bin), [Bin|Acc]).
