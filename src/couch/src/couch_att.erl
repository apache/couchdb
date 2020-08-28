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

-module(couch_att).

-export([
    new/0,
    new/1,
    fetch/2,
    store/2,
    store/3,
    transform/3
]).

-export([
    is_stub/1,
    merge_stubs/2
]).

-export([
    external_size/1,
    size_info/1,
    to_disk_term/1,
    from_disk_term/3
]).

-export([
    from_json/2,
    to_json/4
]).

-export([
    flush/3,
    read_data/1,
    foldl/3,
    range_foldl/5,
    foldl_decode/3,
    to_binary/1
]).

-export([
    max_attachment_size/0,
    validate_attachment_size/3
]).

-compile(nowarn_deprecated_type).
-export_type([att/0]).


-include_lib("couch/include/couch_db.hrl").


-define(CURRENT_ATT_FORMAT, 0).


-type prop_name() ::
    name |
    type |
    att_len |
    disk_len |
    md5 |
    revpos |
    data |
    encoding.


-type data_prop_type() ::
    {loc, #{}, binary(), binary()} |
    stub |
    follows |
    binary() |
    {follows, pid(), reference()} |
    fun(() -> binary()).


-type att() :: #{
    name := binary(),
    type := binary(),
    att_len := non_neg_integer() | undefined,
    disk_len := non_neg_integer() | undefined,
    md5 := binary() | undefined,
    revpos := non_neg_integer(),
    data := data_prop_type(),
    encoding := identity | gzip | undefined
}.


new() ->
    #{
        name => <<>>,
        type => <<>>,
        att_len => undefined,
        disk_len => undefined,
        md5 => undefined,
        revpos => 0,
        data => undefined,
        encoding => undefined
    }.


-spec new([{prop_name(), any()}]) -> att().
new(Props) ->
    store(Props, new()).


-spec fetch([atom()], att()) -> [any()];
           (atom(), att()) -> any().
fetch(Fields, Att) when is_list(Fields) ->
    [fetch(Field, Att) || Field <- Fields];
fetch(Field, Att) ->
    maps:get(Field, Att).


-spec store([{atom(), any()}], att()) -> att().
store(Props, Att0) ->
    lists:foldl(fun({Field, Value}, Att) ->
        maps:update(Field, Value, Att)
    end, Att0, Props).


store(Field, Value, Att) ->
    maps:update(Field, Value, Att).


-spec transform(atom(), fun(), att()) -> att().
transform(Field, Fun, Att) ->
    maps:update_with(Field, Fun, Att).


is_stub(#{data := stub}) -> true;
is_stub(#{}) -> false.


%% merge_stubs takes all stub attachments and replaces them with on disk
%% attachments. It will return {missing, Name} if a stub isn't matched with
%% an existing attachment on disk. If the revpos is supplied with the stub
%% it is also only counted to match if is the same as the disk attachment.
merge_stubs(MemAtts, DiskAtts) ->
    OnDisk = dict:from_list(
        [{fetch(name, Att), Att} || Att <- DiskAtts]
    ),
    merge_stubs(MemAtts, OnDisk, []).


-spec merge_stubs([att()], dict:dict(), [att()]) -> [att()].
merge_stubs([Att | Rest], OnDisk, Merged) ->
    case fetch(data, Att) of
        stub ->
            [Name, Pos] = fetch([name, revpos], Att),
            case dict:find(Name, OnDisk) of
                {ok, DiskAtt} ->
                    RevPos = fetch(revpos, DiskAtt),
                    if
                        %% We want to check for consistency between the stub and
                        %% disk revpos here. If the stub's revpos is undefined
                        %% it means it wasn't provided by the user and does not
                        %% require being matched.
                        RevPos == Pos orelse Pos == undefined ->
                            merge_stubs(Rest, OnDisk, [DiskAtt | Merged]);
                        true ->
                            {missing, Name}
                    end;
                _ ->
                    {missing, Name}
            end;
        _ ->
            merge_stubs(Rest, OnDisk, [Att | Merged])
    end;
merge_stubs([], _, Merged) ->
    {ok, lists:reverse(Merged)}.


external_size(Att) ->
    NameSize = size(fetch(name, Att)),
    TypeSize = case fetch(type, Att) of
        undefined -> 0;
        Type -> size(Type)
    end,
    AttSize = fetch(att_len, Att),
    Md5Size = case fetch(md5, Att) of
        undefined -> 0;
        Md5 -> size(Md5)
    end,
    NameSize + TypeSize + AttSize + Md5Size.


size_info([]) ->
    {ok, []};
size_info(Atts) ->
    Info = lists:map(fun(Att) ->
        [{loc, _Db, _DocId, AttId}, AttLen] = fetch([data, att_len], Att),
        {AttId, AttLen}
    end, Atts),
    {ok, lists:usort(Info)}.


%% When converting an attachment to disk term format, attempt to stay with the
%% old format when possible. This should help make the attachment lazy upgrade
%% as safe as possible, avoiding the need for complicated disk versioning
%% schemes.
to_disk_term(Att) ->
    {loc, #{}, _DocId, AttId} = fetch(data, Att),
    {?CURRENT_ATT_FORMAT, {
        fetch(name, Att),
        fetch(type, Att),
        AttId,
        fetch(att_len, Att),
        fetch(disk_len, Att),
        fetch(revpos, Att),
        fetch(md5, Att),
        fetch(encoding, Att)
    }}.


from_disk_term(#{} = Db, DocId, {?CURRENT_ATT_FORMAT, Props}) ->
    {
        Name,
        Type,
        AttId,
        AttLen,
        DiskLen,
        RevPos,
        Md5,
        Encoding
    } = Props,
    new([
        {name, Name},
        {type, Type},
        {data, {loc, Db#{tx := undefined}, DocId, AttId}},
        {att_len, AttLen},
        {disk_len, DiskLen},
        {revpos, RevPos},
        {md5, Md5},
        {encoding, Encoding}
    ]).


%% from_json reads in embedded JSON attachments and creates usable attachment
%% values. The attachment may be a stub,
from_json(Name, Props) ->
    Type = couch_util:get_value(
        <<"content_type">>, Props, ?DEFAULT_ATTACHMENT_CONTENT_TYPE
    ),
    Att = new([{name, Name}, {type, Type}]),
    IsStub = couch_util:get_value(<<"stub">>, Props),
    Follows = couch_util:get_value(<<"follows">>, Props),
    if
        IsStub -> stub_from_json(Att, Props);
        Follows -> follow_from_json(Att, Props);
        true -> inline_from_json(Att, Props)
    end.


stub_from_json(Att, Props) ->
    {DiskLen, EncodedLen, Encoding} = encoded_lengths_from_json(Props),
    Digest = digest_from_json(Props),
    %% We specifically want undefined rather than the default 0 here to skip
    %% the revpos consistency check on stubs when it's not provided in the
    %% json object. See merge_stubs/3 for the stub check.
    RevPos = couch_util:get_value(<<"revpos">>, Props),
    store([
        {data, stub},
        {disk_len, DiskLen},
        {att_len, EncodedLen},
        {revpos, RevPos},
        {md5, Digest},
        {encoding, Encoding}
    ], Att).


follow_from_json(Att, Props) ->
    {DiskLen, EncodedLen, Encoding} = encoded_lengths_from_json(Props),
    Digest = digest_from_json(Props),
    RevPos = couch_util:get_value(<<"revpos">>, Props, 0),
    store([
        {data, follows},
        {disk_len, DiskLen},
        {att_len, EncodedLen},
        {revpos, RevPos},
        {md5, Digest},
        {encoding, Encoding}
    ], Att).


inline_from_json(Att, Props) ->
    B64Data = couch_util:get_value(<<"data">>, Props),
    try base64:decode(B64Data) of
        Data ->
            Length = size(Data),
            RevPos = couch_util:get_value(<<"revpos">>, Props, 0),
            store([
                {data, Data},
                {disk_len, Length},
                {att_len, Length},
                {revpos, RevPos}
            ], Att)
    catch
        _:_ ->
            Name = fetch(name, Att),
            ErrMsg =  <<"Invalid attachment data for ", Name/binary>>,
            throw({bad_request, ErrMsg})
    end.


encoded_lengths_from_json(Props) ->
    Len = couch_util:get_value(<<"length">>, Props),
    case couch_util:get_value(<<"encoding">>, Props) of
        undefined ->
            Encoding = identity,
            EncodedLen = Len;
        EncodingValue ->
            EncodedLen = couch_util:get_value(<<"encoded_length">>, Props, Len),
            Encoding = list_to_existing_atom(binary_to_list(EncodingValue))
    end,
    {Len, EncodedLen, Encoding}.


digest_from_json(Props) ->
    case couch_util:get_value(<<"digest">>, Props) of
        <<"md5-", EncodedMd5/binary>> -> base64:decode(EncodedMd5);
        _ -> <<>>
    end.


to_json(Att, OutputData, DataToFollow, ShowEncoding) ->
    #{
        name := Name,
        type := Type,
        data := Data,
        disk_len := DiskLen,
        att_len := AttLen,
        revpos := RevPos,
        md5 := Md5,
        encoding := Encoding
    } = Att,
    Props = [
        {<<"content_type">>, Type},
        {<<"revpos">>, RevPos}
    ],
    DigestProp = case base64:encode(Md5) of
        <<>> -> [];
        Digest -> [{<<"digest">>, <<"md5-", Digest/binary>>}]
    end,
    DataProps = if
        not OutputData orelse Data == stub ->
            [{<<"length">>, DiskLen}, {<<"stub">>, true}];
        DataToFollow ->
            [{<<"length">>, DiskLen}, {<<"follows">>, true}];
        true ->
            AttData = case Encoding of
                gzip -> zlib:gunzip(to_binary(Att));
                identity -> to_binary(Att)
            end,
            [{<<"data">>, base64:encode(AttData)}]
    end,
    EncodingProps = if
        ShowEncoding andalso Encoding /= identity ->
            [
                {<<"encoding">>, couch_util:to_binary(Encoding)},
                {<<"encoded_length">>, AttLen}
            ];
        true ->
            []
    end,
    {Name, {Props ++ DigestProp ++ DataProps ++ EncodingProps}}.


flush(Db, DocId, Att1) ->
    Data0 = fetch(data, Att1),
    case {Data0, Db} of
        {{follows, _, _}, #{tx := Tx}} when Tx =/= undefined ->
            error(follows_cannot_be_used_in_a_transaction);
        {_, #{}} ->
            ok
    end,
    Att2 = read_data(Data0, Att1),
    [
        Data,
        AttLen,
        DiskLen,
        ReqMd5,
        Encoding
    ] = fetch([data, att_len, disk_len, md5, encoding], Att2),

    % Eventually, we'll check if we can compress this
    % attachment here and do so if possible.

    % If we were sent a gzip'ed attachment with no
    % length data, we have to set it here.
    Att3 = case DiskLen of
        undefined when AttLen /= undefined ->
            store(disk_len, AttLen, Att2);
        undefined when is_binary(Data) ->
            store(disk_len, size(Data), Att2);
        _ ->
            Att2
    end,

    % If no encoding has been set, default to
    % identity
    Att4 = case Encoding of
        undefined -> store(encoding, identity, Att3);
        _ -> Att3
    end,

    case Data of
        {loc, _, _, _} ->
            % Already flushed
            Att1;
        _ when is_binary(Data) ->
            DataMd5 = couch_hash:md5_hash(Data),
            if ReqMd5 == undefined -> ok; true ->
                couch_util:check_md5(DataMd5, ReqMd5)
            end,
            Att5 = store(md5, DataMd5, Att4),
            Att6 = maybe_compress(Att5),
            fabric2_db:write_attachment(Db, DocId, Att6)
    end.


read_data(Att) ->
    Data = fetch(data, Att),
    read_data(Data, Att).


read_data({loc, #{}, _DocId, _AttId}, Att) ->
    % Attachment already written to fdb
    Att;

read_data({follows, Parser, Ref}, Att) ->
    ParserRef = erlang:monitor(process, Parser),
    Fun = fun() ->
        Parser ! {get_bytes, Ref, self()},
        receive
            {started_open_doc_revs, NewRef} ->
                couch_doc:restart_open_doc_revs(Parser, Ref, NewRef);
            {bytes, Ref, Bytes} ->
                Bytes;
            {'DOWN', ParserRef, _, _, Reason} ->
                throw({mp_parser_died, Reason})
        end
    end,
    try
        read_data(Fun, store(data, Fun, Att))
    after
        erlang:demonitor(ParserRef, [flush])
    end;

read_data(Data, Att) when is_binary(Data) ->
    case fetch(att_len, Att) of
        undefined -> store(att_len, size(Data), Att);
        Int when is_integer(Int) ->  Att
    end;

read_data(Fun, Att) when is_function(Fun) ->
    [AttName, AttLen, InMd5] = fetch([name, att_len, md5], Att),
    MaxAttSize = max_attachment_size(),
    case AttLen of
        undefined ->
            % Fun(MaxChunkSize, WriterFun) must call WriterFun
            % once for each chunk of the attachment,
            WriterFun = fun
                ({0, Footers}, {Len, Acc}) ->
                    F = mochiweb_headers:from_binary(Footers),
                    Md5 = case mochiweb_headers:get_value("Content-MD5", F) of
                        undefined -> undefined;
                        Value -> base64:decode(Value)
                    end,
                    Props0 = [
                        {data, iolist_to_binary(lists:reverse(Acc))},
                        {att_len, Len}
                    ],
                    Props1 = if InMd5 /= md5_in_footer -> Props0; true ->
                        [{md5, Md5} | Props0]
                    end,
                    store(Props1, Att);
                ({ChunkLen, Chunk}, {Len, Acc}) ->
                    NewLen = Len + ChunkLen,
                    validate_attachment_size(AttName, NewLen, MaxAttSize),
                    {NewLen, [Chunk | Acc]}
            end,
            Fun(8192, WriterFun, {0, []});
        AttLen ->
            validate_attachment_size(AttName, AttLen, MaxAttSize),
            read_streamed_attachment(Att, Fun, AttLen, [])
    end.


read_streamed_attachment(Att, _F, 0, Acc) ->
    Bin = iolist_to_binary(lists:reverse(Acc)),
    store([
        {data, Bin},
        {att_len, size(Bin)}
    ], Att);

read_streamed_attachment(_Att, _F, LenLeft, _Acc) when LenLeft < 0 ->
    throw({bad_request, <<"attachment longer than expected">>});

read_streamed_attachment(Att, F, LenLeft, Acc) when LenLeft > 0 ->
    Bin = try
        read_next_chunk(F, LenLeft)
    catch
        {mp_parser_died, normal} ->
            throw({bad_request, <<"attachment shorter than expected">>})
    end,
    Size = iolist_size(Bin),
    read_streamed_attachment(Att, F, LenLeft - Size, [Bin | Acc]).


read_next_chunk(F, _) when is_function(F, 0) ->
    F();

read_next_chunk(F, LenLeft) when is_function(F, 1) ->
    F(lists:min([LenLeft, 16#2000])).


foldl(Att, Fun, Acc) ->
    foldl(fetch(data, Att), Att, Fun, Acc).


foldl({loc, Db, DocId, AttId}, _Att, Fun, Acc) ->
    Bin = fabric2_db:read_attachment(Db#{tx := undefined}, DocId, AttId),
    Fun(Bin, Acc);

foldl(Bin, _Att, Fun, Acc) when is_binary(Bin) ->
    Fun(Bin, Acc);

foldl(DataFun, Att, Fun, Acc) when is_function(DataFun) ->
    Len = fetch(att_len, Att),
    fold_streamed_data(DataFun, Len, Fun, Acc);

foldl({follows, Parser, Ref}, Att, Fun, Acc) ->
    ParserRef = erlang:monitor(process, Parser),
    DataFun = fun() ->
        Parser ! {get_bytes, Ref, self()},
        receive
            {started_open_doc_revs, NewRef} ->
                couch_doc:restart_open_doc_revs(Parser, Ref, NewRef);
            {bytes, Ref, Bytes} ->
                Bytes;
            {'DOWN', ParserRef, _, _, Reason} ->
                throw({mp_parser_died, Reason})
        end
    end,
    try
        foldl(DataFun, store(data, DataFun, Att), Fun, Acc)
    after
        erlang:demonitor(ParserRef, [flush])
    end.


range_foldl(Bin1, From, To, Fun, Acc) when is_binary(Bin1) ->
    ReadLen = To - From,
    Bin2 = case Bin1 of
        _ when size(Bin1) < From -> <<>>;
        <<_:From/binary, B2/binary>> -> B2
    end,
    Bin3 = case Bin2 of
        _ when size(Bin2) < ReadLen -> Bin2;
        <<B3:ReadLen/binary, _/binary>> -> B3
    end,
    Fun(Bin3, Acc);

range_foldl(Att, From, To, Fun, Acc) ->
    {loc, Db, DocId, AttId} = fetch(data, Att),
    Bin = fabric2_db:read_attachment(Db, DocId, AttId),
    range_foldl(Bin, From, To, Fun, Acc).


foldl_decode(Att, Fun, Acc) ->
    [Encoding, Data] = fetch([encoding, data], Att),
    case {Encoding, Data} of
        {gzip, {loc, Db, DocId, AttId}} ->
            NoTxDb = Db#{tx := undefined},
            Bin = fabric2_db:read_attachment(NoTxDb, DocId, AttId),
            foldl_decode(store(data, Bin, Att), Fun, Acc);
        {gzip, _} when is_binary(Data) ->
            Z = zlib:open(),
            ok = zlib:inflateInit(Z, 16 + 15),
            Inflated = iolist_to_binary(zlib:inflate(Z, Data)),
            ok = zlib:inflateEnd(Z),
            ok = zlib:close(Z),
            foldl(Inflated, Att, Fun, Acc);
        _ ->
            foldl(Att, Fun, Acc)
    end.


to_binary(Att) ->
    to_binary(fetch(data, Att), Att).


to_binary(Bin, _Att) when is_binary(Bin) ->
    Bin;
to_binary(Iolist, _Att) when is_list(Iolist) ->
    iolist_to_binary(Iolist);
to_binary({loc, Db, DocId, AttId}, _Att) ->
    NoTxDb = Db#{tx := undefined},
    fabric2_db:read_attachment(NoTxDb, DocId, AttId);
to_binary(DataFun, Att) when is_function(DataFun)->
    Len = fetch(att_len, Att),
    iolist_to_binary(
        lists:reverse(fold_streamed_data(
            DataFun,
            Len,
            fun(Data, Acc) -> [Data | Acc] end,
            []
        ))
    ).


fold_streamed_data(_RcvFun, 0, _Fun, Acc) ->
    Acc;

fold_streamed_data(RcvFun, LenLeft, Fun, Acc) when LenLeft > 0->
    Bin = RcvFun(),
    ResultAcc = Fun(Bin, Acc),
    fold_streamed_data(RcvFun, LenLeft - size(Bin), Fun, ResultAcc).


maybe_compress(Att) ->
    [Encoding, Type] = fetch([encoding, type], Att),
    IsCompressible = is_compressible(Type),
    CompLevel = config:get_integer("attachments", "compression_level", 0),
    case Encoding of
        identity when IsCompressible, CompLevel >= 1, CompLevel =< 9 ->
            compress(Att, CompLevel);
        _ ->
            Att
    end.


compress(Att, Level) ->
    Data = fetch(data, Att),

    Z = zlib:open(),
    % 15 = ?MAX_WBITS (defined in the zlib module)
    % the 16 + ?MAX_WBITS formula was obtained by inspecting zlib:gzip/1
    ok = zlib:deflateInit(Z, Level, deflated, 16 + 15, 8, default),
    CompData = iolist_to_binary(zlib:deflate(Z, Data, finish)),
    ok = zlib:deflateEnd(Z),
    ok = zlib:close(Z),

    store([
        {att_len, size(CompData)},
        {md5, couch_hash:md5_hash(CompData)},
        {data, CompData},
        {encoding, gzip}
    ], Att).


is_compressible(Type) when is_binary(Type) ->
    is_compressible(binary_to_list(Type));
is_compressible(Type) ->
    TypeExpList = re:split(
        config:get("attachments", "compressible_types", ""),
        "\\s*,\\s*",
        [{return, list}]
    ),
    lists:any(
        fun(TypeExp) ->
            Regexp = ["^\\s*", re:replace(TypeExp, "\\*", ".*"),
                "(?:\\s*;.*?)?\\s*", $$],
            re:run(Type, Regexp, [caseless]) =/= nomatch
        end,
        [T || T <- TypeExpList, T /= []]
    ).


max_attachment_size() ->
    case config:get("couchdb", "max_attachment_size", "infinity") of
        "infinity" ->
            infinity;
        MaxAttSize ->
            list_to_integer(MaxAttSize)
    end.


validate_attachment_size(AttName, AttSize, MaxAttSize)
        when is_integer(AttSize),  AttSize > MaxAttSize ->
    throw({request_entity_too_large, {attachment, AttName}});
validate_attachment_size(_AttName, _AttSize, _MAxAttSize) ->
    ok.


%% -ifdef(TEST).
%% -include_lib("eunit/include/eunit.hrl").
%%
%% % Eww...
%% -include("couch_bt_engine.hrl").
%%
%% %% Test utilities
%%
%%
%% empty_att() -> new().
%%
%%
%% upgraded_empty_att() ->
%%     new([{headers, undefined}]).
%%
%%
%% %% Test groups
%%
%%
%% attachment_upgrade_test_() ->
%%     {"Lazy record upgrade tests", [
%%         {"Existing record fields don't upgrade",
%%             {with, empty_att(), [fun test_non_upgrading_fields/1]}
%%         },
%%         {"New fields upgrade",
%%             {with, empty_att(), [fun test_upgrading_fields/1]}
%%         }
%%     ]}.
%%
%%
%% attachment_defaults_test_() ->
%%     {"Attachment defaults tests", [
%%         {"Records retain old default values", [
%%             {with, empty_att(), [fun test_legacy_defaults/1]}
%%         ]},
%%         {"Upgraded records inherit defaults", [
%%             {with, upgraded_empty_att(), [fun test_legacy_defaults/1]}
%%         ]},
%%         {"Undefined entries are elided on upgrade", [
%%             {with, upgraded_empty_att(), [fun test_elided_entries/1]}
%%         ]}
%%     ]}.
%%
%% attachment_field_api_test_() ->
%%     {"Basic attachment field api", [
%%         fun test_construction/0,
%%         fun test_store_and_fetch/0,
%%         fun test_transform/0
%%     ]}.
%%
%%
%% attachment_disk_term_test_() ->
%%     BaseAttachment = new([
%%         {name, <<"empty">>},
%%         {type, <<"application/octet-stream">>},
%%         {att_len, 0},
%%         {disk_len, 0},
%%         {md5, <<212,29,140,217,143,0,178,4,233,128,9,152,236,248,66,126>>},
%%         {revpos, 4},
%%         {data, {stream, {couch_bt_engine_stream, {fake_fd, fake_sp}}}},
%%         {encoding, identity}
%%     ]),
%%     BaseDiskTerm = {
%%         <<"empty">>,
%%         <<"application/octet-stream">>,
%%         fake_sp,
%%         0, 0, 4,
%%         <<212,29,140,217,143,0,178,4,233,128,9,152,236,248,66,126>>,
%%         identity
%%     },
%%     Headers = [{<<"X-Foo">>, <<"bar">>}],
%%     ExtendedAttachment = store(headers, Headers, BaseAttachment),
%%     ExtendedDiskTerm = {BaseDiskTerm, [{headers, Headers}]},
%%     FakeDb = test_util:fake_db([{engine, {couch_bt_engine, #st{fd=fake_fd}}}]),
%%     {"Disk term tests", [
%%         ?_assertEqual(BaseDiskTerm, to_disk_term(BaseAttachment)),
%%         ?_assertEqual(BaseAttachment, from_disk_term(FakeDb, BaseDiskTerm)),
%%         ?_assertEqual(ExtendedDiskTerm, to_disk_term(ExtendedAttachment)),
%%         ?_assertEqual(ExtendedAttachment, from_disk_term(FakeDb, ExtendedDiskTerm))
%%     ]}.
%%
%%
%% attachment_json_term_test_() ->
%%     Props = [
%%         {<<"content_type">>, <<"application/json">>},
%%         {<<"digest">>, <<"md5-QCNtWUNXV0UzJnEjMk92YUk1JA==">>},
%%         {<<"length">>, 14},
%%         {<<"revpos">>, 1}
%%     ],
%%     PropsInline = [{<<"data">>, <<"eyJhbnN3ZXIiOiA0Mn0=">>}] ++ Props,
%%     InvalidProps = [{<<"data">>, <<"!Base64Encoded$">>}] ++ Props,
%%     Att = couch_att:new([
%%         {name, <<"attachment.json">>},
%%         {type, <<"application/json">>}
%%     ]),
%%     ResultStub = couch_att:new([
%%         {name, <<"attachment.json">>},
%%         {type, <<"application/json">>},
%%         {att_len, 14},
%%         {disk_len, 14},
%%         {md5, <<"@#mYCWWE3&q#2OvaI5$">>},
%%         {revpos, 1},
%%         {data, stub},
%%         {encoding, identity}
%%     ]),
%%     ResultFollows = ResultStub#att{data = follows},
%%     ResultInline = ResultStub#att{md5 = <<>>, data = <<"{\"answer\": 42}">>},
%%     {"JSON term tests", [
%%         ?_assertEqual(ResultStub, stub_from_json(Att, Props)),
%%         ?_assertEqual(ResultFollows, follow_from_json(Att, Props)),
%%         ?_assertEqual(ResultInline, inline_from_json(Att, PropsInline)),
%%         ?_assertThrow({bad_request, _}, inline_from_json(Att, Props)),
%%         ?_assertThrow({bad_request, _}, inline_from_json(Att, InvalidProps))
%%     ]}.
%%
%%
%% attachment_stub_merge_test_() ->
%%     %% Stub merging needs to demonstrate revpos matching, skipping, and missing
%%     %% attachment errors.
%%     {"Attachment stub merging tests", []}.
%%
%%
%% %% Test generators
%%
%%
%% test_non_upgrading_fields(Attachment) ->
%%     Pairs = [
%%         {name, "cat.gif"},
%%         {type, "text/very-very-plain"},
%%         {att_len, 1024},
%%         {disk_len, 42},
%%         {md5, <<"md5-hashhashhash">>},
%%         {revpos, 4},
%%         {data, stub},
%%         {encoding, gzip}
%%     ],
%%     lists:foreach(
%%         fun({Field, Value}) ->
%%             ?assertMatch(#att{}, Attachment),
%%             Updated = store(Field, Value, Attachment),
%%             ?assertMatch(#att{}, Updated)
%%         end,
%%     Pairs).
%%
%%
%% test_upgrading_fields(Attachment) ->
%%     ?assertMatch(#att{}, Attachment),
%%     UpdatedHeaders = store(headers, [{<<"Ans">>, <<"42">>}], Attachment),
%%     ?assertMatch(X when is_list(X), UpdatedHeaders),
%%     UpdatedHeadersUndefined = store(headers, undefined, Attachment),
%%     ?assertMatch(X when is_list(X), UpdatedHeadersUndefined).
%%
%%
%% test_legacy_defaults(Attachment) ->
%%     ?assertEqual(<<>>, fetch(md5, Attachment)),
%%     ?assertEqual(0, fetch(revpos, Attachment)),
%%     ?assertEqual(identity, fetch(encoding, Attachment)).
%%
%%
%% test_elided_entries(Attachment) ->
%%     ?assertNot(lists:keymember(name, 1, Attachment)),
%%     ?assertNot(lists:keymember(type, 1, Attachment)),
%%     ?assertNot(lists:keymember(att_len, 1, Attachment)),
%%     ?assertNot(lists:keymember(disk_len, 1, Attachment)),
%%     ?assertNot(lists:keymember(data, 1, Attachment)).
%%
%%
%% test_construction() ->
%%     ?assert(new() == new()),
%%     Initialized = new([{name, <<"foo.bar">>}, {type, <<"application/qux">>}]),
%%     ?assertEqual(<<"foo.bar">>, fetch(name, Initialized)),
%%     ?assertEqual(<<"application/qux">>, fetch(type, Initialized)).
%%
%%
%% test_store_and_fetch() ->
%%     Attachment = empty_att(),
%%     ?assertEqual(<<"abc">>, fetch(name, store(name, <<"abc">>, Attachment))),
%%     ?assertEqual(42, fetch(ans, store(ans, 42, Attachment))).
%%
%%
%% test_transform() ->
%%     Attachment = new([{counter, 0}]),
%%     Transformed = transform(counter, fun(Count) -> Count + 1 end, Attachment),
%%     ?assertEqual(1, fetch(counter, Transformed)).
%%
%%
%% -endif.
