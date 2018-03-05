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
    size_info/1,
    to_disk_term/1,
    from_disk_term/2
]).

-export([
    from_json/2,
    to_json/4
]).

-export([
    flush/2,
    foldl/3,
    range_foldl/5,
    foldl_decode/3,
    to_binary/1
]).

-export([
    upgrade/1,
    downgrade/1
]).

-export([
    max_attachment_size/0,
    validate_attachment_size/3
]).

-compile(nowarn_deprecated_type).
-export_type([att/0]).

-include_lib("couch/include/couch_db.hrl").


%% Legacy attachment record. This is going to be phased out by the new proplist
%% based structure. It's needed for now to allow code to perform lazy upgrades
%% while the patch is rolled out to the cluster. Attachments passed as records
%% will remain so until they are required to be represented as property lists.
%% Once this has been widely deployed, this record will be removed entirely and
%% property lists will be the main format.
-record(att, {
    name :: binary(),
    type :: binary(),
    att_len :: non_neg_integer(),

    %% length of the attachment in its identity form
    %% (that is, without a content encoding applied to it)
    %% differs from att_len when encoding /= identity
    disk_len :: non_neg_integer(),

    md5 = <<>> :: binary(),
    revpos = 0 :: non_neg_integer(),
    data :: stub | follows | binary() | {any(), any()} |
            {follows, pid(), reference()} | fun(() -> binary()),

    %% Encoding of the attachment
    %% currently supported values are:
    %%     identity, gzip
    %% additional values to support in the future:
    %%     deflate, compress
    encoding = identity :: identity | gzip
}).


%% Extensible Attachment Type
%%
%% The following types describe the known properties for attachment fields
%% encoded as property lists to allow easier upgrades. Values not in this list
%% should be accepted at runtime but should be treated as opaque data as might
%% be used by upgraded code. If you plan on operating on new data, please add
%% an entry here as documentation.


%% The name of the attachment is also used as the mime-part name for file
%% downloads. These must be unique per document.
-type name_prop() :: {name, binary()}.


%% The mime type of the attachment. This does affect compression of certain
%% attachments if the type is found to be configured as a compressable type.
%% This is commonly reserved for text/* types but could include other custom
%% cases as well. See definition and use of couch_util:compressable_att_type/1.
-type type_prop() :: {type, binary()}.


%% The attachment length is similar to disk-length but ignores additional
%% encoding that may have occurred.
-type att_len_prop() :: {att_len, non_neg_integer()}.


%% The size of the attachment as stored in a disk stream.
-type disk_len_prop() :: {disk_len, non_neg_integer()}.


%% This is a digest of the original attachment data as uploaded by the client.
%% it's useful for checking validity of contents against other attachment data
%% as well as quick digest computation of the enclosing document.
-type md5_prop() :: {md5, binary()}.


-type revpos_prop() :: {revpos, 0}.


%% This field is currently overloaded with just about everything. The
%% {any(), any()} type is just there until I have time to check the actual
%% values expected. Over time this should be split into more than one property
%% to allow simpler handling.
-type data_prop() :: {
    data, stub | follows | binary() | {any(), any()} |
    {follows, pid(), reference()} | fun(() -> binary())
}.


%% We will occasionally compress our data. See type_prop() for more information
%% on when this happens.
-type encoding_prop() :: {encoding, identity | gzip}.


-type attachment() :: [
    name_prop() | type_prop() |
    att_len_prop() | disk_len_prop() |
    md5_prop() | revpos_prop() |
    data_prop() | encoding_prop()
].


-type att() :: #att{} | attachment().


new() ->
    %% We construct a record by default for compatability. This will be
    %% upgraded on demand. A subtle effect this has on all attachments
    %% constructed via new is that it will pick up the proper defaults
    %% from the #att record definition given above. Newer properties do
    %% not support special default values and will all be treated as
    %% undefined.
    #att{}.


-spec new([{atom(), any()}]) -> att().
new(Props) ->
    store(Props, new()).


-spec fetch([atom()], att()) -> [any()];
           (atom(), att()) -> any().
fetch(Fields, Att) when is_list(Fields) ->
    [fetch(Field, Att) || Field <- Fields];
fetch(Field, Att) when is_list(Att) ->
    case lists:keyfind(Field, 1, Att) of
        {Field, Value} -> Value;
        false -> undefined
    end;
fetch(name, #att{name = Name}) ->
    Name;
fetch(type, #att{type = Type}) ->
    Type;
fetch(att_len, #att{att_len = AttLen}) ->
    AttLen;
fetch(disk_len, #att{disk_len = DiskLen}) ->
    DiskLen;
fetch(md5, #att{md5 = Digest}) ->
    Digest;
fetch(revpos, #att{revpos = RevPos}) ->
    RevPos;
fetch(data, #att{data = Data}) ->
    Data;
fetch(encoding, #att{encoding = Encoding}) ->
    Encoding;
fetch(_, _) ->
    undefined.


-spec store([{atom(), any()}], att()) -> att().
store(Props, Att0) ->
    lists:foldl(fun({Field, Value}, Att) ->
        store(Field, Value, Att)
    end, Att0, Props).


-spec store(atom(), any(), att()) -> att().
store(Field, undefined, Att) when is_list(Att) ->
    lists:keydelete(Field, 1, Att);
store(Field, Value, Att) when is_list(Att) ->
    lists:keystore(Field, 1, Att, {Field, Value});
store(name, Name, Att) ->
    Att#att{name = Name};
store(type, Type, Att) ->
    Att#att{type = Type};
store(att_len, AttLen, Att) ->
    Att#att{att_len = AttLen};
store(disk_len, DiskLen, Att) ->
    Att#att{disk_len = DiskLen};
store(md5, Digest, Att) ->
    Att#att{md5 = Digest};
store(revpos, RevPos, Att) ->
    Att#att{revpos = RevPos};
store(data, Data, Att) ->
    Att#att{data = Data};
store(encoding, Encoding, Att) ->
    Att#att{encoding = Encoding};
store(Field, Value, Att) ->
    store(Field, Value, upgrade(Att)).


-spec transform(atom(), fun(), att()) -> att().
transform(Field, Fun, Att) ->
    NewValue = Fun(fetch(Field, Att)),
    store(Field, NewValue, Att).


is_stub(Att) ->
    stub == fetch(data, Att).


%% merge_stubs takes all stub attachments and replaces them with on disk
%% attachments. It will return {missing, Name} if a stub isn't matched with
%% an existing attachment on disk. If the revpos is supplied with the stub
%% it is also only counted to match if is the same as the disk attachment.
merge_stubs(MemAtts, DiskAtts) ->
    OnDisk = dict:from_list(
        [{fetch(name, Att), Att} || Att <- DiskAtts]
    ),
    merge_stubs(MemAtts, OnDisk, []).


%% restore spec when R14 support is dropped
%% -spec merge_stubs([att()], dict:dict(), [att()]) -> [att()].
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


size_info([]) ->
    {ok, []};
size_info(Atts) ->
    Info = lists:map(fun(Att) ->
        [{_, Pos}, AttLen] = fetch([data, att_len], Att),
        {Pos, AttLen}
    end, Atts),
    {ok, lists:usort(Info)}.


%% When converting an attachment to disk term format, attempt to stay with the
%% old format when possible. This should help make the attachment lazy upgrade
%% as safe as possible, avoiding the need for complicated disk versioning
%% schemes.
to_disk_term(#att{} = Att) ->
    {stream, StreamEngine} = fetch(data, Att),
    {ok, Sp} = couch_stream:to_disk_term(StreamEngine),
    {
        fetch(name, Att),
        fetch(type, Att),
        Sp,
        fetch(att_len, Att),
        fetch(disk_len, Att),
        fetch(revpos, Att),
        fetch(md5, Att),
        fetch(encoding, Att)
    };
to_disk_term(Att) ->
    BaseProps = [name, type, data, att_len, disk_len, revpos, md5, encoding],
    {Extended, Base} = lists:foldl(
        fun
            (data, {Props, Values}) ->
                case lists:keytake(data, 1, Props) of
                    {value, {_, {stream, StreamEngine}}, Other} ->
                        {ok, Sp} = couch_stream:to_disk_term(StreamEngine),
                        {Other, [Sp | Values]};
                    {value, {_, Value}, Other} ->
                        {Other, [Value | Values]};
                    false ->
                        {Props, [undefined | Values]}
                end;
            (Key, {Props, Values}) ->
                case lists:keytake(Key, 1, Props) of
                    {value, {_, Value}, Other} -> {Other, [Value | Values]};
                    false -> {Props, [undefined | Values]}
                end
        end,
        {Att, []},
        BaseProps
    ),
    {list_to_tuple(lists:reverse(Base)), Extended}.


%% The new disk term format is a simple wrapper around the legacy format. Base
%% properties will remain in a tuple while the new fields and possibly data from
%% future extensions will be stored in a list of atom/value pairs. While this is
%% slightly less efficient, future work should be able to make use of
%% compression to remove these sorts of common bits (block level compression
%% with something like a shared dictionary that is checkpointed every now and
%% then).
from_disk_term(StreamSrc, {Base, Extended})
        when is_tuple(Base), is_list(Extended) ->
    store(Extended, from_disk_term(StreamSrc, Base));
from_disk_term(StreamSrc, {Name,Type,Sp,AttLen,DiskLen,RevPos,Md5,Enc}) ->
    {ok, Stream} = open_stream(StreamSrc, Sp),
    #att{
        name=Name,
        type=Type,
        att_len=AttLen,
        disk_len=DiskLen,
        md5=Md5,
        revpos=RevPos,
        data={stream, Stream},
        encoding=upgrade_encoding(Enc)
    };
from_disk_term(StreamSrc, {Name,Type,Sp,AttLen,RevPos,Md5}) ->
    {ok, Stream} = open_stream(StreamSrc, Sp),
    #att{
        name=Name,
        type=Type,
        att_len=AttLen,
        disk_len=AttLen,
        md5=Md5,
        revpos=RevPos,
        data={stream, Stream}
    };
from_disk_term(StreamSrc, {Name,{Type,Sp,AttLen}}) ->
    {ok, Stream} = open_stream(StreamSrc, Sp),
    #att{
        name=Name,
        type=Type,
        att_len=AttLen,
        disk_len=AttLen,
        md5= <<>>,
        revpos=0,
        data={stream, Stream}
    }.


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
        {md5, Digest}, {revpos, RevPos}, {data, stub}, {disk_len, DiskLen},
        {att_len, EncodedLen}, {encoding, Encoding}
    ], Att).


follow_from_json(Att, Props) ->
    {DiskLen, EncodedLen, Encoding} = encoded_lengths_from_json(Props),
    Digest = digest_from_json(Props),
    RevPos = couch_util:get_value(<<"revpos">>, Props, 0),
    store([
        {md5, Digest}, {revpos, RevPos}, {data, follows}, {disk_len, DiskLen},
        {att_len, EncodedLen}, {encoding, Encoding}
    ], Att).


inline_from_json(Att, Props) ->
    B64Data = couch_util:get_value(<<"data">>, Props),
    try base64:decode(B64Data) of
        Data ->
            Length = size(Data),
            RevPos = couch_util:get_value(<<"revpos">>, Props, 0),
            store([
                {data, Data}, {revpos, RevPos}, {disk_len, Length},
                {att_len, Length}
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
    [Name, Data, DiskLen, AttLen, Enc, Type, RevPos, Md5] = fetch(
        [name, data, disk_len, att_len, encoding, type, revpos, md5], Att
    ),
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
            AttData = case Enc of
                gzip -> zlib:gunzip(to_binary(Att));
                identity -> to_binary(Att)
            end,
            [{<<"data">>, base64:encode(AttData)}]
    end,
    EncodingProps = if
        ShowEncoding andalso Enc /= identity ->
            [
                {<<"encoding">>, couch_util:to_binary(Enc)},
                {<<"encoded_length">>, AttLen}
            ];
        true ->
            []
    end,
    HeadersProp = case fetch(headers, Att) of
        undefined -> [];
        Headers -> [{<<"headers">>, Headers}]
    end,
    {Name, {Props ++ DigestProp ++ DataProps ++ EncodingProps ++ HeadersProp}}.


flush(Db, Att) ->
    flush_data(Db, fetch(data, Att), Att).


flush_data(Db, Data, Att) when is_binary(Data) ->
    couch_db:with_stream(Db, Att, fun(OutputStream) ->
        couch_stream:write(OutputStream, Data)
    end);
flush_data(Db, Fun, Att) when is_function(Fun) ->
    AttName = fetch(name, Att),
    MaxAttSize = max_attachment_size(),
    case fetch(att_len, Att) of
        undefined ->
            couch_db:with_stream(Db, Att, fun(OutputStream) ->
                % Fun(MaxChunkSize, WriterFun) must call WriterFun
                % once for each chunk of the attachment,
                Fun(4096,
                    % WriterFun({Length, Binary}, State)
                    % WriterFun({0, _Footers}, State)
                    % Called with Length == 0 on the last time.
                    % WriterFun returns NewState.
                    fun({0, Footers}, _Total) ->
                        F = mochiweb_headers:from_binary(Footers),
                        case mochiweb_headers:get_value("Content-MD5", F) of
                        undefined ->
                            ok;
                        Md5 ->
                            {md5, base64:decode(Md5)}
                        end;
                    ({Length, Chunk}, Total0) ->
                        Total = Total0 + Length,
                        validate_attachment_size(AttName, Total, MaxAttSize),
                        couch_stream:write(OutputStream, Chunk),
                        Total
                    end, 0)
            end);
        AttLen ->
            validate_attachment_size(AttName, AttLen, MaxAttSize),
            couch_db:with_stream(Db, Att, fun(OutputStream) ->
                write_streamed_attachment(OutputStream, Fun, AttLen)
            end)
    end;
flush_data(Db, {follows, Parser, Ref}, Att) ->
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
        flush_data(Db, Fun, store(data, Fun, Att))
    after
        erlang:demonitor(ParserRef, [flush])
    end;
flush_data(Db, {stream, StreamEngine}, Att) ->
    case couch_db:is_active_stream(Db, StreamEngine) of
        true ->
            % Already written
            Att;
        false ->
            NewAtt = couch_db:with_stream(Db, Att, fun(OutputStream) ->
                couch_stream:copy(StreamEngine, OutputStream)
            end),
            InMd5 = fetch(md5, Att),
            OutMd5 = fetch(md5, NewAtt),
            couch_util:check_md5(OutMd5, InMd5),
            NewAtt
    end.


write_streamed_attachment(_Stream, _F, 0) ->
    ok;
write_streamed_attachment(_Stream, _F, LenLeft) when LenLeft < 0 ->
    throw({bad_request, <<"attachment longer than expected">>});
write_streamed_attachment(Stream, F, LenLeft) when LenLeft > 0 ->
    Bin = try read_next_chunk(F, LenLeft)
    catch
        {mp_parser_died, normal} ->
            throw({bad_request, <<"attachment shorter than expected">>})
    end,
    ok = couch_stream:write(Stream, Bin),
    write_streamed_attachment(Stream, F, LenLeft - iolist_size(Bin)).

read_next_chunk(F, _) when is_function(F, 0) ->
    F();
read_next_chunk(F, LenLeft) when is_function(F, 1) ->
    F(lists:min([LenLeft, 16#2000])).


foldl(Att, Fun, Acc) ->
    foldl(fetch(data, Att), Att, Fun, Acc).


foldl(Bin, _Att, Fun, Acc) when is_binary(Bin) ->
    Fun(Bin, Acc);
foldl({stream, StreamEngine}, Att, Fun, Acc) ->
    Md5 = fetch(md5, Att),
    couch_stream:foldl(StreamEngine, Md5, Fun, Acc);
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


range_foldl(Att, From, To, Fun, Acc) ->
    {stream, StreamEngine} = fetch(data, Att),
    couch_stream:range_foldl(StreamEngine, From, To, Fun, Acc).


foldl_decode(Att, Fun, Acc) ->
    case fetch([data, encoding], Att) of
        [{stream, StreamEngine}, Enc] ->
            couch_stream:foldl_decode(
                    StreamEngine, fetch(md5, Att), Enc, Fun, Acc);
        [Fun2, identity] ->
            fold_streamed_data(Fun2, fetch(att_len, Att), Fun, Acc)
    end.


to_binary(Att) ->
    to_binary(fetch(data, Att), Att).


to_binary(Bin, _Att) when is_binary(Bin) ->
    Bin;
to_binary(Iolist, _Att) when is_list(Iolist) ->
    iolist_to_binary(Iolist);
to_binary({stream, _StreamEngine}, Att) ->
    iolist_to_binary(
        lists:reverse(foldl(Att, fun(Bin,Acc) -> [Bin|Acc] end, []))
    );
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


%% Upgrade an attachment record to a property list on demand. This is a one-way
%% operation as downgrading potentially truncates fields with important data.
-spec upgrade(#att{}) -> attachment().
upgrade(#att{} = Att) ->
    Map = lists:zip(
        record_info(fields, att),
        lists:seq(2, record_info(size, att))
    ),
    %% Don't store undefined elements since that is default
    [{F, element(I, Att)} || {F, I} <- Map, element(I, Att) /= undefined];
upgrade(Att) ->
    Att.


%% Downgrade is exposed for interactive convenience. In practice, unless done
%% manually, upgrades are always one-way.
downgrade(#att{} = Att) ->
    Att;
downgrade(Att) ->
    #att{
        name = fetch(name, Att),
        type = fetch(type, Att),
        att_len = fetch(att_len, Att),
        disk_len = fetch(disk_len, Att),
        md5 = fetch(md5, Att),
        revpos = fetch(revpos, Att),
        data = fetch(data, Att),
        encoding = fetch(encoding, Att)
    }.


upgrade_encoding(true) -> gzip;
upgrade_encoding(false) -> identity;
upgrade_encoding(Encoding) -> Encoding.


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


open_stream(StreamSrc, Data) ->
    case couch_db:is_db(StreamSrc) of
        true ->
            couch_db:open_read_stream(StreamSrc, Data);
        false ->
            case is_function(StreamSrc, 1) of
                true ->
                    StreamSrc(Data);
                false ->
                    erlang:error({invalid_stream_source, StreamSrc})
            end
    end.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

% Eww...
-include("couch_bt_engine.hrl").

%% Test utilities


empty_att() -> new().


upgraded_empty_att() ->
    new([{headers, undefined}]).


%% Test groups


attachment_upgrade_test_() ->
    {"Lazy record upgrade tests", [
        {"Existing record fields don't upgrade",
            {with, empty_att(), [fun test_non_upgrading_fields/1]}
        },
        {"New fields upgrade",
            {with, empty_att(), [fun test_upgrading_fields/1]}
        }
    ]}.


attachment_defaults_test_() ->
    {"Attachment defaults tests", [
        {"Records retain old default values", [
            {with, empty_att(), [fun test_legacy_defaults/1]}
        ]},
        {"Upgraded records inherit defaults", [
            {with, upgraded_empty_att(), [fun test_legacy_defaults/1]}
        ]},
        {"Undefined entries are elided on upgrade", [
            {with, upgraded_empty_att(), [fun test_elided_entries/1]}
        ]}
    ]}.

attachment_field_api_test_() ->
    {"Basic attachment field api", [
        fun test_construction/0,
        fun test_store_and_fetch/0,
        fun test_transform/0
    ]}.


attachment_disk_term_test_() ->
    BaseAttachment = new([
        {name, <<"empty">>},
        {type, <<"application/octet-stream">>},
        {att_len, 0},
        {disk_len, 0},
        {md5, <<212,29,140,217,143,0,178,4,233,128,9,152,236,248,66,126>>},
        {revpos, 4},
        {data, {stream, {couch_bt_engine_stream, {fake_fd, fake_sp}}}},
        {encoding, identity}
    ]),
    BaseDiskTerm = {
        <<"empty">>,
        <<"application/octet-stream">>,
        fake_sp,
        0, 0, 4,
        <<212,29,140,217,143,0,178,4,233,128,9,152,236,248,66,126>>,
        identity
    },
    Headers = [{<<"X-Foo">>, <<"bar">>}],
    ExtendedAttachment = store(headers, Headers, BaseAttachment),
    ExtendedDiskTerm = {BaseDiskTerm, [{headers, Headers}]},
    FakeDb = test_util:fake_db([{engine, {couch_bt_engine, #st{fd=fake_fd}}}]),
    {"Disk term tests", [
        ?_assertEqual(BaseDiskTerm, to_disk_term(BaseAttachment)),
        ?_assertEqual(BaseAttachment, from_disk_term(FakeDb, BaseDiskTerm)),
        ?_assertEqual(ExtendedDiskTerm, to_disk_term(ExtendedAttachment)),
        ?_assertEqual(ExtendedAttachment, from_disk_term(FakeDb, ExtendedDiskTerm))
    ]}.


attachment_json_term_test_() ->
    Props = [
        {<<"content_type">>, <<"application/json">>},
        {<<"digest">>, <<"md5-QCNtWUNXV0UzJnEjMk92YUk1JA==">>},
        {<<"length">>, 14},
        {<<"revpos">>, 1}
    ],
    PropsInline = [{<<"data">>, <<"eyJhbnN3ZXIiOiA0Mn0=">>}] ++ Props,
    InvalidProps = [{<<"data">>, <<"!Base64Encoded$">>}] ++ Props,
    Att = couch_att:new([
        {name, <<"attachment.json">>},
        {type, <<"application/json">>}
    ]),
    ResultStub = couch_att:new([
        {name, <<"attachment.json">>},
        {type, <<"application/json">>},
        {att_len, 14},
        {disk_len, 14},
        {md5, <<"@#mYCWWE3&q#2OvaI5$">>},
        {revpos, 1},
        {data, stub},
        {encoding, identity}
    ]),
    ResultFollows = ResultStub#att{data = follows},
    ResultInline = ResultStub#att{md5 = <<>>, data = <<"{\"answer\": 42}">>},
    {"JSON term tests", [
        ?_assertEqual(ResultStub, stub_from_json(Att, Props)),
        ?_assertEqual(ResultFollows, follow_from_json(Att, Props)),
        ?_assertEqual(ResultInline, inline_from_json(Att, PropsInline)),
        ?_assertThrow({bad_request, _}, inline_from_json(Att, Props)),
        ?_assertThrow({bad_request, _}, inline_from_json(Att, InvalidProps))
    ]}.


attachment_stub_merge_test_() ->
    %% Stub merging needs to demonstrate revpos matching, skipping, and missing
    %% attachment errors.
    {"Attachment stub merging tests", []}.


%% Test generators


test_non_upgrading_fields(Attachment) ->
    Pairs = [
        {name, "cat.gif"},
        {type, "text/very-very-plain"},
        {att_len, 1024},
        {disk_len, 42},
        {md5, <<"md5-hashhashhash">>},
        {revpos, 4},
        {data, stub},
        {encoding, gzip}
    ],
    lists:foreach(
        fun({Field, Value}) ->
            ?assertMatch(#att{}, Attachment),
            Updated = store(Field, Value, Attachment),
            ?assertMatch(#att{}, Updated)
        end,
    Pairs).


test_upgrading_fields(Attachment) ->
    ?assertMatch(#att{}, Attachment),
    UpdatedHeaders = store(headers, [{<<"Ans">>, <<"42">>}], Attachment),
    ?assertMatch(X when is_list(X), UpdatedHeaders),
    UpdatedHeadersUndefined = store(headers, undefined, Attachment),
    ?assertMatch(X when is_list(X), UpdatedHeadersUndefined).


test_legacy_defaults(Attachment) ->
    ?assertEqual(<<>>, fetch(md5, Attachment)),
    ?assertEqual(0, fetch(revpos, Attachment)),
    ?assertEqual(identity, fetch(encoding, Attachment)).


test_elided_entries(Attachment) ->
    ?assertNot(lists:keymember(name, 1, Attachment)),
    ?assertNot(lists:keymember(type, 1, Attachment)),
    ?assertNot(lists:keymember(att_len, 1, Attachment)),
    ?assertNot(lists:keymember(disk_len, 1, Attachment)),
    ?assertNot(lists:keymember(data, 1, Attachment)).


test_construction() ->
    ?assert(new() == new()),
    Initialized = new([{name, <<"foo.bar">>}, {type, <<"application/qux">>}]),
    ?assertEqual(<<"foo.bar">>, fetch(name, Initialized)),
    ?assertEqual(<<"application/qux">>, fetch(type, Initialized)).


test_store_and_fetch() ->
    Attachment = empty_att(),
    ?assertEqual(<<"abc">>, fetch(name, store(name, <<"abc">>, Attachment))),
    ?assertEqual(42, fetch(ans, store(ans, 42, Attachment))).


test_transform() ->
    Attachment = new([{counter, 0}]),
    Transformed = transform(counter, fun(Count) -> Count + 1 end, Attachment),
    ?assertEqual(1, fetch(counter, Transformed)).


-endif.
