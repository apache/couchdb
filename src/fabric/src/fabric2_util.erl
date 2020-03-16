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

-module(fabric2_util).


-export([
    revinfo_to_revs/1,
    revinfo_to_path/1,
    sort_revinfos/1,
    rev_size/1,
    ldoc_size/1,

    seq_zero_vs/0,
    seq_max_vs/0,

    user_ctx_to_json/1,

    validate_security_object/1,

    hash_atts/1,

    dbname_ends_with/2,

    get_value/2,
    get_value/3,
    to_hex/1,
    from_hex/1,
    uuid/0,

    encode_all_doc_key/1,

    pmap/2,
    pmap/3
]).


-include_lib("couch/include/couch_db.hrl").


revinfo_to_revs(RevInfo) ->
    #{
        rev_id := {RevPos, Rev},
        rev_path := RevPath
    } = RevInfo,
    {RevPos, [Rev | RevPath]}.


revinfo_to_path(RevInfo) ->
    #{
        rev_id := {RevPos, Rev},
        rev_path := RevPath
    } = RevInfo,
    Revs = lists:reverse(RevPath, [Rev]),
    Path = revinfo_to_path(RevInfo, Revs),
    {RevPos - length(Revs) + 1, Path}.


revinfo_to_path(RevInfo, [Rev]) ->
    {Rev, RevInfo, []};

revinfo_to_path(RevInfo, [Rev | Rest]) ->
    {Rev, ?REV_MISSING, [revinfo_to_path(RevInfo, Rest)]}.


sort_revinfos(RevInfos) ->
    CmpFun = fun(A, B) -> rev_sort_key(A) > rev_sort_key(B) end,
    lists:sort(CmpFun, RevInfos).


rev_sort_key(#{} = RevInfo) ->
    #{
        deleted := Deleted,
        rev_id := {RevPos, Rev}
    } = RevInfo,
    {not Deleted, RevPos, Rev}.


rev_size(#doc{} = Doc) ->
    #doc{
        id = Id,
        revs = Revs,
        body = Body,
        atts = Atts
    } = Doc,

    {Start, Rev} = case Revs of
        {0, []} -> {0, <<>>};
        {N, [RevId | _]} -> {N, RevId}
    end,

    lists:sum([
        size(Id),
        size(erlfdb_tuple:pack({Start})),
        size(Rev),
        1, % FDB tuple encoding of booleans for deleted flag is 1 byte
        couch_ejson_size:encoded_size(Body),
        lists:foldl(fun(Att, Acc) ->
            couch_att:external_size(Att) + Acc
        end, 0, Atts)
    ]).


ldoc_size(#doc{id = <<"_local/", _/binary>>} = Doc) ->
    #doc{
        id = Id,
        revs = {0, [Rev]},
        deleted = Deleted,
        body = Body
    } = Doc,

    StoreRev = case Rev of
        _ when is_integer(Rev) -> integer_to_binary(Rev);
        _ when is_binary(Rev) -> Rev
    end,

    case Deleted of
        true ->
            0;
        false ->
            lists:sum([
                size(Id),
                size(StoreRev),
                couch_ejson_size:encoded_size(Body)
            ])
    end.


seq_zero_vs() ->
    {versionstamp, 0, 0, 0}.


seq_max_vs() ->
    {versionstamp, 18446744073709551615, 65535, 65535}.


user_ctx_to_json(Db) ->
    UserCtx = fabric2_db:get_user_ctx(Db),
    {[
        {<<"db">>, fabric2_db:name(Db)},
        {<<"name">>, UserCtx#user_ctx.name},
        {<<"roles">>, UserCtx#user_ctx.roles}
    ]}.


validate_security_object({SecProps}) ->
    Admins = get_value(<<"admins">>, SecProps, {[]}),
    ok = validate_names_and_roles(Admins),

    % we fallback to readers here for backwards compatibility
    Readers = get_value(<<"readers">>, SecProps, {[]}),
    Members = get_value(<<"members">>, SecProps, Readers),
    ok = validate_names_and_roles(Members).


validate_names_and_roles({Props}) when is_list(Props) ->
    validate_json_list_of_strings(<<"names">>, Props),
    validate_json_list_of_strings(<<"roles">>, Props);
validate_names_and_roles(_) ->
    throw("admins or members must be a JSON list of strings").


validate_json_list_of_strings(Member, Props) ->
    case get_value(Member, Props, []) of
        Values when is_list(Values) ->
            NonBinary = lists:filter(fun(V) -> not is_binary(V) end, Values),
            if NonBinary == [] -> ok; true ->
                MemberStr = binary_to_list(Member),
                throw(MemberStr ++ " must be a JSON list of strings")
            end;
        _ ->
            MemberStr = binary_to_list(Member),
            throw(MemberStr ++ " must be a JSON list of strings")
    end.


hash_atts([]) ->
    <<>>;

hash_atts(Atts) ->
    SortedAtts = lists:sort(fun(A, B) ->
        couch_att:fetch(name, A) =< couch_att:fetch(name, B)
    end, Atts),
    Md5St = lists:foldl(fun(Att, Acc) ->
        {loc, _Db, _DocId, AttId} = couch_att:fetch(data, Att),
        couch_hash:md5_hash_update(Acc, AttId)
    end, couch_hash:md5_hash_init(), SortedAtts),
    couch_hash:md5_hash_final(Md5St).


dbname_ends_with(#{} = Db, Suffix) ->
    dbname_ends_with(fabric2_db:name(Db), Suffix);

dbname_ends_with(DbName, Suffix) when is_binary(DbName), is_binary(Suffix) ->
    Suffix == filename:basename(DbName).


get_value(Key, List) ->
    get_value(Key, List, undefined).


get_value(Key, List, Default) ->
    case lists:keysearch(Key, 1, List) of
        {value, {Key,Value}} ->
            Value;
        false ->
            Default
    end.


to_hex(Bin) ->
    list_to_binary(to_hex_int(Bin)).


to_hex_int(<<>>) ->
    [];
to_hex_int(<<Hi:4, Lo:4, Rest/binary>>) ->
    [nibble_to_hex(Hi), nibble_to_hex(Lo) | to_hex(Rest)].


nibble_to_hex(I) ->
    case I of
        0 -> $0;
        1 -> $1;
        2 -> $2;
        3 -> $3;
        4 -> $4;
        5 -> $5;
        6 -> $6;
        7 -> $7;
        8 -> $8;
        9 -> $9;
        10 -> $a;
        11 -> $b;
        12 -> $c;
        13 -> $d;
        14 -> $e;
        15 -> $f
    end.


from_hex(Bin) ->
    iolist_to_binary(from_hex_int(Bin)).


from_hex_int(<<>>) ->
    [];
from_hex_int(<<Hi:8, Lo:8, RestBinary/binary>>) ->
    HiNib = hex_to_nibble(Hi),
    LoNib = hex_to_nibble(Lo),
    [<<HiNib:4, LoNib:4>> | from_hex_int(RestBinary)];
from_hex_int(<<BadHex/binary>>) ->
    erlang:error({invalid_hex, BadHex}).


hex_to_nibble(N) ->
    case N of
        $0 -> 0;
        $1 -> 1;
        $2 -> 2;
        $3 -> 3;
        $4 -> 4;
        $5 -> 5;
        $6 -> 6;
        $7 -> 7;
        $8 -> 8;
        $9 -> 9;
        $a -> 10;
        $A -> 10;
        $b -> 11;
        $B -> 11;
        $c -> 12;
        $C -> 12;
        $d -> 13;
        $D -> 13;
        $e -> 14;
        $E -> 14;
        $f -> 15;
        $F -> 15;
        _ -> erlang:error({invalid_hex, N})
    end.


uuid() ->
    to_hex(crypto:strong_rand_bytes(16)).


encode_all_doc_key(null) -> <<>>;
encode_all_doc_key(true) -> <<>>;
encode_all_doc_key(false) -> <<>>;
encode_all_doc_key(N) when is_number(N) -> <<>>;
encode_all_doc_key(B) when is_binary(B) -> B;
encode_all_doc_key(L) when is_list(L) -> <<255>>;
encode_all_doc_key({O}) when is_list(O) -> <<255>>.


pmap(Fun, Args) ->
    pmap(Fun, Args, []).


pmap(Fun, Args, Opts) ->
    Refs = lists:map(fun(Arg) ->
        {_, Ref} = spawn_monitor(fun() -> exit(pmap_exec(Fun, Arg)) end),
        Ref
    end, Args),
    Timeout = fabric2_util:get_value(timeout, Opts, 5000),
    lists:map(fun(Ref) ->
        receive
            {'DOWN', Ref, _, _, {'$res', Res}} ->
                Res;
            {'DOWN', Ref, _, _, {'$err', Tag, Reason, Stack}} ->
                erlang:raise(Tag, Reason, Stack)
        after Timeout ->
            error({pmap_timeout, Timeout})
        end
    end, Refs).


% OTP_RELEASE is defined in OTP 21+ only
-ifdef(OTP_RELEASE).

pmap_exec(Fun, Arg) ->
    try
        {'$res', Fun(Arg)}
    catch Tag:Reason:Stack ->
        {'$err', Tag, Reason, Stack}
    end.

-else.

pmap_exec(Fun, Arg) ->
    try
        {'$res', Fun(Arg)}
    catch Tag:Reason ->
        {'$err', Tag, Reason, erlang:get_stacktrace()}
    end.

-endif.
