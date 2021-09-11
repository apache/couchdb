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

-module(couch_util).

-export([priv_dir/0, normpath/1, fold_files/5]).
-export([should_flush/0, should_flush/1, to_existing_atom/1]).
-export([rand32/0, implode/2]).
-export([abs_pathname/1,abs_pathname/2, trim/1, drop_dot_couch_ext/1]).
-export([encodeBase64Url/1, decodeBase64Url/1]).
-export([validate_utf8/1, to_hex/1, parse_term/1, dict_find/3]).
-export([get_nested_json_value/2, json_user_ctx/1]).
-export([proplist_apply_field/2, json_apply_field/2]).
-export([to_binary/1, to_integer/1, to_list/1, url_encode/1]).
-export([json_encode/1, json_decode/1, json_decode/2]).
-export([verify/2,simple_call/2,shutdown_sync/1]).
-export([get_value/2, get_value/3]).
-export([reorder_results/2]).
-export([url_strip_password/1]).
-export([encode_doc_id/1]).
-export([normalize_ddoc_id/1]).
-export([with_db/2]).
-export([rfc1123_date/0, rfc1123_date/1]).
-export([integer_to_boolean/1, boolean_to_integer/1]).
-export([validate_positive_int/1]).
-export([find_in_binary/2]).
-export([callback_exists/3, validate_callback_exists/3]).
-export([with_proc/4]).
-export([process_dict_get/2, process_dict_get/3]).
-export([unique_monotonic_integer/0]).
-export([check_config_blacklist/1]).
-export([check_md5/2]).
-export([set_mqd_off_heap/1]).
-export([set_process_priority/2]).
-export([hmac/3]).

-include_lib("couch/include/couch_db.hrl").

% arbitrarily chosen amount of memory to use before flushing to disk
-define(FLUSH_MAX_MEM, 10000000).

-define(BLACKLIST_CONFIG_SECTIONS, [
    <<"daemons">>,
    <<"external">>,
    <<"httpd_design_handlers">>,
    <<"httpd_db_handlers">>,
    <<"httpd_global_handlers">>,
    <<"native_query_servers">>,
    <<"os_daemons">>,
    <<"query_servers">>,
    <<"feature_flags">>
]).


priv_dir() ->
    case code:priv_dir(couch) of
        {error, bad_name} ->
            % small hack, in dev mode "app" is couchdb. Fixing requires
            % renaming src/couch to src/couch. Not really worth the hassle.
            % -Damien
            code:priv_dir(couchdb);
        Dir -> Dir
    end.

% Normalize a pathname by removing .. and . components.
normpath(Path) ->
    normparts(filename:split(Path), []).

normparts([], Acc) ->
    filename:join(lists:reverse(Acc));
normparts([".." | RestParts], [_Drop | RestAcc]) ->
    normparts(RestParts, RestAcc);
normparts(["." | RestParts], Acc) ->
    normparts(RestParts, Acc);
normparts([Part | RestParts], Acc) ->
    normparts(RestParts, [Part | Acc]).


% This is implementation is similar the builtin filelib:fold_files/5
% except that this version will run the user supplied function
% on directories that match the regular expression as well.
%
% This is motivated by the case when couch_server is searching
% for pluggable storage engines. This change allows a
% database to be either a file or a directory.
fold_files(Dir, RegExp, Recursive, Fun, Acc) ->
    {ok, Re} = re:compile(RegExp, [unicode]),
    fold_files1(Dir, Re, Recursive, Fun, Acc).

fold_files1(Dir, RegExp, Recursive, Fun, Acc) ->
    case file:list_dir(Dir) of
        {ok, Files} ->
            fold_files2(Files, Dir, RegExp, Recursive, Fun, Acc);
        {error, _} ->
            Acc
    end.

fold_files2([], _Dir, _RegExp, _Recursive, _Fun, Acc) ->
    Acc;
fold_files2([File | Rest], Dir, RegExp, Recursive, Fun, Acc0) ->
    FullName = filename:join(Dir, File),
    case (catch re:run(File, RegExp, [{capture, none}])) of
        match ->
            Acc1 = Fun(FullName, Acc0),
            fold_files2(Rest, Dir, RegExp, Recursive, Fun, Acc1);
        _ ->
            case Recursive andalso filelib:is_dir(FullName) of
                true ->
                    Acc1 = fold_files1(FullName, RegExp, Recursive, Fun, Acc0),
                    fold_files2(Rest, Dir, RegExp, Recursive, Fun, Acc1);
                false ->
                    fold_files2(Rest, Dir, RegExp, Recursive, Fun, Acc0)
            end
    end.

% works like list_to_existing_atom, except can be list or binary and it
% gives you the original value instead of an error if no existing atom.
to_existing_atom(V) when is_list(V) ->
    try list_to_existing_atom(V) catch _:_ -> V end;
to_existing_atom(V) when is_binary(V) ->
    try list_to_existing_atom(?b2l(V)) catch _:_ -> V end;
to_existing_atom(V) when is_atom(V) ->
    V.

shutdown_sync(Pid) when not is_pid(Pid)->
    ok;
shutdown_sync(Pid) ->
    MRef = erlang:monitor(process, Pid),
    try
        catch unlink(Pid),
        catch exit(Pid, shutdown),
        receive
        {'DOWN', MRef, _, _, _} ->
            ok
        end
    after
        erlang:demonitor(MRef, [flush])
    end.


simple_call(Pid, Message) ->
    MRef = erlang:monitor(process, Pid),
    try
        Pid ! {self(), Message},
        receive
        {Pid, Result} ->
            Result;
        {'DOWN', MRef, _, _, Reason} ->
            exit(Reason)
        end
    after
        erlang:demonitor(MRef, [flush])
    end.

validate_utf8(Data) when is_list(Data) ->
    validate_utf8(?l2b(Data));
validate_utf8(Bin) when is_binary(Bin) ->
    validate_utf8_fast(Bin, 0).

validate_utf8_fast(B, O) ->
    case B of
        <<_:O/binary>> ->
            true;
        <<_:O/binary, C1, _/binary>> when
                C1 < 128 ->
            validate_utf8_fast(B, 1 + O);
        <<_:O/binary, C1, C2, _/binary>> when
                C1 >= 194, C1 =< 223,
                C2 >= 128, C2 =< 191 ->
            validate_utf8_fast(B, 2 + O);
        <<_:O/binary, C1, C2, C3, _/binary>> when
                C1 >= 224, C1 =< 239,
                C2 >= 128, C2 =< 191,
                C3 >= 128, C3 =< 191 ->
            validate_utf8_fast(B, 3 + O);
        <<_:O/binary, C1, C2, C3, C4, _/binary>> when
                C1 >= 240, C1 =< 244,
                C2 >= 128, C2 =< 191,
                C3 >= 128, C3 =< 191,
                C4 >= 128, C4 =< 191 ->
            validate_utf8_fast(B, 4 + O);
        _ ->
            false
    end.


to_hex(<<Hi:4, Lo:4, Rest/binary>>) ->
    [nibble_to_hex(Hi), nibble_to_hex(Lo) | to_hex(Rest)];
to_hex(<<>>) ->
    [];
to_hex(List) when is_list(List) ->
    to_hex(list_to_binary(List)).

nibble_to_hex(0) -> $0;
nibble_to_hex(1) -> $1;
nibble_to_hex(2) -> $2;
nibble_to_hex(3) -> $3;
nibble_to_hex(4) -> $4;
nibble_to_hex(5) -> $5;
nibble_to_hex(6) -> $6;
nibble_to_hex(7) -> $7;
nibble_to_hex(8) -> $8;
nibble_to_hex(9) -> $9;
nibble_to_hex(10) -> $a;
nibble_to_hex(11) -> $b;
nibble_to_hex(12) -> $c;
nibble_to_hex(13) -> $d;
nibble_to_hex(14) -> $e;
nibble_to_hex(15) -> $f.


parse_term(Bin) when is_binary(Bin) ->
    parse_term(binary_to_list(Bin));
parse_term(List) ->
    {ok, Tokens, _} = erl_scan:string(List ++ "."),
    erl_parse:parse_term(Tokens).

get_value(Key, List) ->
    get_value(Key, List, undefined).

get_value(Key, List, Default) ->
    case lists:keysearch(Key, 1, List) of
    {value, {Key,Value}} ->
        Value;
    false ->
        Default
    end.

get_nested_json_value({Props}, [Key|Keys]) ->
    case couch_util:get_value(Key, Props, nil) of
    nil -> throw({not_found, <<"missing json key: ", Key/binary>>});
    Value -> get_nested_json_value(Value, Keys)
    end;
get_nested_json_value(Value, []) ->
    Value;
get_nested_json_value(_NotJSONObj, _) ->
    throw({not_found, json_mismatch}).

proplist_apply_field(H, L) ->
    {R} = json_apply_field(H, {L}),
    R.

json_apply_field(H, {L}) ->
    json_apply_field(H, L, []).
json_apply_field({Key, NewValue}, [{Key, _OldVal} | Headers], Acc) ->
    json_apply_field({Key, NewValue}, Headers, Acc);
json_apply_field({Key, NewValue}, [{OtherKey, OtherVal} | Headers], Acc) ->
    json_apply_field({Key, NewValue}, Headers, [{OtherKey, OtherVal} | Acc]);
json_apply_field({Key, NewValue}, [], Acc) ->
    {[{Key, NewValue}|Acc]}.

json_user_ctx(Db) ->
    ShardName = couch_db:name(Db),
    Ctx = couch_db:get_user_ctx(Db),
    {[{<<"db">>, mem3:dbname(ShardName)},
            {<<"name">>,Ctx#user_ctx.name},
            {<<"roles">>,Ctx#user_ctx.roles}]}.


% returns a random integer
rand32() ->
    <<I:32>> = crypto:strong_rand_bytes(4),
    I.

% given a pathname "../foo/bar/" it gives back the fully qualified
% absolute pathname.
abs_pathname(" " ++ Filename) ->
    % strip leading whitspace
    abs_pathname(Filename);
abs_pathname([$/ |_]=Filename) ->
    Filename;
abs_pathname(Filename) ->
    {ok, Cwd} = file:get_cwd(),
    {Filename2, Args} = separate_cmd_args(Filename, ""),
    abs_pathname(Filename2, Cwd) ++ Args.

abs_pathname(Filename, Dir) ->
    Name = filename:absname(Filename, Dir ++ "/"),
    OutFilename = filename:join(fix_path_list(filename:split(Name), [])),
    % If the filename is a dir (last char slash, put back end slash
    case string:right(Filename,1) of
    "/" ->
        OutFilename ++ "/";
    "\\" ->
        OutFilename ++ "/";
    _Else->
        OutFilename
    end.

% if this as an executable with arguments, seperate out the arguments
% ""./foo\ bar.sh -baz=blah" -> {"./foo\ bar.sh", " -baz=blah"}
separate_cmd_args("", CmdAcc) ->
    {lists:reverse(CmdAcc), ""};
separate_cmd_args("\\ " ++ Rest, CmdAcc) -> % handle skipped value
    separate_cmd_args(Rest, " \\" ++ CmdAcc);
separate_cmd_args(" " ++ Rest, CmdAcc) ->
    {lists:reverse(CmdAcc), " " ++ Rest};
separate_cmd_args([Char|Rest], CmdAcc) ->
    separate_cmd_args(Rest, [Char | CmdAcc]).

% Is a character whitespace (from https://en.wikipedia.org/wiki/Whitespace_character#Unicode)?
is_whitespace(9) -> true;
is_whitespace(10) -> true;
is_whitespace(11) -> true;
is_whitespace(12) -> true;
is_whitespace(13) -> true;
is_whitespace(32) -> true;
is_whitespace(133) -> true;
is_whitespace(160) -> true;
is_whitespace(5760) -> true;
is_whitespace(8192) -> true;
is_whitespace(8193) -> true;
is_whitespace(8194) -> true;
is_whitespace(8195) -> true;
is_whitespace(8196) -> true;
is_whitespace(8197) -> true;
is_whitespace(8198) -> true;
is_whitespace(8199) -> true;
is_whitespace(8200) -> true;
is_whitespace(8201) -> true;
is_whitespace(8202) -> true;
is_whitespace(8232) -> true;
is_whitespace(8233) -> true;
is_whitespace(8239) -> true;
is_whitespace(8287) -> true;
is_whitespace(12288) -> true;
is_whitespace(6158) -> true;
is_whitespace(8203) -> true;
is_whitespace(8204) -> true;
is_whitespace(8205) -> true;
is_whitespace(8288) -> true;
is_whitespace(65279) -> true;
is_whitespace(_Else) -> false.


% removes leading and trailing whitespace from a string
trim(String) when is_binary(String) ->
    % mirror string:trim() behaviour of returning a binary when a binary is passed in
    ?l2b(trim(?b2l(String)));
trim(String) ->
    String2 = lists:dropwhile(fun is_whitespace/1, String),
    lists:reverse(lists:dropwhile(fun is_whitespace/1, lists:reverse(String2))).


drop_dot_couch_ext(DbName) when is_binary(DbName) ->
    PrefixLen = size(DbName) - 6,
    case DbName of
        <<Prefix:PrefixLen/binary, ".couch">> ->
            Prefix;
        Else ->
            Else
    end;

drop_dot_couch_ext(DbName) when is_list(DbName) ->
    binary_to_list(drop_dot_couch_ext(iolist_to_binary(DbName))).


% takes a heirarchical list of dirs and removes the dots ".", double dots
% ".." and the corresponding parent dirs.
fix_path_list([], Acc) ->
    lists:reverse(Acc);
fix_path_list([".."|Rest], [_PrevAcc|RestAcc]) ->
    fix_path_list(Rest, RestAcc);
fix_path_list(["."|Rest], Acc) ->
    fix_path_list(Rest, Acc);
fix_path_list([Dir | Rest], Acc) ->
    fix_path_list(Rest, [Dir | Acc]).


implode(List, Sep) ->
    implode(List, Sep, []).

implode([], _Sep, Acc) ->
    lists:flatten(lists:reverse(Acc));
implode([H], Sep, Acc) ->
    implode([], Sep, [H|Acc]);
implode([H|T], Sep, Acc) ->
    implode(T, Sep, [Sep,H|Acc]).


should_flush() ->
    should_flush(?FLUSH_MAX_MEM).

should_flush(MemThreshHold) ->
    {memory, ProcMem} = process_info(self(), memory),
    BinMem = lists:foldl(fun({_Id, Size, _NRefs}, Acc) -> Size+Acc end,
        0, element(2,process_info(self(), binary))),
    if ProcMem+BinMem > 2*MemThreshHold ->
        garbage_collect(),
        {memory, ProcMem2} = process_info(self(), memory),
        BinMem2 = lists:foldl(fun({_Id, Size, _NRefs}, Acc) -> Size+Acc end,
            0, element(2,process_info(self(), binary))),
        ProcMem2+BinMem2 > MemThreshHold;
    true -> false end.

encodeBase64Url(Url) ->
    b64url:encode(Url).

decodeBase64Url(Url64) ->
    b64url:decode(Url64).

dict_find(Key, Dict, DefaultValue) ->
    case dict:find(Key, Dict) of
    {ok, Value} ->
        Value;
    error ->
        DefaultValue
    end.

to_binary(V) when is_binary(V) ->
    V;
to_binary(V) when is_list(V) ->
    try
        list_to_binary(V)
    catch
        _:_ ->
            list_to_binary(io_lib:format("~p", [V]))
    end;
to_binary(V) when is_atom(V) ->
    list_to_binary(atom_to_list(V));
to_binary(V) ->
    list_to_binary(io_lib:format("~p", [V])).

to_integer(V) when is_integer(V) ->
    V;
to_integer(V) when is_list(V) ->
    erlang:list_to_integer(V);
to_integer(V) when is_binary(V) ->
    erlang:list_to_integer(binary_to_list(V)).

to_list(V) when is_list(V) ->
    V;
to_list(V) when is_binary(V) ->
    binary_to_list(V);
to_list(V) when is_atom(V) ->
    atom_to_list(V);
to_list(V) ->
    lists:flatten(io_lib:format("~p", [V])).

url_encode(Bin) when is_binary(Bin) ->
    url_encode(binary_to_list(Bin));
url_encode([H|T]) ->
    if
    H >= $a, $z >= H ->
        [H|url_encode(T)];
    H >= $A, $Z >= H ->
        [H|url_encode(T)];
    H >= $0, $9 >= H ->
        [H|url_encode(T)];
    H == $_; H == $.; H == $-; H == $: ->
        [H|url_encode(T)];
    true ->
        case lists:flatten(io_lib:format("~.16.0B", [H])) of
        [X, Y] ->
            [$%, X, Y | url_encode(T)];
        [X] ->
            [$%, $0, X | url_encode(T)]
        end
    end;
url_encode([]) ->
    [].

json_encode(V) ->
    jiffy:encode(V, [force_utf8]).

json_decode(V) ->
    json_decode(V, []).

json_decode(V, Opts) ->
    try
        jiffy:decode(V, [dedupe_keys | Opts])
    catch
        error:Error ->
            throw({invalid_json, Error})
    end.

verify([X|RestX], [Y|RestY], Result) ->
    verify(RestX, RestY, (X bxor Y) bor Result);
verify([], [], Result) ->
    Result == 0.

verify(<<X/binary>>, <<Y/binary>>) ->
    verify(?b2l(X), ?b2l(Y));
verify(X, Y) when is_list(X) and is_list(Y) ->
    case length(X) == length(Y) of
        true ->
            verify(X, Y, 0);
        false ->
            false
    end;
verify(_X, _Y) -> false.

% linear search is faster for small lists, length() is 0.5 ms for 100k list
reorder_results(Keys, SortedResults) when length(Keys) < 100 ->
    [couch_util:get_value(Key, SortedResults) || Key <- Keys];
reorder_results(Keys, SortedResults) ->
    KeyDict = dict:from_list(SortedResults),
    [dict:fetch(Key, KeyDict) || Key <- Keys].

url_strip_password(Url) ->
    re:replace(Url,
        "(http|https|socks5)://([^:]+):[^@]+@(.*)$",
        "\\1://\\2:*****@\\3",
        [{return, list}]).

encode_doc_id(#doc{id = Id}) ->
    encode_doc_id(Id);
encode_doc_id(Id) when is_list(Id) ->
    encode_doc_id(?l2b(Id));
encode_doc_id(<<"_design/", Rest/binary>>) ->
    "_design/" ++ url_encode(Rest);
encode_doc_id(<<"_local/", Rest/binary>>) ->
    "_local/" ++ url_encode(Rest);
encode_doc_id(Id) ->
    url_encode(Id).

normalize_ddoc_id(<<"_design/", _/binary>> = DDocId) ->
    DDocId;
normalize_ddoc_id(DDocId) when is_binary(DDocId) ->
    <<"_design/", DDocId/binary>>.

with_db(DbName, Fun)  when is_binary(DbName) ->
    case couch_db:open_int(DbName, [?ADMIN_CTX]) of
        {ok, Db} ->
            try
                Fun(Db)
            after
                catch couch_db:close(Db)
            end;
        Else ->
            throw(Else)
    end;
with_db(Db, Fun) ->
    case couch_db:is_db(Db) of
        true ->
            Fun(Db);
        false ->
            erlang:error({invalid_db, Db})
    end.

rfc1123_date() ->
    {{YYYY,MM,DD},{Hour,Min,Sec}} = calendar:universal_time(),
    DayNumber = calendar:day_of_the_week({YYYY,MM,DD}),
    lists:flatten(
      io_lib:format("~s, ~2.2.0w ~3.s ~4.4.0w ~2.2.0w:~2.2.0w:~2.2.0w GMT",
            [day(DayNumber),DD,month(MM),YYYY,Hour,Min,Sec])).

rfc1123_date(undefined) ->
    undefined;
rfc1123_date(UniversalTime) ->
    {{YYYY,MM,DD},{Hour,Min,Sec}} = UniversalTime,
    DayNumber = calendar:day_of_the_week({YYYY,MM,DD}),
    lists:flatten(
      io_lib:format("~s, ~2.2.0w ~3.s ~4.4.0w ~2.2.0w:~2.2.0w:~2.2.0w GMT",
            [day(DayNumber),DD,month(MM),YYYY,Hour,Min,Sec])).

%% day

day(1) -> "Mon";
day(2) -> "Tue";
day(3) -> "Wed";
day(4) -> "Thu";
day(5) -> "Fri";
day(6) -> "Sat";
day(7) -> "Sun".

%% month

month(1) -> "Jan";
month(2) -> "Feb";
month(3) -> "Mar";
month(4) -> "Apr";
month(5) -> "May";
month(6) -> "Jun";
month(7) -> "Jul";
month(8) -> "Aug";
month(9) -> "Sep";
month(10) -> "Oct";
month(11) -> "Nov";
month(12) -> "Dec".

integer_to_boolean(1) ->
    true;
integer_to_boolean(0) ->
    false.

boolean_to_integer(true) ->
    1;
boolean_to_integer(false) ->
    0.


validate_positive_int(N) when is_list(N) ->
    try
        I = list_to_integer(N),
        validate_positive_int(I)
    catch error:badarg ->
        false
    end;
validate_positive_int(N) when is_integer(N), N > 0 -> true;
validate_positive_int(_) -> false.


find_in_binary(_B, <<>>) ->
    not_found;

find_in_binary(B, Data) ->
    case binary:match(Data, [B], []) of
    nomatch ->
        MatchLength = erlang:min(byte_size(B), byte_size(Data)),
        match_prefix_at_end(binary:part(B, {0, MatchLength}),
                            binary:part(Data, {byte_size(Data), -MatchLength}),
                            MatchLength, byte_size(Data) - MatchLength);
    {Pos, _Len} ->
        {exact, Pos}
    end.

match_prefix_at_end(Prefix, Data, PrefixLength, N) ->
    FirstCharMatches = binary:matches(Data, [binary:part(Prefix, {0, 1})], []),
    match_rest_of_prefix(FirstCharMatches, Prefix, Data, PrefixLength, N).

match_rest_of_prefix([], _Prefix, _Data, _PrefixLength, _N) ->
    not_found;

match_rest_of_prefix([{Pos, _Len} | Rest], Prefix, Data, PrefixLength, N) ->
    case binary:match(binary:part(Data, {PrefixLength, Pos - PrefixLength}),
                      [binary:part(Prefix, {0, PrefixLength - Pos})], []) of
        nomatch ->
            match_rest_of_prefix(Rest, Prefix, Data, PrefixLength, N);
        {_Pos, _Len1} ->
            {partial, N + Pos}
    end.

callback_exists(Module, Function, Arity) ->
    case ensure_loaded(Module) of
    true ->
        InfoList = Module:module_info(exports),
        lists:member({Function, Arity}, InfoList);
    false ->
        false
    end.

validate_callback_exists(Module, Function, Arity) ->
    case callback_exists(Module, Function, Arity) of
    true ->
        ok;
    false ->
        CallbackStr = lists:flatten(
            io_lib:format("~w:~w/~w", [Module, Function, Arity])),
        throw({error,
            {undefined_callback, CallbackStr, {Module, Function, Arity}}})
    end.


check_md5(_NewSig, <<>>) -> ok;
check_md5(Sig, Sig) -> ok;
check_md5(_, _) -> throw(md5_mismatch).


set_mqd_off_heap(Module) ->
    case config:get_boolean("off_heap_mqd", atom_to_list(Module), true) of
        true ->
            try
                erlang:process_flag(message_queue_data, off_heap),
                ok
            catch error:badarg ->
                    ok
            end;
        false ->
            ok
    end.


set_process_priority(Module, Level) ->
    case config:get_boolean("process_priority", atom_to_list(Module), false) of
        true ->
            process_flag(priority, Level),
            ok;
        false ->
            ok
    end.


ensure_loaded(Module) when is_atom(Module) ->
    case code:ensure_loaded(Module) of
    {module, Module} ->
        true;
    {error, embedded} ->
        true;
    {error, _} ->
        false
    end;
ensure_loaded(_Module) -> false.


%% This is especially useful in gen_servers when you need to call
%% a function that does a receive as it would hijack incoming messages.
with_proc(M, F, A, Timeout) ->
    {Pid, Ref} = spawn_monitor(fun() ->
        exit({reply, erlang:apply(M, F, A)})
    end),
    receive
        {'DOWN', Ref, process, Pid, {reply, Resp}} ->
            {ok, Resp};
        {'DOWN', Ref, process, Pid, Error} ->
            {error, Error}
    after Timeout ->
        erlang:demonitor(Ref, [flush]),
        {error, timeout}
    end.


process_dict_get(Pid, Key) ->
    process_dict_get(Pid, Key, undefined).


process_dict_get(Pid, Key, DefaultValue) ->
    case process_info(Pid, dictionary) of
        {dictionary, Dict} ->
            case lists:keyfind(Key, 1, Dict) of
                false ->
                    DefaultValue;
                {Key, Value} ->
                    Value
            end;
        undefined ->
            DefaultValue
    end.


unique_monotonic_integer() ->
    erlang:unique_integer([monotonic, positive]).


check_config_blacklist(Section) ->
    case lists:member(Section, ?BLACKLIST_CONFIG_SECTIONS) of
    true ->
        Msg = <<"Config section blacklisted for modification over HTTP API.">>,
        throw({forbidden, Msg});
    _ ->
        ok
    end.


-ifdef(OTP_RELEASE).

-if(?OTP_RELEASE >= 22).

% OTP >= 22
hmac(Alg, Key, Message) ->
    crypto:mac(hmac, Alg, Key, Message).

-else.

% OTP >= 21, < 22
hmac(Alg, Key, Message) ->
    crypto:hmac(Alg, Key, Message).

-endif. % -if(?OTP_RELEASE >= 22)

-else.

% OTP < 21
hmac(Alg, Key, Message) ->
    crypto:hmac(Alg, Key, Message).

-endif. % -ifdef(OTP_RELEASE)
