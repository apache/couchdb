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

-export([priv_dir/0, start_driver/1,terminate_linked/1]).
-export([should_flush/0, should_flush/1, to_existing_atom/1]).
-export([rand32/0, implode/2, collate/2, collate/3]).
-export([abs_pathname/1,abs_pathname/2, trim/1, ascii_lower/1]).
-export([encodeBase64/1, decodeBase64/1, encodeBase64Url/1, decodeBase64Url/1,
    to_hex/1,parse_term/1, dict_find/3]).
-export([file_read_size/1, get_nested_json_value/2, json_user_ctx/1]).
-export([to_binary/1, to_integer/1, to_list/1, url_encode/1]).
-export([json_encode/1, json_decode/1]).

-include("couch_db.hrl").
-include_lib("kernel/include/file.hrl").

% arbitrarily chosen amount of memory to use before flushing to disk
-define(FLUSH_MAX_MEM, 10000000).

priv_dir() ->
    case code:priv_dir(couch) of
        {error, bad_name} ->
            % small hack, in dev mode "app" is couchdb. Fixing requires
            % renaming src/couch to src/couch. Not really worth the hassle.
            % -Damien
            code:priv_dir(couchdb);
        Dir -> Dir
    end.

start_driver(LibDir) ->
    case erl_ddll:load_driver(LibDir, "couch_erl_driver") of
    ok ->
        ok;
    {error, already_loaded} ->
        ok = erl_ddll:reload_driver(LibDir, "couch_erl_driver");
    {error, Error} ->
        exit(erl_ddll:format_error(Error))
    end.

% works like list_to_existing_atom, except can be list or binary and it
% gives you the original value instead of an error if no existing atom.
to_existing_atom(V) when is_list(V) ->
    try list_to_existing_atom(V) catch _ -> V end;
to_existing_atom(V) when is_binary(V) ->
    try list_to_existing_atom(?b2l(V)) catch _ -> V end;
to_existing_atom(V) when is_atom(V) ->
    V.


terminate_linked(normal) ->
    terminate_linked(shutdown);
terminate_linked(Reason) ->
    {links, Links} = process_info(self(), links),
    [catch exit(Pid, Reason) || Pid <- Links],
    ok.


to_hex([]) ->
    [];
to_hex(Bin) when is_binary(Bin) ->
    to_hex(binary_to_list(Bin));
to_hex([H|T]) ->
    [to_digit(H div 16), to_digit(H rem 16) | to_hex(T)].

to_digit(N) when N < 10 -> $0 + N;
to_digit(N)             -> $a + N-10.


parse_term(Bin) when is_binary(Bin) ->
    parse_term(binary_to_list(Bin));
parse_term(List) ->
    {ok, Tokens, _} = erl_scan:string(List ++ "."),
    erl_parse:parse_term(Tokens).


get_nested_json_value({Props}, [Key|Keys]) ->
    case proplists:get_value(Key, Props, nil) of
    nil -> throw({not_found, <<"missing json key: ", Key/binary>>});
    Value -> get_nested_json_value(Value, Keys)
    end;
get_nested_json_value(Value, []) ->
    Value;
get_nested_json_value(_NotJSONObj, _) ->
    throw({not_found, json_mismatch}).

json_user_ctx(#db{name=DbName, user_ctx=Ctx}) ->
    {[{<<"db">>, DbName},
            {<<"name">>,Ctx#user_ctx.name},
            {<<"roles">>,Ctx#user_ctx.roles}]}.
    

% returns a random integer
rand32() ->
    crypto:rand_uniform(0, 16#100000000).

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

% lowercases string bytes that are the ascii characters A-Z.
% All other characters/bytes are ignored.
ascii_lower(String) ->
    ascii_lower(String, []).

ascii_lower([], Acc) ->
    lists:reverse(Acc);
ascii_lower([Char | RestString], Acc) when Char >= $A, Char =< $B ->
    ascii_lower(RestString, [Char + ($a-$A) | Acc]);
ascii_lower([Char | RestString], Acc) ->
    ascii_lower(RestString, [Char | Acc]).

% Is a character whitespace?
is_whitespace($\s) -> true;
is_whitespace($\t) -> true;
is_whitespace($\n) -> true;
is_whitespace($\r) -> true;
is_whitespace(_Else) -> false.


% removes leading and trailing whitespace from a string
trim(String) ->
    String2 = lists:dropwhile(fun is_whitespace/1, String),
    lists:reverse(lists:dropwhile(fun is_whitespace/1, lists:reverse(String2))).

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


drv_port() ->
    case get(couch_drv_port) of
    undefined ->
        Port = open_port({spawn, "couch_erl_driver"}, []),
        put(couch_drv_port, Port),
        Port;
    Port ->
        Port
    end.

collate(A, B) ->
    collate(A, B, []).

collate(A, B, Options) when is_binary(A), is_binary(B) ->
    Operation =
    case lists:member(nocase, Options) of
        true -> 1; % Case insensitive
        false -> 0 % Case sensitive
    end,
    SizeA = byte_size(A),
    SizeB = byte_size(B),
    Bin = <<SizeA:32/native, A/binary, SizeB:32/native, B/binary>>,
    [Result] = erlang:port_control(drv_port(), Operation, Bin),
    % Result is 0 for lt, 1 for eq and 2 for gt. Subtract 1 to return the
    % expected typical -1, 0, 1
    Result - 1.

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


%%% Purpose : Base 64 encoding and decoding.
%%% Copied from ssl_base_64 to avoid using the
%%% erlang ssl library

-define(st(X,A), ((X-A+256) div 256)).

%% A PEM encoding consists of characters A-Z, a-z, 0-9, +, / and
%% =. Each character encodes a 6 bits value from 0 to 63 (A = 0, / =
%% 63); = is a padding character.
%%

%%
%% encode64(Bytes|Binary) -> binary
%%
%% Take 3 bytes a time (3 x 8 = 24 bits), and make 4 characters out of
%% them (4 x 6 = 24 bits).
%%
encodeBase64(Bs) when is_list(Bs) ->
    encodeBase64(iolist_to_binary(Bs), <<>>);
encodeBase64(Bs) ->
    encodeBase64(Bs, <<>>).

encodeBase64(<<B:3/binary, Bs/binary>>, Acc) ->
    <<C1:6, C2:6, C3:6, C4:6>> = B,
    encodeBase64(Bs, <<Acc/binary, (enc(C1)), (enc(C2)), (enc(C3)), (enc(C4))>>);
encodeBase64(<<B:2/binary>>, Acc) ->
    <<C1:6, C2:6, C3:6, _:6>> = <<B/binary, 0>>,
    <<Acc/binary, (enc(C1)), (enc(C2)), (enc(C3)), $=>>;
encodeBase64(<<B:1/binary>>, Acc) ->
    <<C1:6, C2:6, _:12>> = <<B/binary, 0, 0>>,
    <<Acc/binary, (enc(C1)), (enc(C2)), $=, $=>>;
encodeBase64(<<>>, Acc) ->
    Acc.

encodeBase64Url(Bs) when is_list(Bs) ->
    encodeBase64Url(list_to_binary(Bs), <<>>);
encodeBase64Url(Bs) ->
    encodeBase64Url(Bs, <<>>).
    
encodeBase64Url(<<B:3/binary, Bs/binary>>, Acc) ->
    <<C1:6, C2:6, C3:6, C4:6>> = B,
    encodeBase64Url(Bs, <<Acc/binary, (encUrl(C1)), (encUrl(C2)), (encUrl(C3)), (encUrl(C4))>>);
encodeBase64Url(<<B:2/binary>>, Acc) ->
    <<C1:6, C2:6, C3:6, _:6>> = <<B/binary, 0>>,
    <<Acc/binary, (encUrl(C1)), (encUrl(C2)), (encUrl(C3))>>;
encodeBase64Url(<<B:1/binary>>, Acc) ->
    <<C1:6, C2:6, _:12>> = <<B/binary, 0, 0>>,
    <<Acc/binary, (encUrl(C1)), (encUrl(C2))>>;
encodeBase64Url(<<>>, Acc) ->
    Acc.

%%
%% decodeBase64(BinaryChars) -> Binary
%%
decodeBase64(Cs) when is_list(Cs) ->
    decodeBase64(list_to_binary(Cs));
decodeBase64(Cs) ->
    decode1(Cs, <<>>).

decode1(<<C1, C2, $=, $=>>, Acc) ->
    <<B1, _:16>> = <<(dec(C1)):6, (dec(C2)):6, 0:12>>,
    <<Acc/binary, B1>>;
decode1(<<C1, C2, C3, $=>>, Acc) ->
    <<B1, B2, _:8>> = <<(dec(C1)):6, (dec(C2)):6, (dec(C3)):6, (dec(0)):6>>,
    <<Acc/binary, B1, B2>>;
decode1(<<C1, C2, C3, C4, Cs/binary>>, Acc) ->
    Bin = <<Acc/binary, (dec(C1)):6, (dec(C2)):6, (dec(C3)):6, (dec(C4)):6>>,
    decode1(Cs, Bin);
decode1(<<>>, Acc) ->
    Acc.

decodeBase64Url(Cs) when is_list(Cs) ->
    decodeBase64Url(list_to_binary(Cs));
decodeBase64Url(Cs) ->
    decode1Url(Cs, <<>>).

decode1Url(<<C1, C2>>, Acc) ->
    <<B1, _:16>> = <<(decUrl(C1)):6, (decUrl(C2)):6, 0:12>>,
    <<Acc/binary, B1>>;
decode1Url(<<C1, C2, C3>>, Acc) ->
    <<B1, B2, _:8>> = <<(decUrl(C1)):6, (decUrl(C2)):6, (decUrl(C3)):6, (decUrl(0)):6>>,
    <<Acc/binary, B1, B2>>;
decode1Url(<<C1, C2, C3, C4, Cs/binary>>, Acc) ->
    Bin = <<Acc/binary, (decUrl(C1)):6, (decUrl(C2)):6, (decUrl(C3)):6, (decUrl(C4)):6>>,
    decode1Url(Cs, Bin);
decode1Url(<<>>, Acc) ->
    Acc.

%% enc/1 and dec/1
%%
%% Mapping: 0-25 -> A-Z, 26-51 -> a-z, 52-61 -> 0-9, 62 -> +, 63 -> /
%%
enc(C) ->
    65 + C + 6*?st(C,26) - 75*?st(C,52) -15*?st(C,62) + 3*?st(C,63).

dec(C) ->
    62*?st(C,43) + ?st(C,47) + (C-59)*?st(C,48) - 69*?st(C,65) - 6*?st(C,97).

%% encUrl/1 and decUrl/1
%%
%% Mapping: 0-25 -> A-Z, 26-51 -> a-z, 52-61 -> 0-9, 62 -> -, 63 -> _
%%
encUrl(C) ->
    65 + C + 6*?st(C,26) - 75*?st(C,52) -13*?st(C,62) + 49*?st(C,63).

decUrl(C) ->
    62*?st(C,45) + (C-58)*?st(C,48) - 69*?st(C,65) + 33*?st(C,95) - 39*?st(C,97).

dict_find(Key, Dict, DefaultValue) ->
    case dict:find(Key, Dict) of
    {ok, Value} ->
        Value;
    error ->
        DefaultValue
    end.


file_read_size(FileName) ->
    case file:read_file_info(FileName) of
        {ok, FileInfo} ->
            FileInfo#file_info.size;
        Error -> Error
    end.

to_binary(V) when is_binary(V) ->
    V;
to_binary(V) when is_list(V) ->
    try
        list_to_binary(V)
    catch
        _ ->
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
    Handler =
    fun({L}) when is_list(L) ->
        {struct,L};
    (Bad) ->
        exit({json_encode, {bad_term, Bad}})
    end,
    (mochijson2:encoder([{handler, Handler}]))(V).

json_decode(V) ->
    try (mochijson2:decoder([{object_hook, fun({struct,L}) -> {L} end}]))(V)
    catch _:_ -> throw({invalid_json,V}) end.
