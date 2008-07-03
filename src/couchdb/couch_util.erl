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

-module(couch_util).

-export([start_driver/1]).
-export([parse_ini/1,should_flush/0, should_flush/1]).
-export([new_uuid/0, rand32/0, implode/2, collate/2, collate/3]).
-export([abs_pathname/1,abs_pathname/2, trim/1, ascii_lower/1]).
-export([encodeBase64/1, decodeBase64/1, to_hex/1]).


% arbitrarily chosen amount of memory to use before flushing to disk
-define(FLUSH_MAX_MEM, 10000000).

start_driver("") ->
    start_driver(filename:join(code:priv_dir(couch), "lib"));
start_driver(LibDir) ->
    case erl_ddll:load_driver(LibDir, "couch_erl_driver") of
    ok -> ok;
    {error, already_loaded} -> ok;
    {error, Error} -> exit(erl_ddll:format_error(Error))
    end.
    
    
    
new_uuid() ->
    to_hex(crypto:rand_bytes(16)).
    
to_hex([]) ->
    [];
to_hex(Bin) when is_binary(Bin) ->
    to_hex(binary_to_list(Bin));
to_hex([H|T]) ->
    [to_digit(H div 16), to_digit(H rem 16) | to_hex(T)].

to_digit(N) when N < 10 -> $0 + N;
to_digit(N)             -> $a + N-10.
    

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
ascii_lower([Char | RestString], Acc)->
    ascii_lower(RestString, [Char | Acc]).

% Is a character whitespace?
is_whitespace($\s)-> true;
is_whitespace($\t)-> true;
is_whitespace($\n)-> true;
is_whitespace($\r)-> true;
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


% This is a simple ini parser. it just converts the string
% contents of a file like this:
%
%; comments are ignored
%;commentedoutvariable=foo
%this is line that gets ignored because it has no equals sign
%[this line gets ignored because it starts with a bracket but doesn't end with one
%bloodtype=Ragu
%[Some Section]
%timeout=30
%Default=zuh ; another comment (leading space or tab before a semi is necessary to be a comment if not at beginning of line)
%[Another Section]
%key with spaces=a value with stuff; and no comment
%oops="it doesn't qet quoted strings with semis quite right ; it thinks it's part comment"
%
%And converts it into this:
%[{{"","bloodtype"},"Ragu"},
% {{"Some Section","timeout"},"30"},
% {{"Some section","Default"}, "zuh"},
% {{"Another Section", "key with spaces"}, "a value with stuff; and no comment"},
% {{"Another Section", "oops"}, "\"it doesn't qet quoted strings with semis quite right"}]
%

parse_ini(FileContents) ->
    {ok, Lines} = regexp:split(FileContents, "\r\n|\n|\r|\032"),
    {_, ParsedIniValues} =
    lists:foldl(fun(Line, {AccSectionName, AccValues}) ->
            case string:strip(Line) of
            "[" ++ Rest ->
                case regexp:split(Rest, "\\]") of
                {ok, [NewSectionName, ""]} ->
                    {NewSectionName, AccValues};
                _Else -> % end bracket not at end, ignore this line
                    {AccSectionName, AccValues}
                end;
            ";" ++ _Comment ->
                {AccSectionName, AccValues};
            Line2 ->
                case regexp:split(Line2, "=") of
                {ok, [_SingleElement]} -> % no "=" found, ignore this line
                    {AccSectionName, AccValues};
                {ok, [""|_LineValues]} -> % line begins with "=", ignore
                    {AccSectionName, AccValues};
                {ok, [ValueName|LineValues]} -> % yeehaw, got a line!
                    RemainingLine = implode(LineValues, "="),
                    {ok, [LineValue | _Rest]} = regexp:split(RemainingLine, " ;|\t;"), % removes comments
                    {AccSectionName, [{{AccSectionName, ValueName}, LineValue} | AccValues]}
                end
            end
        end, {"", []}, Lines),
    {ok, lists:reverse(ParsedIniValues)}.

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

collate(A, B, Options) when is_list(A), is_list(B) ->
    Operation =
    case lists:member(nocase, Options) of
        true -> 1; % Case insensitive
        false -> 0 % Case sensitive
    end,
    Port = drv_port(),
    LenA = length(A),
    LenB = length(B),
    Bin = list_to_binary([<<LenA:32/native>>, A, <<LenB:32/native>>, B]),
    case erlang:port_control(Port, Operation, Bin) of
        [0] -> -1;
        [1] -> 1;
        [2] -> 0
    end.

should_flush() ->
    should_flush(?FLUSH_MAX_MEM).
    
should_flush(MemThreshHold) ->
    case process_info(self(), memory) of
    {memory, Mem} when Mem > 2*MemThreshHold ->
        garbage_collect(),
        case process_info(self(), memory) of
        {memory, Mem} when Mem > MemThreshHold ->
            true;
        _ ->
            false
        end;
    _ ->
        false
    end.



%%% Purpose : Base 64 encoding and decoding.
%%% Copied from ssl_base_64 to avoid using the
%%% erlang ssl library

-define(st(X,A), ((X-A+256) div 256)).
-define(CHARS, 64).

%% A PEM encoding consists of characters A-Z, a-z, 0-9, +, / and
%% =. Each character encodes a 6 bits value from 0 to 63 (A = 0, / =
%% 63); = is a padding character.
%%

%%
%% encode64(Bytes|Binary) -> Chars
%%
%% Take 3 bytes a time (3 x 8 = 24 bits), and make 4 characters out of
%% them (4 x 6 = 24 bits).
%%
encodeBase64(Bs) when list(Bs) ->
    encodeBase64(list_to_binary(Bs));
encodeBase64(<<B:3/binary, Bs/binary>>) ->
    <<C1:6, C2:6, C3:6, C4:6>> = B,
    [enc(C1), enc(C2), enc(C3), enc(C4)| encodeBase64(Bs)];
encodeBase64(<<B:2/binary>>) ->
    <<C1:6, C2:6, C3:6, _:6>> = <<B/binary, 0>>,
    [enc(C1), enc(C2), enc(C3), $=];
encodeBase64(<<B:1/binary>>) ->
    <<C1:6, C2:6, _:12>> = <<B/binary, 0, 0>>,
    [enc(C1), enc(C2), $=, $=];
encodeBase64(<<>>) ->
    [].

%%
%% decodeBase64(Chars) -> Binary
%%
decodeBase64(Cs) ->
    list_to_binary(decode1(Cs)).

decode1([C1, C2, $=, $=]) ->
    <<B1, _:16>> = <<(dec(C1)):6, (dec(C2)):6, 0:12>>,
    [B1];
decode1([C1, C2, C3, $=]) ->
    <<B1, B2, _:8>> = <<(dec(C1)):6, (dec(C2)):6, (dec(C3)):6, (dec(0)):6>>,
    [B1, B2];
decode1([C1, C2, C3, C4| Cs]) ->
    Bin = <<(dec(C1)):6, (dec(C2)):6, (dec(C3)):6, (dec(C4)):6>>,
    [Bin| decode1(Cs)];
decode1([]) ->
    [].

%% enc/1 and dec/1
%%
%% Mapping: 0-25 -> A-Z, 26-51 -> a-z, 52-61 -> 0-9, 62 -> +, 63 -> /
%%
enc(C) ->
    65 + C + 6*?st(C,26) - 75*?st(C,52) -15*?st(C,62) + 3*?st(C,63).

dec(C) ->
    62*?st(C,43) + ?st(C,47) + (C-59)*?st(C,48) - 69*?st(C,65) - 6*?st(C,97).


