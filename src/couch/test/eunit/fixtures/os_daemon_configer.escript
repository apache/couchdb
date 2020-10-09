#! /usr/bin/env escript

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

-include("../../include/couch_eunit.hrl").

read() ->
    case io:get_line('') of
        eof ->
            stop;
        Data ->
            jiffy:decode(Data)
    end.

write(Mesg) ->
    Data = iolist_to_binary(jiffy:encode(Mesg)),
    io:format(binary_to_list(Data) ++ "\n", []).

get_cfg(Section) ->
    write([<<"get">>, Section]),
    read().

get_cfg(Section, Name) ->
    write([<<"get">>, Section, Name]),
    read().

log(Mesg) ->
    write([<<"log">>, Mesg]).

log(Mesg, Level) ->
    write([<<"log">>, Mesg, {[{<<"level">>, Level}]}]).

test_get_cfg1() ->
    Path = list_to_binary(?FILE),
    FileName = list_to_binary(filename:basename(?FILE)),
    {[{FileName, Path}]} = get_cfg(<<"os_daemons">>).

test_get_cfg2() ->
    Path = list_to_binary(?FILE),
    FileName = list_to_binary(filename:basename(?FILE)),
    Path = get_cfg(<<"os_daemons">>, FileName),
    <<"sequential">> = get_cfg(<<"uuids">>, <<"algorithm">>).


test_get_unknown_cfg() ->
    {[]} = get_cfg(<<"aal;3p4">>),
    null = get_cfg(<<"aal;3p4">>, <<"313234kjhsdfl">>).

test_log() ->
    log(<<"foobar!">>),
    log(<<"some stuff!">>, <<"debug">>),
    log(2),
    log(true),
    write([<<"log">>, <<"stuff">>, 2]),
    write([<<"log">>, 3, null]),
    write([<<"log">>, [1, 2], {[{<<"level">>, <<"debug">>}]}]),
    write([<<"log">>, <<"true">>, {[]}]).

do_tests() ->
    test_get_cfg1(),
    test_get_cfg2(),
    test_get_unknown_cfg(),
    test_log(),
    loop(io:read("")).

loop({ok, _}) ->
    loop(io:read(""));
loop(eof) ->
    init:stop();
loop({error, _Reason}) ->
    init:stop().

main([]) ->
    init_code_path(),
    do_tests().

init_code_path() ->
    Paths = [
        "couchdb",
        "jiffy",
        "ibrowse",
        "mochiweb",
        "snappy"
    ],
    lists:foreach(fun(Name) ->
        code:add_patha(filename:join([?BUILDDIR(), "src", Name, "ebin"]))
    end, Paths).
