#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./src/couchdb -sasl errlog_type error -boot start_sasl -noshell

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

filename() -> test_util:build_file("test/etap/temp.011").
sizeblock() -> 4096. % Need to keep this in sync with couch_file.erl

main(_) ->
    test_util:init_code_path(),
    {S1, S2, S3} = now(),
    random:seed(S1, S2, S3),

    etap:plan(18),
    case (catch test()) of
        ok ->
            etap:end_tests();
        Other ->
            etap:diag(io_lib:format("Test died abnormally: ~p", [Other])),
            etap:bail()
    end,
    ok.

test() ->
    {ok, Fd} = couch_file:open(filename(), [create,overwrite]),

    etap:is({ok, 0}, couch_file:bytes(Fd),
        "File should be initialized to contain zero bytes."),

    etap:is(ok, couch_file:write_header(Fd, {<<"some_data">>, 32}),
        "Writing a header succeeds."),

    {ok, Size1} = couch_file:bytes(Fd),
    etap:is_greater(Size1, 0,
        "Writing a header allocates space in the file."),

    etap:is({ok, {<<"some_data">>, 32}}, couch_file:read_header(Fd),
        "Reading the header returns what we wrote."),

    etap:is(ok, couch_file:write_header(Fd, [foo, <<"more">>]),
        "Writing a second header succeeds."),

    {ok, Size2} = couch_file:bytes(Fd),
    etap:is_greater(Size2, Size1,
        "Writing a second header allocates more space."),

    etap:is({ok, [foo, <<"more">>]}, couch_file:read_header(Fd),
        "Reading the second header does not return the first header."),

    % Delete the second header.
    ok = couch_file:truncate(Fd, Size1),

    etap:is({ok, {<<"some_data">>, 32}}, couch_file:read_header(Fd),
        "Reading the header after a truncation returns a previous header."),

    couch_file:write_header(Fd, [foo, <<"more">>]),
    etap:is({ok, Size2}, couch_file:bytes(Fd),
        "Rewriting the same second header returns the same second size."),

    couch_file:write_header(Fd, erlang:make_tuple(5000, <<"CouchDB">>)),
    etap:is(
        couch_file:read_header(Fd),
        {ok, erlang:make_tuple(5000, <<"CouchDB">>)},
        "Headers larger than the block size can be saved (COUCHDB-1319)"
    ),

    ok = couch_file:close(Fd),

    % Now for the fun stuff. Try corrupting the second header and see
    % if we recover properly.

    % Destroy the 0x1 byte that marks a header
    check_header_recovery(fun(CouchFd, RawFd, Expect, HeaderPos) ->
        etap:isnt(Expect, couch_file:read_header(CouchFd),
            "Should return a different header before corruption."),
        file:pwrite(RawFd, HeaderPos, <<0>>),
        etap:is(Expect, couch_file:read_header(CouchFd),
            "Corrupting the byte marker should read the previous header.")
    end),

    % Corrupt the size.
    check_header_recovery(fun(CouchFd, RawFd, Expect, HeaderPos) ->
        etap:isnt(Expect, couch_file:read_header(CouchFd),
            "Should return a different header before corruption."),
        % +1 for 0x1 byte marker
        file:pwrite(RawFd, HeaderPos+1, <<10/integer>>),
        etap:is(Expect, couch_file:read_header(CouchFd),
            "Corrupting the size should read the previous header.")
    end),

    % Corrupt the MD5 signature
    check_header_recovery(fun(CouchFd, RawFd, Expect, HeaderPos) ->
        etap:isnt(Expect, couch_file:read_header(CouchFd),
            "Should return a different header before corruption."),
        % +5 = +1 for 0x1 byte and +4 for term size.
        file:pwrite(RawFd, HeaderPos+5, <<"F01034F88D320B22">>),
        etap:is(Expect, couch_file:read_header(CouchFd),
            "Corrupting the MD5 signature should read the previous header.")
    end),

    % Corrupt the data
    check_header_recovery(fun(CouchFd, RawFd, Expect, HeaderPos) ->
        etap:isnt(Expect, couch_file:read_header(CouchFd),
            "Should return a different header before corruption."),
        % +21 = +1 for 0x1 byte, +4 for term size and +16 for MD5 sig
        file:pwrite(RawFd, HeaderPos+21, <<"some data goes here!">>),
        etap:is(Expect, couch_file:read_header(CouchFd),
            "Corrupting the header data should read the previous header.")
    end),

    ok.

check_header_recovery(CheckFun) ->
    {ok, Fd} = couch_file:open(filename(), [create,overwrite]),
    {ok, RawFd} = file:open(filename(), [read, write, raw, binary]),

    {ok, _} = write_random_data(Fd),
    ExpectHeader = {some_atom, <<"a binary">>, 756},
    ok = couch_file:write_header(Fd, ExpectHeader),

    {ok, HeaderPos} = write_random_data(Fd),
    ok = couch_file:write_header(Fd, {2342, <<"corruption! greed!">>}),

    CheckFun(Fd, RawFd, {ok, ExpectHeader}, HeaderPos),

    ok = file:close(RawFd),
    ok = couch_file:close(Fd),
    ok.

write_random_data(Fd) ->
    write_random_data(Fd, 100 + random:uniform(1000)).

write_random_data(Fd, 0) ->
    {ok, Bytes} = couch_file:bytes(Fd),
    {ok, (1 + Bytes div sizeblock()) * sizeblock()};
write_random_data(Fd, N) ->
    Choices = [foo, bar, <<"bizzingle">>, "bank", ["rough", stuff]],
    Term = lists:nth(random:uniform(4) + 1, Choices),
    {ok, _, _} = couch_file:append_term(Fd, Term),
    write_random_data(Fd, N-1).

