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

filename() -> test_util:build_file("test/etap/temp.010").

main(_) ->
    test_util:init_code_path(),
    etap:plan(16),
    case (catch test()) of
        ok ->
            etap:end_tests();
        Other ->
            etap:diag(io_lib:format("Test died abnormally: ~p", [Other])),
            etap:bail()
    end,
    ok.

test() ->
    etap:is({error, enoent}, couch_file:open("not a real file"),
        "Opening a non-existant file should return an enoent error."),

    etap:fun_is(
        fun({ok, _}) -> true; (_) -> false end,
        couch_file:open(filename() ++ ".1", [create, invalid_option]),
        "Invalid flags to open are ignored."
    ),

    {ok, Fd} = couch_file:open(filename() ++ ".0", [create, overwrite]),
    etap:ok(is_pid(Fd),
        "Returned file descriptor is a Pid"),

    etap:is({ok, 0}, couch_file:bytes(Fd),
        "Newly created files have 0 bytes."),

    etap:is({ok, 0}, couch_file:append_term(Fd, foo),
        "Appending a term returns the previous end of file position."),

    {ok, Size} = couch_file:bytes(Fd),
    etap:is_greater(Size, 0,
        "Writing a term increased the file size."),

    etap:is({ok, Size}, couch_file:append_binary(Fd, <<"fancy!">>),
        "Appending a binary returns the current file size."),

    etap:is({ok, foo}, couch_file:pread_term(Fd, 0),
        "Reading the first term returns what we wrote: foo"),

    etap:is({ok, <<"fancy!">>}, couch_file:pread_binary(Fd, Size),
        "Reading back the binary returns what we wrote: <<\"fancy\">>."),

    etap:is({ok, <<131, 100, 0, 3, 102, 111, 111>>},
        couch_file:pread_binary(Fd, 0),
        "Reading a binary at a term position returns the term as binary."
    ),

    {ok, BinPos} = couch_file:append_binary(Fd, <<131,100,0,3,102,111,111>>),
    etap:is({ok, foo}, couch_file:pread_term(Fd, BinPos),
        "Reading a term from a written binary term representation succeeds."),

    % append_binary == append_iolist?
    % Possible bug in pread_iolist or iolist() -> append_binary
    {ok, IOLPos} = couch_file:append_binary(Fd, ["foo", $m, <<"bam">>]),
    etap:is({ok, [<<"foombam">>]}, couch_file:pread_iolist(Fd, IOLPos),
        "Reading an results in a binary form of the written iolist()"),

    % XXX: How does on test fsync?
    etap:is(ok, couch_file:sync(Fd),
        "Syncing does not cause an error."),

    etap:is(ok, couch_file:truncate(Fd, Size),
        "Truncating a file succeeds."),

    %etap:is(eof, (catch couch_file:pread_binary(Fd, Size)),
    %    "Reading data that was truncated fails.")
    etap:skip(fun() -> ok end,
        "No idea how to test reading beyond EOF"),

    etap:is({ok, foo}, couch_file:pread_term(Fd, 0),
        "Truncating does not affect data located before the truncation mark."),

    etap:is(ok, couch_file:close(Fd),
        "Files close properly."),
    ok.
