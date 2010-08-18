#!/usr/bin/env escript
%% -*- erlang -*-

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

main(_) ->
    test_util:init_code_path(),
    etap:plan(13),
    case (catch test()) of
        ok ->
            etap:end_tests();
        Other ->
            etap:diag(io_lib:format("Test died abnormally: ~p", [Other])),
            etap:bail(Other)
    end,
    ok.

read_all(Fd, PosList) ->
    Data = couch_stream:foldl(Fd, PosList, fun(Bin, Acc) -> [Bin, Acc] end, []),
    iolist_to_binary(Data).

test() ->
    {ok, Fd} = couch_file:open("test/etap/temp.050", [create,overwrite]),
    {ok, Stream} = couch_stream:open(Fd),

    etap:is(ok, couch_stream:write(Stream, <<"food">>),
        "Writing to streams works."),

    etap:is(ok, couch_stream:write(Stream, <<"foob">>),
        "Consecutive writing to streams works."),

    etap:is(ok, couch_stream:write(Stream, <<>>),
        "Writing an empty binary does nothing."),

    {Ptrs, Length, _, _, _} = couch_stream:close(Stream),
    etap:is(Ptrs, [{0, 8}], "Close returns the file pointers."),
    etap:is(Length, 8, "Close also returns the number of bytes written."),
    etap:is(<<"foodfoob">>, read_all(Fd, Ptrs), "Returned pointers are valid."),

    % Remeber where we expect the pointer to be.
    {ok, ExpPtr} = couch_file:bytes(Fd),
    {ok, Stream2} = couch_stream:open(Fd),
    OneBits = <<1:(8*10)>>,
    etap:is(ok, couch_stream:write(Stream2, OneBits),
        "Successfully wrote 80 1 bits."),

    ZeroBits = <<0:(8*10)>>,
    etap:is(ok, couch_stream:write(Stream2, ZeroBits),
        "Successfully wrote 80 0 bits."),

    {Ptrs2, Length2, _, _, _} = couch_stream:close(Stream2),
    etap:is(Ptrs2, [{ExpPtr, 20}], "Closing stream returns the file pointers."),
    etap:is(Length2, 20, "Length written is 160 bytes."),

    AllBits = iolist_to_binary([OneBits,ZeroBits]),
    etap:is(AllBits, read_all(Fd, Ptrs2), "Returned pointers are valid."),

    % Stream more the 4K chunk size.
    {ok, ExpPtr2} = couch_file:bytes(Fd),
    {ok, Stream3} = couch_stream:open(Fd),
    Acc2 = lists:foldl(fun(_, Acc) ->
        Data = <<"a1b2c">>,
        couch_stream:write(Stream3, Data),
        [Data | Acc]
    end, [], lists:seq(1, 1024)),
    {Ptrs3, Length3, _, _, _} = couch_stream:close(Stream3),

    % 4095 because of 5 * 4096 rem 5 (last write before exceeding threshold)
    % + 5 puts us over the threshold
    % + 4 bytes for the term_to_binary adding a length header
    % + 1 byte every 4K for tail append headers
    SecondPtr = ExpPtr2 + 4095 + 5 + 4 + 1,
    etap:is(Ptrs3, [{ExpPtr2, 4100}, {SecondPtr, 1020}], "Pointers every 4K bytes."),
    etap:is(Length3, 5120, "Wrote the expected 5K bytes."),

    couch_file:close(Fd),
    ok.
