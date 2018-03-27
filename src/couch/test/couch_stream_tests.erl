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

-module(couch_stream_tests).

-include_lib("couch/include/couch_eunit.hrl").

-define(ENGINE(FdVar), {couch_bt_engine_stream, {FdVar, []}}).

setup() ->
    {ok, Fd} = couch_file:open(?tempfile(), [create, overwrite]),
    {ok, Stream} = couch_stream:open(?ENGINE(Fd), []),
    {Fd, Stream}.

teardown({Fd, _}) ->
    ok = couch_file:close(Fd).


stream_test_() ->
    {
        "CouchDB stream tests",
        {
            setup,
            fun() -> test_util:start(?MODULE, [ioq]) end, fun test_util:stop/1,
            {
                foreach,
                fun setup/0, fun teardown/1,
                [
                    fun should_write/1,
                    fun should_write_consecutive/1,
                    fun should_write_empty_binary/1,
                    fun should_return_file_pointers_on_close/1,
                    fun should_return_stream_size_on_close/1,
                    fun should_return_valid_pointers/1,
                    fun should_recall_last_pointer_position/1,
                    fun should_stream_more_with_4K_chunk_size/1,
                    fun should_stop_on_normal_exit_of_stream_opener/1
                ]
            }
        }
    }.


should_write({_, Stream}) ->
    ?_assertEqual(ok, couch_stream:write(Stream, <<"food">>)).

should_write_consecutive({_, Stream}) ->
    couch_stream:write(Stream, <<"food">>),
    ?_assertEqual(ok, couch_stream:write(Stream, <<"foob">>)).

should_write_empty_binary({_, Stream}) ->
    ?_assertEqual(ok, couch_stream:write(Stream, <<>>)).

should_return_file_pointers_on_close({_, Stream}) ->
    couch_stream:write(Stream, <<"foodfoob">>),
    {NewEngine, _, _, _, _} = couch_stream:close(Stream),
    {ok, Ptrs} = couch_stream:to_disk_term(NewEngine),
    ?_assertEqual([{0, 8}], Ptrs).

should_return_stream_size_on_close({_, Stream}) ->
    couch_stream:write(Stream, <<"foodfoob">>),
    {_, Length, _, _, _} = couch_stream:close(Stream),
    ?_assertEqual(8, Length).

should_return_valid_pointers({_Fd, Stream}) ->
    couch_stream:write(Stream, <<"foodfoob">>),
    {NewEngine, _, _, _, _} = couch_stream:close(Stream),
    ?_assertEqual(<<"foodfoob">>, read_all(NewEngine)).

should_recall_last_pointer_position({Fd, Stream}) ->
    couch_stream:write(Stream, <<"foodfoob">>),
    {_, _, _, _, _} = couch_stream:close(Stream),
    {ok, ExpPtr} = couch_file:bytes(Fd),
    {ok, Stream2} = couch_stream:open(?ENGINE(Fd)),
    ZeroBits = <<0:(8 * 10)>>,
    OneBits = <<1:(8 * 10)>>,
    ok = couch_stream:write(Stream2, OneBits),
    ok = couch_stream:write(Stream2, ZeroBits),
    {NewEngine, 20, _, _, _} = couch_stream:close(Stream2),
    {ok, Ptrs} = couch_stream:to_disk_term(NewEngine),
    [{ExpPtr, 20}] = Ptrs,
    AllBits = iolist_to_binary([OneBits, ZeroBits]),
    ?_assertEqual(AllBits, read_all(NewEngine)).

should_stream_more_with_4K_chunk_size({Fd, _}) ->
    {ok, Stream} = couch_stream:open(?ENGINE(Fd), [{buffer_size, 4096}]),
    lists:foldl(
        fun(_, Acc) ->
            Data = <<"a1b2c">>,
            couch_stream:write(Stream, Data),
            [Data | Acc]
        end, [], lists:seq(1, 1024)),
    {NewEngine, Length, _, _, _} = couch_stream:close(Stream),
    {ok, Ptrs} = couch_stream:to_disk_term(NewEngine),
    ?_assertMatch({[{0, 4100}, {4106, 1020}], 5120}, {Ptrs, Length}).

should_stop_on_normal_exit_of_stream_opener({Fd, _}) ->
    RunnerPid = self(),
    OpenerPid = spawn(
        fun() ->
            {ok, StreamPid} = couch_stream:open(?ENGINE(Fd)),
            RunnerPid ! {pid, StreamPid}
        end),
    StreamPid = receive
        {pid, StreamPid0} -> StreamPid0
    end,
    % Confirm the validity of the test by verifying the stream opener has died
    ?assertNot(is_process_alive(OpenerPid)),
    % Verify the stream itself has also died
    ?_assertNot(is_process_alive(StreamPid)).


read_all(Engine) ->
    Data = couch_stream:foldl(Engine, fun(Bin, Acc) -> [Bin, Acc] end, []),
    iolist_to_binary(Data).
