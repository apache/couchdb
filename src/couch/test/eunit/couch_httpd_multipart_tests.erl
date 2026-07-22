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

-module(couch_httpd_multipart_tests).

-include_lib("couch/include/couch_eunit.hrl").

-define(CONTENT_TYPE, ~S'multipart/related;boundary="abc123"').
-define(DOC_JSON,
    ~B'{"_attachments":{"ohai":{"follows":true,"content_type":"text/plain","length":4}}}'
).
-define(ATT_LEN, 4).

couch_httpd_multipart_test_() ->
    {
        foreach,
        fun setup/0,
        fun teardown/1,
        [
            ?TDEF_FE(t_single_writer_short_part_fails_fast),
            ?TDEF_FE(t_single_writer_empty_part_fails_fast),
            ?TDEF_FE(t_single_writer_exact_part_waits_for_more_writers),
            ?TDEF_FE(t_all_writers_short_part_fail)
        ]
    }.

setup() ->
    test_util:start_applications([config]).

teardown(Apps) ->
    test_util:stop_applications(Apps).

% Doc with on attachment declaring length 4 but only having length 3. The
% writer which detects doesn't have enough bytes exits.
t_single_writer_short_part_fails_fast(_) ->
    {Parser, ParserRef, Ref} = parse(3, body(<<"oha">>)),
    Writer = spawn_writer(Parser, Ref, ?ATT_LEN),
    ?assertEqual({parser_died, normal}, wait_writer(Writer, 3000)),
    ?assertEqual(normal, wait_down(ParserRef, 3000)),
    stop_writers([Writer]).

% Empty body part. Writer won't get any chunk before it starves.
t_single_writer_empty_part_fails_fast(_) ->
    {Parser, ParserRef, Ref} = parse(3, body(<<>>)),
    Writer = spawn_writer(Parser, Ref, ?ATT_LEN),
    ?assertEqual({parser_died, normal}, wait_writer(Writer, 3000)),
    ?assertEqual(normal, wait_down(ParserRef, 3000)),
    stop_writers([Writer]).

% First writer consuming all bytes should not exit and wait for other to also
% connect and fetch their copies.
t_single_writer_exact_part_waits_for_more_writers(_) ->
    {Parser, ParserRef, Ref} = parse(3, body(<<"ohai">>)),
    Writer1 = spawn_writer(Parser, Ref, ?ATT_LEN),
    ?assertEqual(got_all_bytes, wait_writer(Writer1, 3000)),
    % Check it didn't die
    ?assertEqual(timeout, wait_down(ParserRef, 300)),
    % Late-arriving writers
    Writer2 = spawn_writer(Parser, Ref, ?ATT_LEN),
    ?assertEqual(got_all_bytes, wait_writer(Writer2, 3000)),
    % Still shouldn't die. One more left
    ?assertEqual(timeout, wait_down(ParserRef, 300)),
    Writer3 = spawn_writer(Parser, Ref, ?ATT_LEN),
    ?assertEqual(got_all_bytes, wait_writer(Writer3, 3000)),
    % Everyone got their bytes, now it can die
    ?assertEqual(normal, wait_down(ParserRef, 3000)),
    stop_writers([Writer1, Writer2, Writer3]).

% Multuple writers get the "too short" condition. That's fine, everyone dies
% without blocking.
t_all_writers_short_part_fail(_) ->
    {Parser, ParserRef, Ref} = parse(3, body(<<"oha">>)),
    Writers = [spawn_writer(Parser, Ref, ?ATT_LEN) || _ <- lists:seq(1, 3)],
    [?assertEqual({parser_died, normal}, wait_writer(W, 3000)) || W <- Writers],
    ?assertEqual(normal, wait_down(ParserRef, 3000)),
    stop_writers(Writers).

body(AttBytes) ->
    <<
        "--abc123\r\n"
        "Content-Type: application/json\r\n"
        "\r\n",
        ?DOC_JSON/binary,
        "\r\n"
        "--abc123\r\n"
        "\r\n",
        AttBytes/binary,
        "\r\n"
        "--abc123--"
    >>.

% Spawn the parser like chttpd_db for MP PUT
parse(NumWriters, Body) ->
    Ref = make_ref(),
    couch_httpd_multipart:num_mp_writers(NumWriters),
    DataFun = fun() -> {Body, fun() -> throw(<<"expected more data">>) end} end,
    {{doc_bytes, Ref, DocBytes}, Parser, ParserRef} =
        couch_httpd_multipart:decode_multipart_stream(?CONTENT_TYPE, DataFun, Ref),
    ?assertEqual(?DOC_JSON, iolist_to_binary(DocBytes)),
    {Parser, ParserRef, Ref}.

% This is like the reader loop in fabric_rpc:make_att_reader()
spawn_writer(Parser, Ref, Need) ->
    ReportTo = self(),
    spawn_link(fun() ->
        ParserRef = monitor(process, Parser),
        Parser ! {hello_from_writer, Ref, self()},
        ReportTo ! {self(), writer_read(Parser, ParserRef, Ref, Need)},
        receive
            stop -> ok
        end
    end).

writer_read(_Parser, _ParserRef, _Ref, Need) when Need =< 0 ->
    got_all_bytes;
writer_read(Parser, ParserRef, Ref, Need) ->
    Parser ! {get_bytes, Ref, self()},
    receive
        {bytes, Ref, Bytes} ->
            writer_read(Parser, ParserRef, Ref, Need - iolist_size(Bytes));
        {'DOWN', ParserRef, _, _, Reason} ->
            {parser_died, Reason}
    end.

wait_writer(Writer, Timeout) ->
    receive
        {Writer, Result} -> Result
    after Timeout -> timeout
    end.

wait_down(MonitorRef, Timeout) ->
    receive
        {'DOWN', MonitorRef, _, _, Reason} -> Reason
    after Timeout -> timeout
    end.

stop_writers(Writers) ->
    [Writer ! stop || Writer <- Writers],
    ok.
