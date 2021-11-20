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

-module(fabric_moved_shards_seq_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("mem3/include/mem3.hrl").

-define(TDEF(A), {atom_to_list(A), fun A/0}).

main_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        [
            ?TDEF(t_shard_moves_avoid_sequence_rewinds)
        ]
    }.

setup() ->
    test_util:start_couch([fabric]).

teardown(Ctx) ->
    meck:unload(),
    test_util:stop_couch(Ctx).

t_shard_moves_avoid_sequence_rewinds() ->
    DocCnt = 30,
    DbName = ?tempdb(),

    ok = fabric:create_db(DbName, [{q, 1}, {n, 1}]),
    lists:foreach(
        fun(I) ->
            update_doc(DbName, #doc{id = erlang:integer_to_binary(I)})
        end,
        lists:seq(1, DocCnt)
    ),

    {ok, _, Seq1, 0} = changes(DbName, #changes_args{limit = 1, since = "now"}),
    [{_, Range, {Seq, Uuid, _}}] = seq_decode(Seq1),

    % Transform Seq1 pretending it came from a fake source node, before the
    % shard was moved to the current node.
    SrcNode = 'srcnode@srchost',
    Seq2 = seq_encode([{SrcNode, Range, {Seq, Uuid, SrcNode}}]),

    % First, check when the shard file epoch is mismatched epoch and the
    % sequence would rewind. This ensures the epoch and uuid check protection
    % in couch_db works as intended.
    Result1 = changes(DbName, #changes_args{limit = 1, since = Seq2}),
    ?assertMatch({ok, _, _, _}, Result1),
    {ok, _, _, PendingRewind} = Result1,
    ?assertEqual(DocCnt - 1, PendingRewind),

    % Mock epoch checking to pretend that shard actually used to live on
    % SrcNode. In this case, we should not have rewinds.
    mock_epochs([{node(), DocCnt}, {SrcNode, 1}]),
    Result2 = changes(DbName, #changes_args{limit = 1, since = Seq2}),
    ?assertMatch({ok, _, _, _}, Result2),
    {ok, _, _, PendingNoRewind} = Result2,
    ?assertEqual(0, PendingNoRewind),

    ok = fabric:delete_db(DbName, []).

changes_callback(start, Acc) ->
    {ok, Acc};
changes_callback({change, {Change}}, Acc) ->
    CM = maps:from_list(Change),
    {ok, [CM | Acc]};
changes_callback({stop, EndSeq, Pending}, Acc) ->
    {ok, Acc, EndSeq, Pending}.

changes(DbName, #changes_args{} = Args) ->
    fabric_util:isolate(fun() ->
        fabric:changes(DbName, fun changes_callback/2, [], Args)
    end).

update_doc(DbName, #doc{} = Doc) ->
    fabric_util:isolate(fun() ->
        case fabric:update_doc(DbName, Doc, [?ADMIN_CTX]) of
            {ok, Res} -> Res
        end
    end).

seq_decode(Seq) ->
    % This is copied from fabric_view_changes
    Pattern = "^\"?([0-9]+-)?(?<opaque>.*?)\"?$",
    Options = [{capture, [opaque], binary}],
    {match, Seq1} = re:run(Seq, Pattern, Options),
    binary_to_term(couch_util:decodeBase64Url(Seq1)).

seq_encode(Unpacked) ->
    % Copied from fabric_view_changes
    Opaque = couch_util:encodeBase64Url(term_to_binary(Unpacked, [compressed])),
    ?l2b(["30", $-, Opaque]).

mock_epochs(Epochs) ->
    % Since we made up a node name we'll have to mock epoch checking
    meck:new(couch_db_engine, [passthrough]),
    meck:expect(couch_db_engine, get_epochs, fun(_) -> Epochs end).
