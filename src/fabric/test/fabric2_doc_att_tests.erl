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

-module(fabric2_doc_att_tests).


-include_lib("couch/include/couch_db.hrl").
-include_lib("couch/include/couch_eunit.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("fabric2.hrl").
-include("fabric2_test.hrl").


doc_crud_test_() ->
    {
        "Test document CRUD operations",
        {
            setup,
            fun setup/0,
            fun cleanup/1,
            with([
                ?TDEF(create_att),
                ?TDEF(delete_att),
                ?TDEF(multiple_atts),
                ?TDEF(delete_one_att),
                ?TDEF(large_att),
                ?TDEF(att_on_conflict_isolation)
            ])
        }
    }.


setup() ->
    Ctx = test_util:start_couch([fabric]),
    {ok, Db} = fabric2_db:create(?tempdb(), [{user_ctx, ?ADMIN_USER}]),
    {Db, Ctx}.


cleanup({Db, Ctx}) ->
    ok = fabric2_db:delete(fabric2_db:name(Db), []),
    test_util:stop_couch(Ctx).


create_att({Db, _}) ->
    DocId = fabric2_util:uuid(),
    Att1 = couch_att:new([
        {name, <<"foo.txt">>},
        {type, <<"application/octet-stream">>},
        {att_len, 6},
        {data, <<"foobar">>},
        {encoding, identity},
        {md5, <<>>}
    ]),
    Doc1 = #doc{
        id = DocId,
        atts = [Att1]
    },
    {ok, _} = fabric2_db:update_doc(Db, Doc1),
    {ok, Doc2} = fabric2_db:open_doc(Db, DocId),
    #doc{
        atts = [Att2]
    } = Doc2,
    {loc, _Db, DocId, AttId} = couch_att:fetch(data, Att2),
    AttData = fabric2_db:read_attachment(Db, DocId, AttId),
    ?assertEqual(<<"foobar">>, AttData),

    % Check that the raw keys exist
    #{
        db_prefix := DbPrefix
    } = Db,
    IdKey = erlfdb_tuple:pack({?DB_ATT_NAMES, DocId, AttId}, DbPrefix),
    AttKey = erlfdb_tuple:pack({?DB_ATTS, DocId, AttId}, DbPrefix),

    fabric2_fdb:transactional(fun(Tx) ->
        IdVal = erlfdb:wait(erlfdb:get(Tx, IdKey)),
        AttVals = erlfdb:wait(erlfdb:get_range_startswith(Tx, AttKey)),

        ?assertEqual(<<>>, IdVal),
        ?assertMatch([{_, <<"foobar">>}], AttVals)
    end).


delete_att({Db, _}) ->
    DocId = fabric2_util:uuid(),
    Att1 = couch_att:new([
        {name, <<"foo.txt">>},
        {type, <<"application/octet-stream">>},
        {att_len, 6},
        {data, <<"foobar">>},
        {encoding, identity},
        {md5, <<>>}
    ]),
    Doc1 = #doc{
        id = DocId,
        atts = [Att1]
    },
    {ok, _} = fabric2_db:update_doc(Db, Doc1),
    {ok, Doc2} = fabric2_db:open_doc(Db, DocId),
    #doc{
        atts = [Att2]
    } = Doc2,
    {loc, _Db, DocId, AttId} = couch_att:fetch(data, Att2),

    Doc3 = Doc2#doc{atts = []},
    {ok, _} = fabric2_db:update_doc(Db, Doc3),

    {ok, Doc4} = fabric2_db:open_doc(Db, DocId),
    ?assertEqual([], Doc4#doc.atts),

    % Check that the raw keys were removed
    #{
        db_prefix := DbPrefix
    } = Db,
    IdKey = erlfdb_tuple:pack({?DB_ATT_NAMES, DocId, AttId}, DbPrefix),
    AttKey = erlfdb_tuple:pack({?DB_ATTS, DocId, AttId}, DbPrefix),

    fabric2_fdb:transactional(fun(Tx) ->
        IdVal = erlfdb:wait(erlfdb:get(Tx, IdKey)),
        AttVals = erlfdb:wait(erlfdb:get_range_startswith(Tx, AttKey)),

        ?assertEqual(not_found, IdVal),
        ?assertMatch([], AttVals)
    end).


multiple_atts({Db, _}) ->
    DocId = fabric2_util:uuid(),
    Atts = [
        mk_att(<<"foo.txt">>, <<"foobar">>),
        mk_att(<<"bar.txt">>, <<"barfoo">>),
        mk_att(<<"baz.png">>, <<"blargh">>)
    ],
    {ok, _} = create_doc(Db, DocId, Atts),
    ?assertEqual(
            #{
                <<"foo.txt">> => <<"foobar">>,
                <<"bar.txt">> => <<"barfoo">>,
                <<"baz.png">> => <<"blargh">>
            },
            read_atts(Db, DocId)
        ).


delete_one_att({Db, _}) ->
    DocId = fabric2_util:uuid(),
    Atts1 = [
        mk_att(<<"foo.txt">>, <<"foobar">>),
        mk_att(<<"bar.txt">>, <<"barfoo">>),
        mk_att(<<"baz.png">>, <<"blargh">>)
    ],
    {ok, RevId} = create_doc(Db, DocId, Atts1),
    Atts2 = tl(Atts1),
    {ok, _} = update_doc(Db, DocId, RevId, stubify(RevId, Atts2)),
    ?assertEqual(
            #{
                <<"bar.txt">> => <<"barfoo">>,
                <<"baz.png">> => <<"blargh">>
            },
            read_atts(Db, DocId)
        ).


large_att({Db, _}) ->
    DocId = fabric2_util:uuid(),
    % Total size ~360,000 bytes
    AttData = iolist_to_binary([
        <<"foobar">> || _ <- lists:seq(1, 60000)
    ]),
    Att1 = mk_att(<<"long.txt">>, AttData),
    {ok, _} = create_doc(Db, DocId, [Att1]),
    ?assertEqual(#{<<"long.txt">> => AttData}, read_atts(Db, DocId)),

    {ok, Doc} = fabric2_db:open_doc(Db, DocId),
    #doc{atts = [Att2]} = Doc,
    {loc, _Db, DocId, AttId} = couch_att:fetch(data, Att2),

    #{db_prefix := DbPrefix} = Db,
    AttKey = erlfdb_tuple:pack({?DB_ATTS, DocId, AttId}, DbPrefix),
    fabric2_fdb:transactional(fun(Tx) ->
        AttVals = erlfdb:wait(erlfdb:get_range_startswith(Tx, AttKey)),
        ?assertEqual(4, length(AttVals))
    end).


att_on_conflict_isolation({Db, _}) ->
    DocId = fabric2_util:uuid(),
    [PosRevA1, PosRevB1] = create_conflicts(Db, DocId, []),
    Att = mk_att(<<"happy_goat.tiff">>, <<":D>">>),
    {ok, PosRevA2} = update_doc(Db, DocId, PosRevA1, [Att]),
    ?assertEqual(
            #{<<"happy_goat.tiff">> => <<":D>">>},
            read_atts(Db, DocId, PosRevA2)
        ),
    ?assertEqual(#{}, read_atts(Db, DocId, PosRevB1)).


mk_att(Name, Data) ->
    couch_att:new([
        {name, Name},
        {type, <<"application/octet-stream">>},
        {att_len, size(Data)},
        {data, Data},
        {encoding, identity},
        {md5, <<>>}
    ]).


stubify(RevId, Atts) when is_list(Atts) ->
    lists:map(fun(Att) ->
        stubify(RevId, Att)
    end, Atts);

stubify({Pos, _Rev}, Att) ->
    couch_att:store([
        {data, stub},
        {revpos, Pos}
    ], Att).


create_doc(Db, DocId, Atts) ->
    Doc = #doc{
        id = DocId,
        atts = Atts
    },
    fabric2_db:update_doc(Db, Doc).


update_doc(Db, DocId, {Pos, Rev}, Atts) ->
    Doc = #doc{
        id = DocId,
        revs = {Pos, [Rev]},
        atts = Atts
    },
    fabric2_db:update_doc(Db, Doc).


create_conflicts(Db, DocId, Atts) ->
    Base = #doc{
        id = DocId,
        atts = Atts
    },
    {ok, {_, Rev1} = PosRev} = fabric2_db:update_doc(Db, Base),
    <<Rev2:16/binary, Rev3:16/binary>> = fabric2_util:uuid(),
    Doc1 = #doc{
        id = DocId,
        revs = {2, [Rev2, Rev1]},
        atts = stubify(PosRev, Atts)
    },
    Doc2 = #doc{
        id = DocId,
        revs = {2, [Rev3, Rev1]},
        atts = stubify(PosRev, Atts)
    },
    {ok, _} = fabric2_db:update_doc(Db, Doc1, [replicated_changes]),
    {ok, _} = fabric2_db:update_doc(Db, Doc2, [replicated_changes]),
    lists:reverse(lists:sort([{2, Rev2}, {2, Rev3}])).


read_atts(Db, DocId) ->
    {ok, #doc{atts = Atts}} = fabric2_db:open_doc(Db, DocId),
    atts_to_map(Db, DocId, Atts).


read_atts(Db, DocId, PosRev) ->
    {ok, Docs} = fabric2_db:open_doc_revs(Db, DocId, [PosRev], []),
    [{ok, #doc{atts = Atts}}] = Docs,
    atts_to_map(Db, DocId, Atts).


atts_to_map(Db, DocId, Atts) ->
    lists:foldl(fun(Att, Acc) ->
        [Name, Data] = couch_att:fetch([name, data], Att),
        {loc, _Db, DocId, AttId} = Data,
        AttBin = fabric2_db:read_attachment(Db, DocId, AttId),
        maps:put(Name, AttBin, Acc)
    end, #{}, Atts).
