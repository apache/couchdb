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

-module(fabric2_db_size_tests).

-export([
    random_body/0
]).

-include_lib("couch/include/couch_db.hrl").
-include_lib("couch/include/couch_eunit.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("fabric2_test.hrl").


db_size_test_() ->
    {
        "Test database size calculations",
        {
            setup,
            fun setup/0,
            fun cleanup/1,
            with([
                ?TDEF(new_doc),
                ?TDEF(replicate_new_doc),
                ?TDEF(edit_doc),
                ?TDEF(delete_doc),
                ?TDEF(create_conflict),
                ?TDEF(replicate_new_winner),
                ?TDEF(replicate_deep_deleted),
                ?TDEF(delete_winning_revision),
                ?TDEF(delete_conflict_revision),
                ?TDEF(replicate_existing_revision),
                ?TDEF(replicate_shared_history),
                ?TDEF(create_doc_with_attachment),
                ?TDEF(add_attachment_in_update),
                ?TDEF(add_second_attachment),
                ?TDEF(delete_attachment),
                ?TDEF(delete_one_attachment),
                ?TDEF(delete_all_attachments),
                ?TDEF(re_add_attachment),
                ?TDEF(update_and_remove_attachment),
                ?TDEF(replicate_new_doc_with_attachment),
                ?TDEF(replicate_remove_attachment),
                ?TDEF(replicate_stub_attachment),
                ?TDEF(replicate_stub_and_new_attachment),
                ?TDEF(replicate_new_att_to_winner),
                ?TDEF(replicate_change_att_to_winner),
                ?TDEF(replicate_rem_att_from_winner),
                ?TDEF(replicate_stub_to_winner),
                ?TDEF(replicate_new_att_to_conflict),
                ?TDEF(replicate_change_att_to_conflict),
                ?TDEF(replicate_rem_att_from_conflict),
                ?TDEF(replicate_stub_to_conflict),
                ?TDEF(create_local_doc),
                ?TDEF(update_local_doc),
                ?TDEF(delete_local_doc),
                ?TDEF(recreate_local_doc)
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


new_doc({Db, _}) ->
    check(Db, [
        {create, #{tgt => rev1}}
    ]).


replicate_new_doc({Db, _}) ->
    check(Db, [
        {replicate, #{tgt => rev1}}
    ]).


edit_doc({Db, _}) ->
    check(Db, [
        {create, #{tgt => rev1}},
        {update, #{src => rev1, tgt => rev2}}
    ]).


delete_doc({Db, _}) ->
    check(Db, [
        {create, #{tgt => rev1}},
        {delete, #{src => rev1, tgt => rev2}}
    ]).


create_conflict({Db, _}) ->
    check(Db, [
        {create, #{tgt => rev1}},
        {replicate, #{tgt => rev2}}
    ]).


replicate_new_winner({Db, _}) ->
    check(Db, [
        {create, #{tgt => rev1}},
        {replicate, #{tgt => rev2, depth => 3}}
    ]).


replicate_deep_deleted({Db, _}) ->
    check(Db, [
        {create, #{tgt => rev1, depth => 2}},
        {replicate, #{tgt => rev2, depth => 5, deleted => true}}
    ]).


delete_winning_revision({Db, _}) ->
    check(Db, [
        {create, #{tgt => rev1}},
        {replicate, #{tgt => rev2}},
        {delete, #{src => {winner, [rev1, rev2]}, tgt => rev3}}
    ]).


delete_conflict_revision({Db, _}) ->
    check(Db, [
        {create, #{tgt => rev1}},
        {replicate, #{tgt => rev2}},
        {delete, #{src => {conflict, [rev1, rev2]}, tgt => rev3}}
    ]).


replicate_existing_revision({Db, _}) ->
    check(Db, [
        {create, #{tgt => rev1}},
        {replicate, #{src => rev1, tgt => rev2, depth => 0}}
    ]).


replicate_shared_history({Db, _}) ->
    check(Db, [
        {create, #{tgt => rev1, depth => 5}},
        {update, #{src => rev1, tgt => rev2, depth => 5}},
        {replicate, #{
            src => rev1,
            src_exists => false,
            tgt => rev3,
            depth => 5
        }}
    ]).


create_doc_with_attachment({Db, _}) ->
    check(Db, [
        {mk_att, #{tgt => att1}},
        {create, #{tgt => rev1, atts => [att1]}}
    ]).


add_attachment_in_update({Db, _}) ->
    check(Db, [
        {mk_att, #{tgt => att1}},
        {create, #{tgt => rev1}},
        {update, #{src => rev1, tgt => rev2, atts => [att1]}}
    ]).


add_second_attachment({Db, _}) ->
    check(Db, [
        {mk_att, #{tgt => att1}},
        {mk_att, #{tgt => att2}},
        {create, #{tgt => rev1, atts => [att1]}},
        {update, #{src => rev1, tgt => rev2, atts => [att1, att2]}}
    ]).


delete_attachment({Db, _}) ->
    check(Db, [
        {mk_att, #{tgt => att1}},
        {create, #{tgt => rev1, atts => [att1]}},
        {update, #{src => rev1, tgt => rev2}}
    ]).


delete_one_attachment({Db, _}) ->
    check(Db, [
        {mk_att, #{tgt => att1}},
        {mk_att, #{tgt => att2}},
        {mk_att, #{tgt => att3, stub => att1, revpos => 1}},
        {create, #{tgt => rev1, atts => [att1, att2]}},
        {update, #{src => rev1, tgt => rev2, atts => [att3]}}
    ]).


delete_all_attachments({Db, _}) ->
    check(Db, [
        {mk_att, #{tgt => att1}},
        {mk_att, #{tgt => att2}},
        {create, #{tgt => rev1, atts => [att1, att2]}},
        {update, #{src => rev1, tgt => rev2, atts => []}}
    ]).


re_add_attachment({Db, _}) ->
    check(Db, [
        {mk_att, #{tgt => att1}},
        {create, #{tgt => rev1, atts => [att1]}},
        {update, #{src => rev1, tgt => rev2}},
        {update, #{src => rev2, tgt => rev3, atts => [att1]}}
    ]).


update_and_remove_attachment({Db, _}) ->
    check(Db, [
        {mk_att, #{tgt => att1}},
        {mk_att, #{tgt => att2}},
        {mk_att, #{tgt => att3, stub => att1, revpos => 1}},
        {mk_att, #{tgt => att4}},
        {create, #{tgt => rev1, atts => [att1, att2]}},
        {update, #{src => rev1, tgt => rev2, atts => [att3, att4]}}
    ]).


replicate_new_doc_with_attachment({Db, _}) ->
    check(Db, [
        {mk_att, #{tgt => att1}},
        {replicate, #{tgt => rev1, atts => [att1]}}
    ]).


replicate_remove_attachment({Db, _}) ->
    check(Db, [
        {mk_att, #{tgt => att1}},
        {create, #{tgt => rev1, atts => [att1]}},
        {replicate, #{src => rev1, tgt => rev2}}
    ]).


replicate_stub_attachment({Db, _}) ->
    check(Db, [
        {mk_att, #{tgt => att1}},
        {mk_att, #{tgt => att2, stub => att1, revpos => 1}},
        {create, #{tgt => rev1, atts => [att1]}},
        {replicate, #{src => rev1, tgt => rev2, atts => [att2]}}
    ]).


replicate_stub_and_new_attachment({Db, _}) ->
    check(Db, [
        {mk_att, #{tgt => att1}},
        {mk_att, #{tgt => att2, stub => att1, revpos => 1}},
        {mk_att, #{tgt => att3}},
        {create, #{tgt => rev1, atts => [att1]}},
        {replicate, #{src => rev1, tgt => rev2, atts => [att2, att3]}}
    ]).


replicate_new_att_to_winner({Db, _}) ->
    check(Db, [
        {mk_att, #{tgt => att1}},
        {create, #{tgt => rev1}},
        {replicate, #{tgt => rev2}},
        {replicate, #{
            src => {winner, [rev1, rev2]},
            tgt => rev3,
            atts => [att1]}
        }
    ]).


replicate_change_att_to_winner({Db, _}) ->
    check(Db, [
        {mk_att, #{tgt => att1}},
        {mk_att, #{tgt => att2}},
        {create, #{tgt => rev1, atts => [att1]}},
        {replicate, #{tgt => rev2, atts => [att1]}},
        {replicate, #{
            src => {winner, [rev1, rev2]},
            tgt => rev3,
            atts => [att2]}
        }
    ]).


replicate_rem_att_from_winner({Db, _}) ->
    check(Db, [
        {mk_att, #{tgt => att1}},
        {create, #{tgt => rev1, atts => [att1]}},
        {replicate, #{tgt => rev2, atts => [att1]}},
        {replicate, #{src => {winner, [rev1, rev2]}, tgt => rev3}}
    ]).


replicate_stub_to_winner({Db, _}) ->
    check(Db, [
        {mk_att, #{tgt => att1}},
        {mk_att, #{tgt => att2, stub => att1, revpos => 1}},
        {create, #{tgt => rev1, atts => [att1]}},
        {replicate, #{tgt => rev2, atts => [att1]}},
        {replicate, #{
            src => {winner, [rev1, rev2]},
            tgt => rev3,
            atts => [att2]}}
    ]).


replicate_new_att_to_conflict({Db, _}) ->
    check(Db, [
        {mk_att, #{tgt => att1}},
        {create, #{tgt => rev1}},
        {replicate, #{tgt => rev2}},
        {replicate, #{
            src => {conflict, [rev1, rev2]},
            tgt => rev3,
            atts => [att1]}
        }
    ]).


replicate_change_att_to_conflict({Db, _}) ->
    check(Db, [
        {mk_att, #{tgt => att1}},
        {mk_att, #{tgt => att2}},
        {create, #{tgt => rev1, atts => [att1]}},
        {replicate, #{tgt => rev2, atts => [att1]}},
        {replicate, #{
            src => {conflict, [rev1, rev2]},
            tgt => rev3,
            atts => [att2]}
        }
    ]).


replicate_rem_att_from_conflict({Db, _}) ->
    check(Db, [
        {mk_att, #{tgt => att1}},
        {create, #{tgt => rev1, atts => [att1]}},
        {replicate, #{tgt => rev2, atts => [att1]}},
        {replicate, #{src => {conflict, [rev1, rev2]}, tgt => rev3}}
    ]).


replicate_stub_to_conflict({Db, _}) ->
    check(Db, [
        {mk_att, #{tgt => att1}},
        {mk_att, #{tgt => att2, stub => att1, revpos => 1}},
        {create, #{tgt => rev1, atts => [att1]}},
        {replicate, #{tgt => rev2, atts => [att1]}},
        {replicate, #{
            src => {conflict, [rev1, rev2]},
            tgt => rev3,
            atts => [att2]}}
    ]).


create_local_doc({Db, _}) ->
    check(Db, #{local => true}, [
        {create, #{tgt => rev1}}
    ]).


update_local_doc({Db, _}) ->
    check(Db, #{local => true}, [
        {create, #{tgt => rev1}},
        {update, #{src => rev1, tgt => rev2}}
    ]).


delete_local_doc({Db, _}) ->
    check(Db, #{local => true}, [
        {create, #{tgt => rev1}},
        {update, #{src => rev1, tgt => rev2}},
        {delete, #{src => rev2, tgt => rev3}}
    ]).


recreate_local_doc({Db, _}) ->
    check(Db, #{local => true}, [
        {create, #{tgt => rev1}},
        {update, #{src => rev1, tgt => rev2}},
        {delete, #{src => rev2, tgt => rev3}},
        {create, #{tgt => rev4}}
    ]).


check(Db, Actions) ->
    check(Db, #{}, Actions).


check(Db, CheckOpts, Actions) ->
    DocId = case maps:get(local, CheckOpts, false) of
        true ->
            Base = couch_uuids:random(),
            <<"_local/", Base/binary>>;
        false ->
            couch_uuids:random()
    end,
    InitSt = #{
        doc_id => DocId,
        revs => #{},
        atts => #{},
        size => db_size(Db)
    },
    lists:foldl(fun({Action, Opts}, StAcc) ->
        case Action of
            create -> create_doc(Db, Opts, StAcc);
            update -> update_doc(Db, Opts, StAcc);
            delete -> delete_doc(Db, Opts, StAcc);
            replicate -> replicate_doc(Db, Opts, StAcc);
            mk_att -> make_attachment(Opts, StAcc);
            log_state -> log_state(Opts, StAcc)
        end
    end, InitSt, Actions).


create_doc(Db, Opts, St) ->
    #{
        doc_id := DocId,
        revs := Revs,
        atts := Atts,
        size := InitDbSize
    } = St,

    ?assert(maps:is_key(tgt, Opts)),

    Tgt = maps:get(tgt, Opts),
    AttKeys = maps:get(atts, Opts, []),
    Depth = maps:get(depth, Opts, 1),

    ?assert(not maps:is_key(Tgt, Revs)),
    lists:foreach(fun(AttKey) ->
        ?assert(maps:is_key(AttKey, Atts))
    end, AttKeys),
    ?assert(Depth >= 1),

    AttRecords = lists:map(fun(AttKey) ->
        maps:get(AttKey, Atts)
    end, AttKeys),

    InitDoc = #doc{id = DocId},
    FinalDoc = lists:foldl(fun(Iter, Doc0) ->
        #doc{
            revs = {_OldStart, OldRevs}
        } = Doc1 = randomize_doc(Doc0),
        Doc2 = if Iter < Depth -> Doc1; true ->
            Doc1#doc{atts = AttRecords}
        end,
        {ok, {Pos, Rev}} = fabric2_db:update_doc(Db, Doc2),
        Doc2#doc{revs = {Pos, [Rev | OldRevs]}}
    end, InitDoc, lists:seq(1, Depth)),

    FinalDocSize = doc_size(FinalDoc),
    FinalDbSize = db_size(Db),

    ?assertEqual(FinalDbSize - InitDbSize, FinalDocSize),

    store_rev(Db, St, FinalDbSize, Tgt, FinalDoc).


update_doc(Db, Opts, St) ->
    #{
        doc_id := DocId,
        revs := Revs,
        atts := Atts,
        size := InitDbSize
    } = St,

    IsLocal = case DocId of
        <<"_local/", _/binary>> -> true;
        _ -> false
    end,

    ?assert(maps:is_key(src, Opts)),
    ?assert(maps:is_key(tgt, Opts)),

    Src = pick_rev(Revs, maps:get(src, Opts)),
    Tgt = maps:get(tgt, Opts),
    AttKeys = maps:get(atts, Opts, []),
    Depth = maps:get(depth, Opts, 1),

    ?assert(maps:is_key(Src, Revs)),
    ?assert(not maps:is_key(Tgt, Revs)),
    lists:foreach(fun(AttKey) ->
        ?assert(maps:is_key(AttKey, Atts))
    end, AttKeys),
    ?assert(Depth >= 1),

    AttRecords = lists:map(fun(AttKey) ->
        maps:get(AttKey, Atts)
    end, AttKeys),

    InitDoc = maps:get(Src, Revs),
    FinalDoc = lists:foldl(fun(Iter, Doc0) ->
        #doc{
            revs = {_OldStart, OldRevs}
        } = Doc1 = randomize_doc(Doc0),
        Doc2 = if Iter < Depth -> Doc1; true ->
            Doc1#doc{atts = AttRecords}
        end,
        {ok, {Pos, Rev}} = fabric2_db:update_doc(Db, Doc2),
        case IsLocal of
            true -> Doc2#doc{revs = {Pos, [Rev]}};
            false -> Doc2#doc{revs = {Pos, [Rev | OldRevs]}}
        end
    end, InitDoc, lists:seq(1, Depth)),

    InitDocSize = doc_size(InitDoc),
    FinalDocSize = doc_size(FinalDoc),
    FinalDbSize = db_size(Db),

    ?assertEqual(FinalDbSize - InitDbSize, FinalDocSize - InitDocSize),

    store_rev(Db, St, FinalDbSize, Tgt, FinalDoc).


delete_doc(Db, Opts, St) ->
    #{
        doc_id := DocId,
        revs := Revs,
        size := InitDbSize
    } = St,

    IsLocal = case DocId of
        <<"_local/", _/binary>> -> true;
        _ -> false
    end,

    ?assert(maps:is_key(src, Opts)),
    ?assert(maps:is_key(tgt, Opts)),

    Src = pick_rev(Revs, maps:get(src, Opts)),
    Tgt = maps:get(tgt, Opts),

    ?assert(maps:is_key(Src, Revs)),
    ?assert(not maps:is_key(Tgt, Revs)),

    InitDoc = maps:get(Src, Revs),
    #doc{
        revs = {_OldStart, OldRevs}
    } = UpdateDoc = randomize_deleted_doc(InitDoc),

    {ok, {Pos, Rev}} = fabric2_db:update_doc(Db, UpdateDoc),

    FinalDoc = case IsLocal of
        true -> UpdateDoc#doc{revs = {Pos, [Rev]}};
        false -> UpdateDoc#doc{revs = {Pos, [Rev | OldRevs]}}
    end,

    InitDocSize = doc_size(InitDoc),
    FinalDocSize = doc_size(FinalDoc),
    FinalDbSize = db_size(Db),

    ?assertEqual(FinalDbSize - InitDbSize, FinalDocSize - InitDocSize),

    store_rev(Db, St, FinalDbSize, Tgt, FinalDoc).


replicate_doc(Db, Opts, St) ->
    #{
        doc_id := DocId,
        revs := Revs,
        atts := Atts,
        size := InitDbSize
    } = St,

    ?assert(maps:is_key(tgt, Opts)),

    Src = pick_rev(Revs, maps:get(src, Opts, undefined)),
    SrcExists = maps:get(src_exists, Opts, true),
    Tgt = maps:get(tgt, Opts),
    Deleted = maps:get(deleted, Opts, false),
    AttKeys = maps:get(atts, Opts, []),
    Depth = maps:get(depth, Opts, 1),

    if Src == undefined -> ok; true ->
        ?assert(maps:is_key(Src, Revs))
    end,
    ?assert(not maps:is_key(Tgt, Revs)),
    ?assert(is_boolean(Deleted)),
    lists:foreach(fun(AttKey) ->
        ?assert(maps:is_key(AttKey, Atts))
    end, AttKeys),
    ?assert(Depth >= 0),

    if Depth > 0 -> ok; true ->
        ?assert(length(AttKeys) == 0)
    end,

    InitDoc = maps:get(Src, Revs, #doc{id = DocId}),
    NewRevsDoc = lists:foldl(fun(_, Doc0) ->
        #doc{
            revs = {RevStart, RevIds}
        } = Doc0,
        NewRev = crypto:strong_rand_bytes(16),
        Doc0#doc{
            revs = {RevStart + 1, [NewRev | RevIds]}
        }
    end, InitDoc, lists:seq(1, Depth)),

    FinalDoc = if NewRevsDoc == InitDoc -> NewRevsDoc; true ->
        UpdateDoc = case Deleted of
            true -> randomize_deleted_doc(NewRevsDoc);
            false -> randomize_doc(NewRevsDoc)
        end,
        #doc{
            revs = {RevPos, _}
        } = UpdateDoc,
        AttRecords = lists:map(fun(AttKey) ->
            BaseAtt = maps:get(AttKey, Atts),
            case couch_att:fetch(data, BaseAtt) of
                stub -> BaseAtt;
                <<_/binary>> -> couch_att:store(revpos, RevPos, BaseAtt)
            end
        end, AttKeys),
        UpdateDoc#doc{atts = AttRecords}
    end,

    try
        {ok, _} = fabric2_db:update_doc(Db, FinalDoc, [replicated_changes])
    catch throw:{missing_stub, _} ->
        log_state(#{}, St),
        ?debugFmt("Replicated: ~p~n", [FinalDoc]),
        ?assert(false)
    end,

    InitDocSize = doc_size(InitDoc),
    FinalDocSize = doc_size(FinalDoc),
    FinalDbSize = db_size(Db),

    SizeChange = case {Src, SrcExists} of
        {undefined, _} -> FinalDocSize;
        {_, false} -> FinalDocSize;
        {_, _} -> FinalDocSize - InitDocSize
    end,
    ?assertEqual(FinalDbSize - InitDbSize, SizeChange),

    store_rev(Db, St, FinalDbSize, Tgt, FinalDoc).


make_attachment(Opts, St) ->
    #{
        atts := Atts
    } = St,

    ?assert(maps:is_key(tgt, Opts)),

    Tgt = maps:get(tgt, Opts),
    Stub = maps:get(stub, Opts, undefined),
    RevPos = maps:get(revpos, Opts, undefined),
    NameRaw = maps:get(name, Opts, undefined),

    ?assert(not maps:is_key(Tgt, Atts)),
    if Stub == undefined -> ok; true ->
        ?assert(maps:is_key(Stub, Atts))
    end,
    ?assert(RevPos == undefined orelse RevPos >= 0),

    Name = if
        NameRaw == undefined -> undefined;
        is_atom(NameRaw) -> atom_to_binary(NameRaw, utf8);
        is_binary(NameRaw) -> NameRaw;
        is_list(NameRaw) -> list_to_binary(NameRaw)
    end,

    Att0 = case Stub of
        undefined ->
            random_attachment(Name);
        _ ->
            SrcAtt = maps:get(Stub, Atts),
            couch_att:store(data, stub, SrcAtt)
    end,
    Att1 = if RevPos == undefined -> Att0; true ->
        couch_att:store(revpos, RevPos, Att0)
    end,

    St#{atts := maps:put(Tgt, Att1, Atts)}.


log_state(_Opts, St) ->
    #{
        doc_id := DocId,
        revs := Revs,
        atts := Atts,
        size := DbSize
    } = St,

    ?debugFmt("~nDocId: ~p~n", [DocId]),
    ?debugFmt("Db Size: ~p~n~n", [DbSize]),

    RevKeys = maps:keys(Revs),
    lists:foreach(fun(RevKey) ->
        #doc{
            id = RevDocId,
            revs = {Pos, [Rev | RestRevs]},
            body = Body,
            deleted = Deleted,
            atts = DocAtts,
            meta = Meta
        } = Doc = maps:get(RevKey, Revs),
        ?debugFmt("Doc: ~p (~p)~n", [RevKey, doc_size(Doc)]),
        ?debugFmt("Id: ~p~n", [RevDocId]),
        ?debugFmt("Rev: ~p ~w~n", [Pos, Rev]),
        lists:foreach(fun(R) ->
            ?debugFmt("     ~p~n", [R])
        end, RestRevs),
        ?debugFmt("Deleted: ~p~n", [Deleted]),
        ?debugFmt("Atts:~n", []),
        lists:foreach(fun(Att) ->
            ?debugFmt("  ~p~n", [Att])
        end, DocAtts),
        ?debugFmt("Body: ~p~n", [Body]),
        ?debugFmt("Meta: ~p~n", [Meta]),
        ?debugFmt("~n", [])
    end, lists:sort(RevKeys)),

    AttKeys = maps:keys(Atts),
    ?debugFmt("~n~nAtts:~n", []),
    lists:foreach(fun(AttKey) ->
        Att = maps:get(AttKey, Atts),
        ?debugFmt("Att: ~p (~p)~n", [AttKey, couch_att:external_size(Att)]),
        ?debugFmt("  ~p~n", [Att])
    end, lists:sort(AttKeys)),

    St.


pick_rev(_Revs, Rev) when is_atom(Rev) ->
    Rev;
pick_rev(Revs, {Op, RevList}) when Op == winner; Op == conflict ->
    ChooseFrom = lists:map(fun(Rev) ->
        #doc{
            revs = {S, [R | _]},
            deleted = Deleted
        } = maps:get(Rev, Revs),
        #{
            deleted => Deleted,
            rev_id => {S, R},
            name => Rev
        }
    end, RevList),
    Sorted = fabric2_util:sort_revinfos(ChooseFrom),
    RetRev = case Op of
        winner -> hd(Sorted);
        conflict -> choose(tl(Sorted))
    end,
    maps:get(name, RetRev).


store_rev(Db, St, DbSize, Tgt, #doc{id = <<"_local/", _/binary>>} = Doc) ->
    DbDoc = case fabric2_db:open_doc(Db, Doc#doc.id) of
        {ok, Found} -> Found;
        {not_found, _} -> not_found
    end,
    store_rev(St, DbSize, Tgt, DbDoc);

store_rev(Db, St, DbSize, Tgt, #doc{} = Doc) ->
    #doc{
        id = DocId,
        revs = {Pos, [Rev | _]}
    } = Doc,
    RevId = {Pos, Rev},
    {ok, [{ok, DbDoc}]} = fabric2_db:open_doc_revs(Db, DocId, [RevId], []),
    store_rev(St, DbSize, Tgt, DbDoc).


store_rev(St, DbSize, Tgt, Doc) ->
    #{
        revs := Revs
    } = St,
    ?assert(not maps:is_key(Tgt, Revs)),
    St#{
        revs := maps:put(Tgt, Doc, Revs),
        size := DbSize
    }.


randomize_doc(#doc{} = Doc) ->
    Doc#doc{
        deleted = false,
        body = random_body()
    }.


randomize_deleted_doc(Doc) ->
    NewDoc = case rand:uniform() < 0.05 of
        true -> randomize_doc(Doc);
        false -> Doc#doc{body = {[]}}
    end,
    NewDoc#doc{deleted = true}.


db_size(Info) when is_list(Info) ->
    {sizes, {Sizes}} = lists:keyfind(sizes, 1, Info),
    {<<"external">>, External} = lists:keyfind(<<"external">>, 1, Sizes),
    External;
db_size(Db) when is_map(Db) ->
    {ok, Info} = fabric2_db:get_db_info(Db),
    db_size(Info).


doc_size(#doc{id = <<"_local/", _/binary>>} = Doc) ->
    fabric2_util:ldoc_size(Doc);
doc_size(#doc{} = Doc) ->
    fabric2_util:rev_size(Doc).


-define(MAX_JSON_ELEMENTS, 5).
-define(MAX_STRING_LEN, 10).
-define(MAX_INT, 4294967296).


random_body() ->
    Elems = rand:uniform(?MAX_JSON_ELEMENTS),
    {Obj, _} = random_json_object(Elems),
    Obj.


random_json(MaxElems) ->
    case choose([object, array, terminal]) of
        object -> random_json_object(MaxElems);
        array -> random_json_array(MaxElems);
        terminal -> {random_json_terminal(), MaxElems}
    end.


random_json_object(MaxElems) ->
    NumKeys = rand:uniform(MaxElems + 1) - 1,
    {Props, RemElems} = lists:mapfoldl(fun(_, Acc1) ->
        {Value, Acc2} = random_json(Acc1),
        {{random_json_string(), Value}, Acc2}
    end, MaxElems - NumKeys, lists:seq(1, NumKeys)),
    {{Props}, RemElems}.


random_json_array(MaxElems) ->
    NumItems = rand:uniform(MaxElems + 1) - 1,
    lists:mapfoldl(fun(_, Acc1) ->
        random_json(Acc1)
    end, MaxElems - NumItems, lists:seq(1, NumItems)).


random_json_terminal() ->
    case choose([null, true, false, number, string]) of
        null -> null;
        true -> true;
        false -> false;
        number -> random_json_number();
        string -> random_json_string()
    end.


random_json_number() ->
    AbsValue = case choose([integer, double]) of
        integer -> rand:uniform(?MAX_INT);
        double -> rand:uniform() * rand:uniform()
    end,
    case choose([pos, neg]) of
        pos -> AbsValue;
        neg -> -1 * AbsValue
    end.


random_json_string() ->
    random_string(0, ?MAX_STRING_LEN).


random_attachment(undefined) ->
    random_attachment(random_string(1, 32));

random_attachment(Name) when is_binary(Name) ->
    Type = random_string(1, 32),
    Data = random_string(1, 512),
    Md5 = erlang:md5(Data),
    couch_att:new([
        {name, Name},
        {type, Type},
        {att_len, size(Data)},
        {data, Data},
        {encoding, identity},
        {md5, Md5}
    ]).


random_string(MinLen, MaxLen) ->
    Alphabet = [
        $a, $b, $c, $d, $e, $f, $g, $h, $i, $j, $k, $l, $m,
        $n, $o, $p, $q, $r, $s, $t, $u, $v, $w, $x, $y, $z,
        $A, $B, $C, $D, $E, $F, $G, $H, $I, $J, $K, $L, $M,
        $N, $O, $P, $Q, $R, $S, $T, $U, $V, $W, $Y, $X, $Z,
        $1, $2, $3, $4, $5, $6, $7, $8, $9, $0,
        $!, $@, $#, $$, $%, $^, $&, $*, $(, $),
        $ , ${, $}, $[, $], $", $', $-, $_, $+, $=, $,, $.,
        $\x{1}, $\x{a2}, $\x{20ac}, $\x{10348}
    ],
    Len = MinLen + rand:uniform(MaxLen - MinLen) - 1,
    Str = lists:map(fun(_) ->
        choose(Alphabet)
    end, lists:seq(1, Len)),
    unicode:characters_to_binary(Str).


choose(Options) ->
    Pos = rand:uniform(length(Options)),
    lists:nth(Pos, Options).
