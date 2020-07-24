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

-module(couch_views_size_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").
-include_lib("fabric/include/fabric2.hrl").
-include_lib("couch_views/include/couch_views.hrl").

% N.B., we should move to couch_ejson_size instead
% of erlang:external_size
%
% to calculate view size:
% total = 0
% for (fdb_k, fdb_v) in VIEW_MAP_RANGE:
%   {EncUserKey, EncUserval} = erlfdb_tuple:unpack(fdb_v),
%   UserKey = couch_views_encoding:decode(EncUserKey),
%   UserVal = couch_views_encoding:decode(EncUserVal),
%   total += erlang:external_size(UserKey),
%   total += erlang:external_size(UserVal)
%
% Our goal in checking the size calculations is that we cover
% as much of the possible key mutation space as possible while
% not relying on fuzzing out the edge cases. Conceptually we have
% two sets of keys E and U. E is keys as currently exist in the
% view, and U is the new set of keys corresponding to an update.
%
% Both sets E and U have the same possible set of state variables:
%
% 1. N unique keys, where 0 =< N =< infinity
% 2. D keys with duplicates, where 0 =< D =< N,
% 3. R repeats for each member of D, for 2 =< R =< infinity
%
% Given two sets S1 and S2, we then have a set of transition variables:
%
% 1. deltaN - shared unique keys, where 0 =< deltaN =< N
% 2. deltaD - shared duplicates, where 0 =< deltaD =< N
% 3. deltaR - shared repeats for each D, where 2 =< deltaR =< infinity
%
% To search our state transition space, we can create two functions to
% first define our start and end states, and for each transition we have
% a function that defines the shared overlap between states.
%
% Given a list of transitions are checks then become simple in that
% we can iterate over each transition checking that our index is valid
% after each one. Index validation will purely look at the existing
% state of the index in fdb and validate correctness.

-define(NUM_SINGLE_TESTS, 100).
-define(NUM_MULTI_TESTS, 100).

-define(N_DOMAIN, [0, 1, 2, 5]).
-define(D_DOMAIN, [0, 1, 2, 5]).
-define(R_DOMAIN, [2, 4]).

-define(DELTA_N_DOMAIN, [0, 1, 2, 5]).
-define(DELTA_D_DOMAIN, [0, 1, 2, 5]).
-define(DELTA_R_DOMAIN, [1, 2, 4]).


generate_sets() ->
    permute(?N_DOMAIN, ?D_DOMAIN, ?R_DOMAIN, fun(N, D, R) ->
        % We can't have more duplicates than total keys
        case D > N of
            true -> throw(skip);
            false -> ok
        end,

        % Only include one of the repeat values
        % for our zero sets
        case D == 0 of
            true when R == 2 -> ok;
            true -> throw(skip);
            false -> ok
        end,

        % Replace R with a sentinel value for sanity
        % when there are no dupes to have repeats
        ActualR = if D == 0 -> 0; true -> R end,

        {N, D, ActualR}
    end).


generate_transitions() ->
    Sets = generate_sets(),
    Pairs = [{Set1, Set2} || Set1 <- Sets, Set2 <- Sets],
    lists:flatmap(fun({{N1, D1, _R1} = S1, {N2, D2, _R2} = S2}) ->
        Filter = fun(DeltaN, DeltaD, DeltaR) ->
            % Can't share more keys than the smaller of the
            % two sets
            case DeltaN > min(N1, N2) of
                true -> throw(skip);
                false -> ok
            end,

            % For DeltaD == 0, all combinations of DeltaD and
            % DeltaR are equivalent tests
            case DeltaN == 0 of
                true when DeltaD == 0, DeltaR == 1 -> ok;
                true -> throw(skip);
                false -> ok
            end,

            % Can't share more dupes than exist in either set
            % or the total number of shared keys
            case DeltaD > min(D1, D2) orelse DeltaD > DeltaN of
                true -> throw(skip);
                false -> ok
            end,

            % For DeltaD == 0, all DeltaR correspond to the
            % same test so only include one instance
            case DeltaD == 0 of
                true when DeltaR == 1 -> ok;
                true -> throw(skip);
                false -> ok
            end,

            % If we have more non-repeated keys in our
            % transition than there's "room" for in the target
            % set it isn't a valid test case.
            TransitionNonRepeats = DeltaN - DeltaD,
            TargetNonRepeats = N2 - D2,
            case TransitionNonRepeats > TargetNonRepeats of
                true -> throw(skip);
                false -> ok
            end,

            {S1, S2, {DeltaN, DeltaD, DeltaR}}
        end,
        permute(?DELTA_N_DOMAIN, ?DELTA_D_DOMAIN, ?DELTA_R_DOMAIN, Filter)
    end, Pairs).


permute(NList, DList, RList, Filter) ->
    % Technically we could call into Filter in each
    % outer loops to conditionally skip inner loops.
    % If someone comes along looking to speed up the
    % fixture setup time, this would likely be an
    % easy win.
    lists:foldl(fun(N, NAcc) ->
        lists:foldl(fun(D, DAcc) ->
            lists:foldl(fun(R, RAcc) ->
                try
                    [Filter(N, D, R) | RAcc]
                catch throw:skip ->
                    RAcc
                end
            end, DAcc, RList)
        end, NAcc, DList)
    end, [], NList).


row_transition_test_() ->
    {
        "Test view size tracking",
        {
            setup,
            fun setup/0,
            fun cleanup/1,
            fun create_transition_tests/1
        }
    }.


setup() ->
    Ctx = test_util:start_couch([
            fabric,
            couch_jobs,
            couch_js,
            couch_views
        ]),
    {ok, Db} = fabric2_db:create(?tempdb(), [{user_ctx, ?ADMIN_USER}]),
    {Ctx, Db}.


cleanup({Ctx, Db}) ->
    ok = fabric2_db:delete(fabric2_db:name(Db), []),
    test_util:stop_couch(Ctx).


create_transition_tests({_Ctx, Db}) ->
    try
        throw(disabled),
        Transitions = generate_transitions(),
        Single = lists:flatmap(fun(T) ->
            Name = lists:flatten(io_lib:format("single ~s", [tname(T)])),
            [{Name, fun() -> check_single_transition(Db, T) end}]
        end, lists:sort(Transitions)),
        Multi = lists:flatmap(fun(T) ->
            Name = lists:flatten(io_lib:format("multi ~s", [tname(T)])),
            [{Name, fun() -> check_multi_transition(Db, T) end}]
        end, lists:sort(group(shuffle(Transitions)))),
        subset(?NUM_SINGLE_TESTS, Single) ++ subset(?NUM_MULTI_TESTS, Multi)
    catch throw:disabled ->
        [{"Disabled", fun() -> ok end}]
    end.


check_single_transition(Db, {Set1, Set2, Transition}) ->
    clear_views(Db),
    InitKVs = init_set(Set1, [a, b, c, d, e]),
    CommonKVs = reduce_set(Transition, InitKVs),
    FinalKVs = fill_set(Set2, CommonKVs, [v, w, x, y, z]),
    {InitJSONKVs, Bindings} = unlabel(InitKVs, #{}),
    {FinalJSONKVs, _} = unlabel(FinalKVs, Bindings),

    Sig = couch_uuids:random(),
    DocId = couch_uuids:random(),

    fabric2_fdb:transactional(Db, fun(TxDb) ->
        write_docs(TxDb, Sig, [make_doc(DocId, InitJSONKVs)])
    end),

    fabric2_fdb:transactional(Db, fun(TxDb) ->
        write_docs(TxDb, Sig, [make_doc(DocId, FinalJSONKVs)])
    end),

    validate_index(Db, Sig, #{DocId => FinalJSONKVs}).


check_multi_transition(Db, Transitions) ->
    clear_views(Db),

    {Docs, IdMap} = lists:mapfoldl(fun({Set1, Set2, Transition}, IdMapAcc) ->
        DocId = couch_uuids:random(),
        InitKVs = init_set(Set1, [a, b, c, d, e]),
        CommonKVs = reduce_set(Transition, InitKVs),
        FinalKVs = fill_set(Set2, CommonKVs, [v, w, x, y, z]),
        {InitJSONKVs, Bindings} = unlabel(InitKVs, #{}),
        {FinalJSONKVs, _} = unlabel(FinalKVs, Bindings),
        InitDoc = make_doc(DocId, InitJSONKVs),
        FinalDoc = make_doc(DocId, FinalJSONKVs),
        {{InitDoc, FinalDoc}, maps:put(DocId, FinalJSONKVs, IdMapAcc)}
    end, #{}, Transitions),

    {InitDocs, FinalDocs} = lists:unzip(Docs),

    Sig = couch_uuids:random(),

    fabric2_fdb:transactional(Db, fun(TxDb) ->
        write_docs(TxDb, Sig, InitDocs)
    end),

    fabric2_fdb:transactional(Db, fun(TxDb) ->
        write_docs(TxDb, Sig, FinalDocs)
    end),

    validate_index(Db, Sig, IdMap).


clear_views(Db) ->
    fabric2_fdb:transactional(Db, fun(TxDb) ->
        #{
            tx := Tx,
            db_prefix := DbPrefix
        } = TxDb,
        {Start, End} = erlfdb_tuple:range({?DB_VIEWS}, DbPrefix),
        erlfdb:clear_range(Tx, Start, End),

        GlobalKey = {?DB_STATS, <<"sizes">>, <<"views">>},
        BinGlobalKey = erlfdb_tuple:pack(GlobalKey, DbPrefix),
        erlfdb:set(Tx, BinGlobalKey, ?uint2bin(0))
    end).


write_docs(TxDb, Sig, Docs) ->
    Mrst = #mrst{
        sig = Sig,
        views = [#mrview{
            id_num = 1
        }]
    },
    IdxState = #{
        last_seq => <<"foo">>
    },
    couch_views_indexer:write_docs(TxDb, Mrst, Docs, IdxState).


validate_index(Db, Sig, JSONRows) ->
    #{
        db_prefix := DbPrefix
    } = Db,
    Rows = fabric2_fdb:transactional(Db, fun(TxDb) ->
        #{
            tx := Tx
        } = TxDb,
        {Start, End} = erlfdb_tuple:range({?DB_VIEWS}, DbPrefix),
        erlfdb:get_range(Tx, Start, End)
    end),

    InitAcc = #{
        row_count => 0,
        kv_size => 0,
        ids => #{},
        rows => []
    },

    MapData = lists:foldl(fun({Key, Value}, Acc) ->
        case erlfdb_tuple:unpack(Key, DbPrefix) of
            {?DB_VIEWS, ?VIEW_INFO, ?VIEW_UPDATE_SEQ, Sig} ->
                ?assertEqual(<<"foo">>, Value),
                Acc;
            {?DB_VIEWS, ?VIEW_INFO, ?VIEW_ROW_COUNT, Sig, 1} ->
                maps:put(row_count, ?bin2uint(Value), Acc);
            {?DB_VIEWS, ?VIEW_INFO, ?VIEW_KV_SIZE, Sig, 1} ->
                maps:put(kv_size, ?bin2uint(Value), Acc);
            {?DB_VIEWS, ?VIEW_DATA, Sig, ?VIEW_ID_RANGE, DocId, 1} ->
                [
                    TotalKeys, TotalSize, UniqueKeys
                ] = couch_views_encoding:decode(Value),
                maps:update_with(ids, fun(Ids) ->
                    false = maps:is_key(DocId, Ids),
                    maps:put(DocId, {TotalKeys, TotalSize, UniqueKeys}, Ids)
                end, Acc);
            {?DB_VIEWS, ?VIEW_DATA, Sig, ?VIEW_MAP_RANGE, 1, MapKey, _DupeId} ->
                {EncKey, DocId} = MapKey,
                {UserKey, UserVal} = erlfdb_tuple:unpack(Value),

                UserJsonKey = couch_views_encoding:decode(UserKey),
                UserJsonVal = couch_views_encoding:decode(UserVal),

                ?assertEqual(
                        EncKey,
                        couch_views_encoding:encode(UserJsonKey, key)
                    ),

                maps:update_with(rows, fun(RAcc) ->
                    [{DocId, UserJsonKey, UserJsonVal} | RAcc]
                end, Acc)
        end
    end, InitAcc, Rows),

    #{
        row_count := RowCount,
        kv_size := KVSize,
        ids := MapIds,
        rows := MapRows
    } = MapData,

    SumFun = fun(_DocId, {TotalKVs, TotalSize, _UniqueKeys}, {KVAcc, SAcc}) ->
        {KVAcc + TotalKVs, SAcc + TotalSize}
    end,
    {SumKVCount, SumKVSize} = maps:fold(SumFun, {0, 0}, MapIds),
    ?assertEqual(RowCount, length(MapRows)),
    ?assertEqual(RowCount, SumKVCount),
    ?assertEqual(KVSize, SumKVSize),
    ?assert(KVSize >= 0),

    fabric2_fdb:transactional(Db, fun(TxDb) ->
        GlobalSize = get_global_size(TxDb),
        ?assertEqual(KVSize, GlobalSize),

        ViewSize = couch_views_fdb:get_kv_size(TxDb, #mrst{sig = Sig}, 1),
        ?assertEqual(KVSize, ViewSize)
    end),

    % Compare our raw JSON rows to what was indexed
    IdsFromJSONRows = maps:fold(fun(DocId, DocRows, IdAcc) ->
        FinalAcc = lists:foldl(fun({JsonKey, JsonVal}, {CAcc, SAcc, UAcc}) ->
            KeySize = erlang:external_size(JsonKey),
            ValSize = erlang:external_size(JsonVal),
            NewUnique = lists:usort([JsonKey | UAcc]),
            {CAcc + 1, SAcc + KeySize + ValSize, NewUnique}
        end, {0, 0, []}, DocRows),
        if FinalAcc == {0, 0, []} -> IdAcc; true ->
            maps:put(DocId, FinalAcc, IdAcc)
        end
    end, #{}, JSONRows),
    ?assertEqual(MapIds, IdsFromJSONRows),

    % Compare the found id entries to our row data
    IdsFromMapRows = lists:foldl(fun({DocId, JsonKey, JsonVal}, Acc) ->
        KeySize = erlang:external_size(JsonKey),
        ValSize = erlang:external_size(JsonVal),
        Default = {1, KeySize + ValSize, [JsonKey]},
        maps:update_with(DocId, fun({TotalKVs, TotalSize, UniqueKeys}) ->
            NewUnique = lists:usort([JsonKey | UniqueKeys]),
            {TotalKVs + 1, TotalSize + KeySize + ValSize, NewUnique}
        end, Default, Acc)
    end, #{}, MapRows),
    ?assertEqual(MapIds, IdsFromMapRows).


make_doc(DocId, []) ->
    case rand:uniform() < 0.5 of
        true ->
            #{
                id => DocId,
                deleted => true,
                results => [[]]
            };
        false ->
            #{
                id => DocId,
                deleted => false,
                results => [[]]
            }
    end;
make_doc(DocId, Results) ->
    #{
        id => DocId,
        deleted => false,
        results => [Results]
    }.


get_global_size(TxDb) ->
    #{
        tx := Tx,
        db_prefix := DbPrefix
    } = TxDb,
    GlobalKey = {?DB_STATS, <<"sizes">>, <<"views">>},
    BinGlobalKey = erlfdb_tuple:pack(GlobalKey, DbPrefix),
    ?bin2uint(erlfdb:wait(erlfdb:get(Tx, BinGlobalKey))).


init_set({N, D, R}, Labels) ->
    {Dupes, RestLabels} = fill_keys(D, Labels, []),
    {Unique, _} = fill_keys(N - D, RestLabels, []),
    % Sanity assertions
    N = length(Unique) + length(Dupes),
    D = length(Dupes),
    {Unique, [{Key, R} || Key <- Dupes]}.


reduce_set({DeltaN, DeltaD, DeltaR}, {Unique, Dupes}) ->
    NewDupes = lists:sublist(Dupes, DeltaD),
    NewUnique = lists:sublist(Unique, DeltaN - DeltaD),
    {NewUnique, [{Key, DeltaR} || {Key, _} <- NewDupes]}.


fill_set({N, D, R}, {Unique, Dupes}, Labels) ->
    AddDupes = D - length(Dupes),
    {NewDupes, RestLabels} = fill_keys(AddDupes, Labels, Dupes),

    AddUnique = N - length(Unique) - length(NewDupes),
    {NewUnique, _} = fill_keys(AddUnique, RestLabels, Unique),
    % Sanity assertions
    N = length(NewUnique) + length(NewDupes),
    D = length(NewDupes),
    {NewUnique, lists:map(fun(Dupe) ->
        case Dupe of
            {_, _} -> Dupe;
            A when is_atom(A) -> {A, R}
        end
    end, NewDupes)}.


fill_keys(0, Labels, Acc) ->
    {Acc, Labels};
fill_keys(Count, [Label | RestLabels], Acc) when Count > 0 ->
    fill_keys(Count - 1, RestLabels, [Label | Acc]).


unlabel({Unique, Dupes}, Bindings) ->
    lists:foldl(fun(Item, {KVAcc, BindingsAcc}) ->
        {KVs, NewBindingsAcc} = unlabel_item(Item, BindingsAcc),
        {KVs ++ KVAcc, NewBindingsAcc}
    end, {[], Bindings}, Unique ++ Dupes).


unlabel_item(Label, Bindings) when is_atom(Label) ->
    NewBindings = maybe_bind(Label, Bindings),
    KV = maps:get(Label, NewBindings),
    {[KV], NewBindings};
unlabel_item({Label, Count}, Bindings) when is_atom(Label), is_integer(Count) ->
    NewBindings = maybe_bind(Label, Bindings),
    {K, _} = KV = maps:get(Label, NewBindings),
    ToAdd = lists:map(fun(_) ->
        {K, gen_value()}
    end,  lists:seq(1, Count - 1)),
    {[KV | ToAdd], NewBindings}.


maybe_bind(Label, Bindings) ->
    case maps:is_key(Label, Bindings) of
        true ->
            case rand:uniform() < 0.5 of
                true ->
                    rebind(Label, Bindings);
                false ->
                    Bindings
            end;
        false ->
            bind(Label, Bindings)
    end.


bind(Label, Bindings) ->
    maps:put(Label, {gen_key(), gen_value()}, Bindings).


rebind(Label, Bindings) ->
    {Key, _} = maps:get(Label, Bindings),
    maps:put(Label, {Key, gen_value()}, Bindings).


gen_key() ->
    Unique = couch_uuids:random(),
    case rand:uniform() of
        N when N < 0.2 ->
            [Unique, true, rand:uniform()];
        N when N < 0.4 ->
            {[{Unique, true}, {<<"foo">>, [<<"bar">>, null, 1, {[]}]}]};
        _ ->
            Unique
    end.


gen_value() ->
    case rand:uniform() of
        N when N < 0.2 ->
            [false, rand:uniform(), {[]}];
        N when N < 0.4 ->
            {[{<<"a">>, 1}, {<<"b">>, 2}]};
        N when N < 0.6 ->
            rand:uniform(100);
        N when N < 0.8 ->
            rand:uniform();
        _ ->
            1
    end.


group(Items) ->
    case length(Items) > 5 of
        true ->
            {Group, Rest} = lists:split(5, Items),
            [lists:sort(Group) | group(Rest)];
        false when Items == [] ->
            [];
        false ->
            [lists:sort(Items)]
    end.


shuffle(Items) ->
    Tagged = [{rand:uniform(), I} || I <- Items],
    Sorted = lists:sort(Tagged),
    [I || {_T, I} <- Sorted].


subset(Count, Items) ->
    Random = shuffle(Items),
    Take = lists:sublist(Random, Count),
    lists:sort(Take).


tname([]) ->
    [];
tname([Transition | RestTransitions]) ->
    [tname(Transition) | tname(RestTransitions)];
tname({{N1, D1, R1}, {N2, D2, R2}, {DN, DD, DR}}) ->
    io_lib:format("~b~b~b~b~b~b~b~b~b", [N1, D1, R1, N2, D2, R2, DN, DD, DR]).

