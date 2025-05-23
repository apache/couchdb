% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
% http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

% This module is for the "index object" as in, the data structure
% representing an index. Not to be confused with mango_index which
% contains APIs for managing indexes.

-module(mango_idx).

-export([
    list/1,
    recover/1,

    new/2,
    validate_new/2,
    add/2,
    remove/2,
    from_ddoc/2,
    special/1,

    dbname/1,
    ddoc/1,
    name/1,
    type/1,
    def/1,
    partitioned/1,
    opts/1,
    columns/1,
    is_usable/3,
    start_key/2,
    end_key/2,
    cursor_mod/1,
    idx_mod/1,
    to_json/1,
    delete/4,
    get_usable_indexes/4,
    get_partial_filter_selector/1
]).

-include_lib("couch/include/couch_db.hrl").
-include("mango.hrl").

list(Db) ->
    {ok, Indexes} = ddoc_cache:open(db_to_name(Db), ?MODULE),
    Indexes.

-spec get_usable_indexes(Database, Selector, Options, Kind) -> {[#idx{}], Trace} | no_return() when
    Database :: database(),
    Selector :: selector(),
    Options :: cursor_options(),
    Kind :: cursor_kind(),
    Trace :: trace().
get_usable_indexes(Db, Selector, Opts, Kind) ->
    ExistingIndexes = list(Db),
    GlobalIndexes = mango_cursor:remove_indexes_with_partial_filter_selector(
        ExistingIndexes
    ),
    UserSpecifiedIndex = mango_cursor:maybe_filter_indexes_by_ddoc(ExistingIndexes, Opts),
    UsableIndexes0 = lists:usort(GlobalIndexes ++ UserSpecifiedIndex),
    PartitionIndexes = filter_partition_indexes(UsableIndexes0, Opts),

    SortFields = get_sort_fields(Opts),
    UsabilityCheck = fun(Ix) -> {Ix, is_usable(Ix, Selector, SortFields)} end,
    UsabilityMap = lists:map(UsabilityCheck, PartitionIndexes),
    UsabilityFilter =
        fun({Ix, {Verdict, _}}) ->
            case Verdict of
                true -> [Ix];
                false -> []
            end
        end,

    case {lists:flatmap(UsabilityFilter, UsabilityMap), Kind} of
        {[], find} ->
            mango_sort_error(Db, Opts);
        {UsableIndexes, _} ->
            Trace = #{
                all_indexes => couch_util:set_from_list(ExistingIndexes),
                global_indexes => couch_util:set_from_list(GlobalIndexes),
                partition_indexes => couch_util:set_from_list(PartitionIndexes),
                usable_indexes => couch_util:set_from_list(UsableIndexes),
                usability_map => UsabilityMap
            },
            {UsableIndexes, Trace}
    end.

mango_sort_error(Db, Opts) ->
    case {fabric_util:is_partitioned(Db), is_opts_partitioned(Opts)} of
        {false, _} ->
            ?MANGO_ERROR({no_usable_index, missing_sort_index});
        {true, true} ->
            ?MANGO_ERROR({no_usable_index, missing_sort_index_partitioned});
        {true, false} ->
            ?MANGO_ERROR({no_usable_index, missing_sort_index_global})
    end.

recover(Db) ->
    {ok, DDocs0} = mango_util:open_ddocs(Db),
    Pred = fun({Props}) ->
        case proplists:get_value(<<"language">>, Props) of
            <<"query">> -> true;
            _ -> false
        end
    end,
    DDocs = lists:filter(Pred, DDocs0),
    Special = special(Db),
    {ok,
        Special ++
            lists:flatmap(
                fun(Doc) ->
                    from_ddoc(Db, Doc)
                end,
                DDocs
            )}.

get_sort_fields(Opts) ->
    case lists:keyfind(sort, 1, Opts) of
        {sort, Sort} ->
            mango_sort:fields(Sort);
        _ ->
            []
    end.

new(Db, Opts) ->
    Def = get_idx_def(Opts),
    Type = get_idx_type(Opts),
    IdxName = get_idx_name(Def, Opts),
    DDoc = get_idx_ddoc(Def, Opts),
    {ok, #idx{
        dbname = db_to_name(Db),
        ddoc = DDoc,
        name = IdxName,
        type = Type,
        def = Def,
        partitioned = get_idx_partitioned(Opts),
        opts = filter_opts(Opts)
    }}.

validate_new(Idx, Db) ->
    Mod = idx_mod(Idx),
    Mod:validate_new(Idx, Db).

add(DDoc, Idx) ->
    Mod = idx_mod(Idx),
    {ok, NewDDoc1} = Mod:add(DDoc, Idx),
    NewDDoc2 = set_ddoc_partitioned(NewDDoc1, Idx),
    % Round trip through JSON for normalization
    Body = ?JSON_DECODE(?JSON_ENCODE(NewDDoc2#doc.body)),
    {ok, NewDDoc2#doc{body = Body}}.

remove(DDoc, Idx) ->
    Mod = idx_mod(Idx),
    {ok, NewDDoc} = Mod:remove(DDoc, Idx),
    % Round trip through JSON for normalization
    Body = ?JSON_DECODE(?JSON_ENCODE(NewDDoc#doc.body)),
    {ok, NewDDoc#doc{body = Body}}.

delete(Filt, Db, Indexes, DelOpts) ->
    case lists:filter(Filt, Indexes) of
        [Idx] ->
            {ok, DDoc} = mango_util:load_ddoc(Db, mango_idx:ddoc(Idx)),
            {ok, NewDDoc} = mango_idx:remove(DDoc, Idx),
            FinalDDoc =
                case NewDDoc#doc.body of
                    {[{<<"language">>, <<"query">>}]} ->
                        NewDDoc#doc{deleted = true, body = {[]}};
                    _ ->
                        NewDDoc
                end,
            case mango_crud:insert(Db, FinalDDoc, DelOpts) of
                {ok, _} ->
                    {ok, true};
                Error ->
                    {error, Error}
            end;
        [] ->
            {error, not_found}
    end.

from_ddoc(Db, {Props}) ->
    DbName = db_to_name(Db),
    DDoc = proplists:get_value(<<"_id">>, Props),

    case proplists:get_value(<<"language">>, Props) of
        <<"query">> -> ok;
        _ -> ?MANGO_ERROR(invalid_query_ddoc_language)
    end,
    IdxMods =
        case dreyfus:available() of
            true -> [mango_idx_text];
            false -> []
        end ++
            case nouveau:enabled() of
                true -> [mango_idx_nouveau];
                false -> []
            end ++
            [mango_idx_view],
    Idxs = lists:flatmap(fun(Mod) -> Mod:from_ddoc({Props}) end, IdxMods),
    lists:map(
        fun(Idx) ->
            Idx#idx{
                dbname = DbName,
                ddoc = DDoc,
                partitioned = get_idx_partitioned(Db, Props)
            }
        end,
        Idxs
    ).

special(Db) ->
    AllDocs = #idx{
        dbname = db_to_name(Db),
        name = <<"_all_docs">>,
        type = <<"special">>,
        def = all_docs,
        opts = []
    },
    % Add one for _update_seq
    [AllDocs].

dbname(#idx{dbname = DbName}) ->
    DbName.

ddoc(#idx{ddoc = DDoc}) ->
    DDoc.

name(#idx{name = Name}) ->
    Name.

type(#idx{type = Type}) ->
    Type.

def(#idx{def = Def}) ->
    Def.

partitioned(#idx{partitioned = Partitioned}) ->
    Partitioned.

opts(#idx{opts = Opts}) ->
    Opts.

to_json(#idx{} = Idx) ->
    Mod = idx_mod(Idx),
    Mod:to_json(Idx).

columns(#idx{} = Idx) ->
    Mod = idx_mod(Idx),
    Mod:columns(Idx).

is_usable(#idx{} = Idx, Selector, SortFields) ->
    Mod = idx_mod(Idx),
    Mod:is_usable(Idx, Selector, SortFields).

start_key(#idx{} = Idx, Ranges) ->
    Mod = idx_mod(Idx),
    Mod:start_key(Ranges).

end_key(#idx{} = Idx, Ranges) ->
    Mod = idx_mod(Idx),
    Mod:end_key(Ranges).

cursor_mod(#idx{type = <<"json">>}) ->
    mango_cursor_view;
cursor_mod(#idx{def = all_docs, type = <<"special">>}) ->
    mango_cursor_special;
cursor_mod(#idx{type = <<"nouveau">>}) ->
    case nouveau:enabled() of
        true ->
            mango_cursor_nouveau;
        false ->
            ?MANGO_ERROR({index_service_unavailable, <<"nouveau">>})
    end;
cursor_mod(#idx{type = <<"text">>}) ->
    case dreyfus:available() of
        true ->
            mango_cursor_text;
        false ->
            ?MANGO_ERROR({index_service_unavailable, <<"text">>})
    end.

idx_mod(#idx{type = <<"json">>}) ->
    mango_idx_view;
idx_mod(#idx{type = <<"special">>}) ->
    mango_idx_special;
idx_mod(#idx{type = <<"nouveau">>}) ->
    case nouveau:enabled() of
        true ->
            mango_idx_nouveau;
        false ->
            ?MANGO_ERROR({index_service_unavailable, <<"nouveau">>})
    end;
idx_mod(#idx{type = <<"text">>}) ->
    case dreyfus:available() of
        true ->
            mango_idx_text;
        false ->
            ?MANGO_ERROR({index_service_unavailable, <<"text">>})
    end.

db_to_name(Name) when is_binary(Name) ->
    Name;
db_to_name(Name) when is_list(Name) ->
    iolist_to_binary(Name);
db_to_name(Db) ->
    couch_db:name(Db).

get_idx_def(Opts) ->
    case proplists:get_value(def, Opts) of
        undefined ->
            ?MANGO_ERROR(no_index_definition);
        Def ->
            Def
    end.

get_idx_type(Opts) ->
    case proplists:get_value(type, Opts) of
        <<"json">> ->
            <<"json">>;
        <<"nouveau">> ->
            case nouveau:enabled() of
                true ->
                    <<"nouveau">>;
                false ->
                    ?MANGO_ERROR({index_service_unavailable, <<"nouveau">>})
            end;
        <<"text">> ->
            case dreyfus:available() of
                true ->
                    <<"text">>;
                false ->
                    ?MANGO_ERROR({index_service_unavailable, <<"text">>})
            end;
        %<<"geo">> -> <<"geo">>;
        undefined ->
            <<"json">>;
        BadType ->
            ?MANGO_ERROR({invalid_index_type, BadType})
    end.

get_idx_ddoc(Idx, Opts) ->
    case proplists:get_value(ddoc, Opts) of
        <<"_design/", _Rest/binary>> = Name ->
            Name;
        Name when is_binary(Name) ->
            <<"_design/", Name/binary>>;
        _ ->
            Bin = gen_name(Idx, Opts),
            <<"_design/", Bin/binary>>
    end.

get_idx_name(Idx, Opts) ->
    case proplists:get_value(name, Opts) of
        Name when is_binary(Name) ->
            Name;
        _ ->
            gen_name(Idx, Opts)
    end.

gen_name(Idx, Opts0) ->
    Opts = lists:usort(Opts0),
    TermBin = ?term_to_bin({Idx, Opts}),
    Sha = crypto:hash(sha, TermBin),
    mango_util:enc_hex(Sha).

get_idx_partitioned(Opts) ->
    case proplists:get_value(partitioned, Opts) of
        B when is_boolean(B) ->
            B;
        db_default ->
            % Default to the partitioned setting on
            % the database.
            undefined
    end.

set_ddoc_partitioned(DDoc, Idx) ->
    % We have to verify that the new index being added
    % to this design document either matches the current
    % ddoc's design options *or* this is a new design doc
    #doc{
        id = DDocId,
        revs = Revs,
        body = {BodyProps}
    } = DDoc,
    OldDOpts = couch_util:get_value(<<"options">>, BodyProps),
    OldOpt =
        case OldDOpts of
            {OldDOptProps} when is_list(OldDOptProps) ->
                couch_util:get_value(<<"partitioned">>, OldDOptProps);
            _ ->
                undefined
        end,
    % If new matches old we're done
    if
        Idx#idx.partitioned == OldOpt ->
            DDoc;
        true ->
            % If we're creating a ddoc then we can set the options
            case Revs == {0, []} of
                true when Idx#idx.partitioned /= undefined ->
                    set_ddoc_partitioned_option(DDoc, Idx#idx.partitioned);
                true when Idx#idx.partitioned == undefined ->
                    DDoc;
                false ->
                    ?MANGO_ERROR({partitioned_option_mismatch, DDocId})
            end
    end.

set_ddoc_partitioned_option(DDoc, Partitioned) ->
    #doc{
        body = {BodyProps}
    } = DDoc,
    NewProps =
        case couch_util:get_value(<<"options">>, BodyProps) of
            {Existing} when is_list(Existing) ->
                Opt = {<<"partitioned">>, Partitioned},
                New = lists:keystore(<<"partitioned">>, 1, Existing, Opt),
                lists:keystore(<<"options">>, 1, BodyProps, {<<"options">>, New});
            undefined ->
                New = {<<"options">>, {[{<<"partitioned">>, Partitioned}]}},
                lists:keystore(<<"options">>, 1, BodyProps, New)
        end,
    DDoc#doc{body = {NewProps}}.

get_idx_partitioned(Db, DDocProps) ->
    Default = fabric_util:is_partitioned(Db),
    case couch_util:get_value(<<"options">>, DDocProps) of
        {DesignOpts} ->
            case couch_util:get_value(<<"partitioned">>, DesignOpts) of
                P when is_boolean(P) ->
                    P;
                undefined ->
                    Default
            end;
        undefined ->
            Default
    end.

is_opts_partitioned(Opts) ->
    case couch_util:get_value(partition, Opts, <<>>) of
        <<>> ->
            false;
        Partition when is_binary(Partition) ->
            true
    end.

filter_partition_indexes(Indexes, Opts) ->
    PFilt =
        case is_opts_partitioned(Opts) of
            false ->
                fun(#idx{partitioned = P}) -> not P end;
            true ->
                fun(#idx{partitioned = P}) -> P end
        end,
    Filt = fun(Idx) -> type(Idx) == <<"special">> orelse PFilt(Idx) end,
    lists:filter(Filt, Indexes).

filter_opts([]) ->
    [];
filter_opts([{user_ctx, _} | Rest]) ->
    filter_opts(Rest);
filter_opts([{ddoc, _} | Rest]) ->
    filter_opts(Rest);
filter_opts([{name, _} | Rest]) ->
    filter_opts(Rest);
filter_opts([{type, _} | Rest]) ->
    filter_opts(Rest);
filter_opts([{w, _} | Rest]) ->
    filter_opts(Rest);
filter_opts([{partitioned, _} | Rest]) ->
    filter_opts(Rest);
filter_opts([Opt | Rest]) ->
    [Opt | filter_opts(Rest)].

get_partial_filter_selector(#idx{def = Def}) when Def =:= all_docs; Def =:= undefined ->
    undefined;
get_partial_filter_selector(#idx{def = {Def}}) ->
    case proplists:get_value(<<"partial_filter_selector">>, Def) of
        undefined -> get_legacy_selector(Def);
        {[]} -> undefined;
        Selector -> Selector
    end.

% Partial filter selectors is supported in text indexes via the selector field
% This adds backwards support for existing indexes that might have a selector in it
get_legacy_selector(Def) ->
    case proplists:get_value(<<"selector">>, Def) of
        undefined -> undefined;
        {[]} -> undefined;
        Selector -> Selector
    end.

-ifdef(TEST).
-include_lib("couch/include/couch_eunit.hrl").

get_usable_indexes_test_() ->
    {
        foreach,
        fun() ->
            meck:new(fabric_util)
        end,
        fun(_) ->
            meck:unload()
        end,
        [
            ?TDEF_FE(t_get_usable_indexes_empty),
            ?TDEF_FE(t_get_usable_indexes_regular),
            ?TDEF_FE(t_get_usable_indexes_user_specified_index)
        ]
    }.

t_get_usable_indexes_empty(_) ->
    Database = <<"db">>,
    Options1 = [],
    ExistingIndexes = [],
    GlobalIndexes = [],
    UserSpecifiedIndex = [],
    meck:expect(ddoc_cache, open, [Database, mango_idx], meck:val({ok, ExistingIndexes})),
    meck:expect(
        mango_cursor,
        remove_indexes_with_partial_filter_selector,
        [ExistingIndexes],
        meck:val(GlobalIndexes)
    ),
    meck:expect(
        mango_cursor,
        maybe_filter_indexes_by_ddoc,
        [ExistingIndexes, Options1],
        meck:val(UserSpecifiedIndex)
    ),
    meck:expect(fabric_util, is_partitioned, [Database], meck:val(false)),
    Exception1 = {mango_error, mango_idx, {no_usable_index, missing_sort_index}},
    ?assertThrow(Exception1, get_usable_indexes(Database, selector, Options1, find)),
    meck:expect(fabric_util, is_partitioned, [Database], meck:val(true)),
    Exception2 = {mango_error, mango_idx, {no_usable_index, missing_sort_index_global}},
    ?assertThrow(Exception2, get_usable_indexes(Database, selector, Options1, find)),
    Trace =
        #{
            all_indexes => couch_util:new_set(),
            global_indexes => couch_util:new_set(),
            partition_indexes => couch_util:new_set(),
            usable_indexes => couch_util:new_set(),
            usability_map => []
        },
    ?assertEqual({[], Trace}, get_usable_indexes(Database, selector, Options1, explain)),
    Options2 = [{partition, <<"partition">>}],
    meck:expect(
        mango_cursor,
        maybe_filter_indexes_by_ddoc,
        [ExistingIndexes, Options2],
        meck:val(UserSpecifiedIndex)
    ),
    Exception3 = {mango_error, mango_idx, {no_usable_index, missing_sort_index_partitioned}},
    ?assertThrow(Exception3, get_usable_indexes(Database, selector, Options2, find)).

t_get_usable_indexes_regular(_) ->
    Database = <<"db">>,
    Options = [{sort, sort_options}],
    Index1 = #idx{type = <<"special">>, def = all_docs, partitioned = false},
    Index2 = #idx{type = <<"json">>, name = <<"view_idx1">>, partitioned = false},
    Index3 = #idx{type = <<"json">>, name = <<"view_idx2">>, partitioned = false},
    ExistingIndexes = [Index1, Index2, Index3],
    GlobalIndexes = [Index1, Index2, Index3],
    PartitionIndexes = [Index1, Index2, Index3],
    UserSpecifiedIndex = [],
    UsableIndexes = [Index1, Index2],
    UsabilityMap =
        [
            {Index1, {true, usability_info_1}},
            {Index2, {true, usability_info_2}},
            {Index3, {false, usability_info_3}}
        ],
    Trace =
        #{
            all_indexes => couch_util:set_from_list(ExistingIndexes),
            global_indexes => couch_util:set_from_list(GlobalIndexes),
            partition_indexes => couch_util:set_from_list(PartitionIndexes),
            usable_indexes => couch_util:set_from_list(UsableIndexes),
            usability_map => UsabilityMap
        },
    meck:expect(ddoc_cache, open, [Database, mango_idx], meck:val({ok, ExistingIndexes})),
    meck:expect(
        mango_cursor,
        remove_indexes_with_partial_filter_selector,
        [ExistingIndexes],
        meck:val(GlobalIndexes)
    ),
    meck:expect(mango_sort, fields, [sort_options], meck:val(sort_fields)),
    meck:expect(
        mango_cursor,
        maybe_filter_indexes_by_ddoc,
        [ExistingIndexes, Options],
        meck:val(UserSpecifiedIndex)
    ),
    meck:expect(
        mango_idx_special,
        is_usable,
        [Index1, selector, sort_fields],
        meck:val({true, usability_info_1})
    ),
    meck:expect(
        mango_idx_view,
        is_usable,
        fun(#idx{name = Name}, selector, sort_fields) ->
            case Name of
                <<"view_idx1">> -> {true, usability_info_2};
                <<"view_idx2">> -> {false, usability_info_3}
            end
        end
    ),
    ?assertNot(meck:called(fabric_util, is_partitioned, '_')),
    ?assertEqual({UsableIndexes, Trace}, get_usable_indexes(Database, selector, Options, find)).

t_get_usable_indexes_user_specified_index(_) ->
    Database = <<"db">>,
    Options = [],
    Index1 = #idx{type = <<"special">>, def = all_docs, partitioned = false},
    Index2 = #idx{type = <<"json">>, name = <<"users">>, partitioned = false},
    ExistingIndexes = [Index1, Index2],
    GlobalIndexes = [Index1, Index2],
    PartitionIndexes = [Index1, Index2],
    UserSpecifiedIndex = [Index2],
    UsableIndexes = [Index1, Index2],
    UsabilityMap =
        [
            {Index1, {true, usability_info_1}},
            {Index2, {true, usability_info_2}}
        ],
    Trace =
        #{
            all_indexes => couch_util:set_from_list(ExistingIndexes),
            global_indexes => couch_util:set_from_list(GlobalIndexes),
            partition_indexes => couch_util:set_from_list(PartitionIndexes),
            usable_indexes => couch_util:set_from_list(UsableIndexes),
            usability_map => UsabilityMap
        },
    meck:expect(ddoc_cache, open, [Database, mango_idx], meck:val({ok, ExistingIndexes})),
    meck:expect(
        mango_cursor,
        remove_indexes_with_partial_filter_selector,
        [ExistingIndexes],
        meck:val(GlobalIndexes)
    ),
    meck:expect(
        mango_cursor,
        maybe_filter_indexes_by_ddoc,
        [ExistingIndexes, Options],
        meck:val(UserSpecifiedIndex)
    ),
    meck:expect(
        mango_idx_special,
        is_usable,
        [Index1, selector, []],
        meck:val({true, usability_info_1})
    ),
    meck:expect(
        mango_idx_view,
        is_usable,
        [Index2, selector, []],
        meck:val({true, usability_info_2})
    ),
    ?assertNot(meck:called(fabric_util, is_partitioned, '_')),
    ?assertEqual({UsableIndexes, Trace}, get_usable_indexes(Database, selector, Options, find)).

index(SelectorName, Selector) ->
    {
        idx,
        <<"mango_test_46418cd02081470d93290dc12306ebcb">>,
        <<"_design/57e860dee471f40a2c74ea5b72997b81dda36a24">>,
        <<"Selected">>,
        <<"json">>,
        {[
            {<<"fields">>, {[{<<"location">>, <<"asc">>}]}},
            {SelectorName, {Selector}}
        ]},
        false,
        [{<<"def">>, {[{<<"fields">>, [<<"location">>]}]}}]
    }.

get_partial_filter_all_docs_test() ->
    Idx = #idx{def = all_docs},
    ?assertEqual(undefined, get_partial_filter_selector(Idx)).

get_partial_filter_undefined_def_test() ->
    Idx = #idx{def = undefined},
    ?assertEqual(undefined, get_partial_filter_selector(Idx)).

get_partial_filter_selector_default_test() ->
    Idx = index(<<"partial_filter_selector">>, []),
    ?assertEqual(undefined, get_partial_filter_selector(Idx)).

get_partial_filter_selector_missing_test() ->
    Idx = index(<<"partial_filter_selector">>, []),
    ?assertEqual(undefined, get_partial_filter_selector(Idx)).

get_partial_filter_selector_with_selector_test() ->
    Selector = [{<<"location">>, {[{<<"$gt">>, <<"FRA">>}]}}],
    Idx = index(<<"partial_filter_selector">>, Selector),
    ?assertEqual({Selector}, get_partial_filter_selector(Idx)).

get_partial_filter_selector_with_legacy_selector_test() ->
    Selector = [{<<"location">>, {[{<<"$gt">>, <<"FRA">>}]}}],
    Idx = index(<<"selector">>, Selector),
    ?assertEqual({Selector}, get_partial_filter_selector(Idx)).

get_partial_filter_selector_with_legacy_default_selector_test() ->
    Idx = index(<<"selector">>, []),
    ?assertEqual(undefined, get_partial_filter_selector(Idx)).

get_idx_ddoc_name_only_test() ->
    Opts = [{ddoc, <<"foo">>}],
    ?assertEqual(<<"_design/foo">>, get_idx_ddoc({}, Opts)).

get_idx_ddoc_design_slash_name_test() ->
    Opts = [{ddoc, <<"_design/foo">>}],
    ?assertEqual(<<"_design/foo">>, get_idx_ddoc({}, Opts)).

-endif.
