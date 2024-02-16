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

-module(mango_cursor_text).

-ifdef(HAVE_DREYFUS).

-export([
    create/4,
    explain/1,
    execute/3
]).

-include_lib("couch/include/couch_db.hrl").
-include_lib("dreyfus/include/dreyfus.hrl").
-include("mango_cursor.hrl").
-include("mango.hrl").

-record(cacc, {
    selector,
    dbname,
    ddocid,
    idx_name,
    query_args,
    bookmark,
    limit,
    skip,
    user_fun,
    user_acc,
    fields,
    execution_stats,
    documents_seen
}).

create(Db, {Indexes, Trace}, Selector, Opts0) ->
    Index =
        case Indexes of
            [Index0] ->
                Index0;
            _ ->
                ?MANGO_ERROR(multiple_text_indexes)
        end,

    Opts = unpack_bookmark(couch_db:name(Db), Opts0),

    DreyfusLimit = get_dreyfus_limit(),
    Limit = erlang:min(DreyfusLimit, couch_util:get_value(limit, Opts, mango_opts:default_limit())),
    Skip = couch_util:get_value(skip, Opts, 0),
    Fields = couch_util:get_value(fields, Opts, all_fields),

    {ok, #cursor{
        db = Db,
        index = Index,
        ranges = null,
        trace = Trace,
        selector = Selector,
        opts = Opts,
        limit = Limit,
        skip = Skip,
        fields = Fields
    }}.

explain(Cursor) ->
    #cursor{
        selector = Selector,
        opts = Opts
    } = Cursor,
    [
        {query, mango_selector_text:convert(Selector)},
        {partition, get_partition(Opts, null)},
        {sort, sort_query(Opts, Selector)}
    ].

execute(Cursor, UserFun, UserAcc) ->
    #cursor{
        db = Db,
        index = Idx,
        limit = Limit,
        skip = Skip,
        selector = Selector,
        opts = Opts,
        execution_stats = Stats
    } = Cursor,
    DbName = couch_db:name(Db),
    Query = mango_selector_text:convert(Selector),
    QueryArgs = #index_query_args{
        q = Query,
        partition = get_partition(Opts, nil),
        sort = sort_query(Opts, Selector),
        raw_bookmark = true
    },
    CAcc = #cacc{
        selector = Selector,
        dbname = DbName,
        ddocid = ddocid(Idx),
        idx_name = mango_idx:name(Idx),
        bookmark = get_bookmark(Opts),
        limit = Limit,
        skip = Skip,
        query_args = QueryArgs,
        user_fun = UserFun,
        user_acc = UserAcc,
        fields = Cursor#cursor.fields,
        execution_stats = mango_execution_stats:log_start(Stats, DbName),
        documents_seen = sets:new([{version, 2}])
    },
    try
        case Query of
            <<>> ->
                throw({stop, CAcc});
            _ ->
                execute(CAcc)
        end
    catch
        throw:{stop, FinalCAcc} ->
            #cacc{
                bookmark = FinalBM,
                user_fun = UserFun,
                user_acc = LastUserAcc,
                execution_stats = Stats0
            } = FinalCAcc,
            JsonBM = dreyfus_bookmark:pack(FinalBM),
            Arg = {add_key, bookmark, JsonBM},
            {_Go, FinalUserAcc} = UserFun(Arg, LastUserAcc),
            {FinalUserAcc0, Stats1} = mango_execution_stats:maybe_add_stats(
                Opts, UserFun, Stats0, FinalUserAcc
            ),
            %% This needs Stats1 as log_end is called in maybe_add_stats
            mango_execution_stats:log_stats(Stats1),
            FinalUserAcc1 = mango_cursor:maybe_add_warning(UserFun, Cursor, Stats1, FinalUserAcc0),
            {ok, FinalUserAcc1}
    end.

execute(CAcc) ->
    case search_docs(CAcc) of
        {ok, Bookmark, []} ->
            % If we don't have any results from the
            % query it means the request has paged through
            % all possible results and the request is over.
            NewCAcc = CAcc#cacc{bookmark = Bookmark},
            throw({stop, NewCAcc});
        {ok, Bookmark, Hits} ->
            NewCAcc = CAcc#cacc{bookmark = Bookmark},
            HitDocs = get_json_docs(CAcc#cacc.dbname, Hits),
            {ok, FinalCAcc} = handle_hits(NewCAcc, HitDocs),
            execute(FinalCAcc)
    end.

search_docs(CAcc) ->
    #cacc{
        dbname = DbName,
        ddocid = DDocId,
        idx_name = IdxName
    } = CAcc,
    QueryArgs = update_query_args(CAcc),
    case dreyfus_fabric_search:go(DbName, DDocId, IdxName, QueryArgs) of
        {ok, Bookmark, _, Hits, _, _} ->
            {ok, Bookmark, Hits};
        {error, Reason} ->
            ?MANGO_ERROR({text_search_error, {error, Reason}})
    end.

handle_hits(CAcc, []) ->
    {ok, CAcc};
handle_hits(CAcc0, [{Sort, Doc} | Rest]) ->
    CAcc1 = handle_hit(CAcc0, Sort, Doc),
    handle_hits(CAcc1, Rest).

handle_hit(CAcc0, Sort, not_found) ->
    CAcc1 = update_bookmark(CAcc0, Sort),
    CAcc1;
handle_hit(CAcc0, Sort, Doc) ->
    #cacc{
        limit = Limit,
        skip = Skip,
        execution_stats = Stats0,
        documents_seen = Seen
    } = CAcc0,
    CAcc1 = update_bookmark(CAcc0, Sort),
    Stats1 = mango_execution_stats:incr_keys_examined(Stats0),
    Stats = mango_execution_stats:incr_docs_examined(Stats1),
    couch_stats:increment_counter([mango, docs_examined]),
    CAcc2 = CAcc1#cacc{execution_stats = Stats},
    case mango_selector:match(CAcc2#cacc.selector, Doc) of
        true ->
            DocId = mango_doc:get_field(Doc, <<"_id">>),
            case sets:is_element(DocId, Seen) of
                true ->
                    CAcc2;
                false ->
                    CAcc3 = CAcc2#cacc{
                        documents_seen = sets:add_element(DocId, Seen)
                    },
                    if
                        Skip > 0 ->
                            CAcc3#cacc{skip = Skip - 1};
                        Limit == 0 ->
                            % We hit this case if the user specified
                            % with a zero limit. Notice that in this
                            % case we need to return the bookmark from
                            % before this match.
                            throw({stop, CAcc0});
                        Limit == 1 ->
                            CAcc4 = apply_user_fun(CAcc3, Doc),
                            throw({stop, CAcc4});
                        Limit > 1 ->
                            CAcc4 = apply_user_fun(CAcc3, Doc),
                            CAcc4#cacc{limit = Limit - 1}
                    end
            end;
        false ->
            CAcc2
    end.

apply_user_fun(CAcc, Doc) ->
    FinalDoc = mango_fields:extract(Doc, CAcc#cacc.fields),
    #cacc{
        user_fun = UserFun,
        user_acc = UserAcc,
        execution_stats = Stats
    } = CAcc,
    Stats0 = mango_execution_stats:incr_results_returned(Stats),
    case UserFun({row, FinalDoc}, UserAcc) of
        {ok, NewUserAcc} ->
            CAcc#cacc{user_acc = NewUserAcc, execution_stats = Stats0};
        {stop, NewUserAcc} ->
            throw({stop, CAcc#cacc{user_acc = NewUserAcc, execution_stats = Stats0}})
    end.

%% Convert Query to Dreyfus sort specifications
%% Covert <<"Field">>, <<"desc">> to <<"-Field">>
%% and append to the dreyfus query
sort_query(Opts, Selector) ->
    {sort, {Sort}} = lists:keyfind(sort, 1, Opts),
    SortList = lists:map(
        fun(SortField) ->
            {Dir, RawSortField} =
                case SortField of
                    {Field, <<"asc">>} -> {asc, Field};
                    {Field, <<"desc">>} -> {desc, Field};
                    Field when is_binary(Field) -> {asc, Field}
                end,
            SField = mango_selector_text:append_sort_type(RawSortField, Selector),
            case Dir of
                asc ->
                    SField;
                desc ->
                    <<"-", SField/binary>>
            end
        end,
        Sort
    ),
    case SortList of
        [] -> relevance;
        _ -> SortList
    end.

get_partition(Opts, Default) ->
    case couch_util:get_value(partition, Opts) of
        <<>> -> Default;
        Else -> Else
    end.

get_bookmark(Opts) ->
    case lists:keyfind(bookmark, 1, Opts) of
        {_, BM} when is_list(BM), BM /= [] ->
            BM;
        _ ->
            nil
    end.

update_bookmark(CAcc, Sortable) ->
    BM = CAcc#cacc.bookmark,
    QueryArgs = CAcc#cacc.query_args,
    Sort = QueryArgs#index_query_args.sort,
    NewBM = dreyfus_bookmark:update(Sort, BM, [Sortable]),
    CAcc#cacc{bookmark = NewBM}.

pack_bookmark(Bookmark) ->
    case dreyfus_bookmark:pack(Bookmark) of
        null -> nil;
        Enc -> Enc
    end.

unpack_bookmark(DbName, Opts) ->
    NewBM =
        case lists:keyfind(bookmark, 1, Opts) of
            {_, nil} ->
                [];
            {_, Bin} ->
                try
                    dreyfus_bookmark:unpack(DbName, Bin)
                catch
                    _:_ ->
                        ?MANGO_ERROR({invalid_bookmark, Bin})
                end
        end,
    lists:keystore(bookmark, 1, Opts, {bookmark, NewBM}).

ddocid(Idx) ->
    case mango_idx:ddoc(Idx) of
        <<"_design/", Rest/binary>> ->
            Rest;
        Else ->
            Else
    end.

update_query_args(CAcc) ->
    #cacc{
        bookmark = Bookmark,
        query_args = QueryArgs
    } = CAcc,
    QueryArgs#index_query_args{
        bookmark = pack_bookmark(Bookmark),
        limit = get_limit(CAcc)
    }.

get_limit(CAcc) ->
    erlang:min(get_dreyfus_limit(), CAcc#cacc.limit + CAcc#cacc.skip).

get_dreyfus_limit() ->
    config:get_integer("dreyfus", "max_limit", 200).

get_json_docs(DbName, Hits) ->
    Ids = lists:map(
        fun(#sortable{item = Item}) ->
            couch_util:get_value(<<"_id">>, Item#hit.fields)
        end,
        Hits
    ),
    % TODO: respect R query parameter (same as json indexes)
    {ok, IdDocs} = dreyfus_fabric:get_json_docs(DbName, Ids),
    lists:map(
        fun(#sortable{item = Item} = Sort) ->
            Id = couch_util:get_value(<<"_id">>, Item#hit.fields),
            case lists:keyfind(Id, 1, IdDocs) of
                {Id, {doc, Doc}} ->
                    {Sort, Doc};
                false ->
                    {Sort, not_found}
            end
        end,
        Hits
    ).

%%%%%%%% module tests %%%%%%%%

-ifdef(TEST).
-include_lib("couch/include/couch_eunit.hrl").

% This behavior needs to be revisited and potentially fixed, the tests
% below are only to record the current version.

create_test_() ->
    {
        foreach,
        fun() -> meck:expect(couch_db, name, [db], meck:val(db_name)) end,
        fun(_) -> meck:unload() end,
        [
            ?TDEF_FE(t_create_no_indexes),
            ?TDEF_FE(t_create_multiple_indexes),
            ?TDEF_FE(t_create_regular),
            ?TDEF_FE(t_create_no_bookmark),
            ?TDEF_FE(t_create_invalid_bookmark)
        ]
    }.

t_create_no_indexes(_) ->
    Exception = {mango_error, mango_cursor_text, multiple_text_indexes},
    ?assertThrow(Exception, create(db, {[], trace}, selector, options)).

t_create_multiple_indexes(_) ->
    Indexes = [index1, index2, index3],
    Exception = {mango_error, mango_cursor_text, multiple_text_indexes},
    ?assertThrow(Exception, create(db, {Indexes, trace}, selector, options)).

t_create_regular(_) ->
    Index = #idx{type = <<"text">>},
    Indexes = [Index],
    Trace = #{},
    Limit = 10,
    Options = [{limit, Limit}, {skip, skip}, {fields, fields}, {bookmark, bookmark}],
    Options1 = [{limit, Limit}, {skip, skip}, {fields, fields}, {bookmark, unpacked_bookmark}],
    Cursor = #cursor{
        db = db,
        index = Index,
        ranges = null,
        trace = Trace,
        selector = selector,
        opts = Options1,
        limit = Limit,
        skip = skip,
        fields = fields
    },
    meck:expect(dreyfus_bookmark, unpack, [db_name, bookmark], meck:val(unpacked_bookmark)),
    ?assertEqual({ok, Cursor}, create(db, {Indexes, Trace}, selector, Options)).

t_create_no_bookmark(_) ->
    Limit = 99,
    Options = [{limit, Limit}, {skip, skip}, {fields, fields}, {bookmark, nil}],
    Options1 = [{limit, Limit}, {skip, skip}, {fields, fields}, {bookmark, []}],
    Cursor = #cursor{
        db = db,
        index = index,
        ranges = null,
        trace = trace,
        selector = selector,
        opts = Options1,
        limit = Limit,
        skip = skip,
        fields = fields
    },
    ?assertEqual({ok, Cursor}, create(db, {[index], trace}, selector, Options)).

t_create_invalid_bookmark(_) ->
    Options = [{bookmark, invalid}],
    Exception = {mango_error, mango_cursor_text, {invalid_bookmark, invalid}},
    meck:expect(dreyfus_bookmark, unpack, [db_name, invalid], meck:raise(error, something)),
    ?assertThrow(Exception, create(db, {[index], trace}, selector, Options)).

execute_test_() ->
    {
        foreach,
        fun() ->
            meck:new(foo, [non_strict]),
            meck:new(couch_stats),
            meck:expect(couch_db, name, [db], meck:val(db_name)),
            meck:expect(
                couch_stats,
                increment_counter,
                [
                    {[[mango, docs_examined]], meck:val(ok)},
                    {[[mango, results_returned]], meck:val(ok)},
                    {[[couch_log, level, report]], meck:val(ok)}
                ]
            ),
            meck:expect(couch_stats, update_histogram, [[mango, query_time], '_'], meck:val(ok)),
            % Dummy mock functions to progressively update the
            % respective states therefore their results could be
            % asserted later on.
            meck:expect(
                dreyfus_bookmark,
                pack,
                fun(Bookmark) ->
                    case Bookmark of
                        nil -> null;
                        [bookmark, N] -> [bookmark, N]
                    end
                end
            ),
            meck:expect(
                dreyfus_bookmark,
                update,
                fun(_Sort, [bookmark, N], [#sortable{}]) -> [bookmark, N + 1] end
            ),
            meck:expect(
                dreyfus_fabric,
                get_json_docs,
                fun(db_name, Ids) ->
                    IdDocs = lists:flatmap(
                        fun({id, N} = Id) ->
                            case N of
                                not_found -> [];
                                _ -> [{Id, {doc, {doc, N}}}]
                            end
                        end,
                        Ids
                    ),
                    {ok, IdDocs}
                end
            ),
            meck:expect(
                mango_selector_text,
                append_sort_type,
                fun(RawField, selector) -> <<RawField/binary, "<type>">> end
            ),
            meck:expect(mango_doc, get_field, fun({doc, N}, <<"_id">>) -> N end),
            meck:expect(mango_fields, extract, fun({doc, N}, fields) -> {final_doc, N} end),
            meck:expect(
                foo,
                add_key_only,
                fun({add_key, bookmark, [bookmark, _]}, {acc, N}) -> {ok, {acc, N + 1}} end
            ),
            meck:expect(
                foo,
                normal,
                fun(Args, {acc, N}) ->
                    case Args of
                        {row, {final_doc, _}} -> ok;
                        {add_key, bookmark, [bookmark, _]} -> ok
                    end,
                    {ok, {acc, N + 1}}
                end
            )
        end,
        fun(_) -> meck:unload() end,
        [
            ?TDEF_FE(t_execute_empty, 10),
            ?TDEF_FE(t_execute_no_results, 10),
            ?TDEF_FE(t_execute_more_results, 10),
            ?TDEF_FE(t_execute_unique_results, 10),
            ?TDEF_FE(t_execute_limit_cutoff, 10),
            ?TDEF_FE(t_execute_limit_cutoff_unique, 10),
            ?TDEF_FE(t_execute_limit_zero, 10),
            ?TDEF_FE(t_execute_limit_unique, 10),
            ?TDEF_FE(t_execute_skip, 10),
            ?TDEF_FE(t_execute_skip_unique, 10),
            ?TDEF_FE(t_execute_no_matches, 10),
            ?TDEF_FE(t_execute_mixed_matches, 10),
            ?TDEF_FE(t_execute_user_fun_returns_stop, 10),
            ?TDEF_FE(t_execute_search_error, 10)
        ]
    }.

t_execute_empty(_) ->
    Options = [{partition, partition}, {sort, {[]}}, {bookmark, [bookmark, 0]}],
    Cursor = #cursor{
        db = db,
        index = #idx{ddoc = <<"ddoc">>},
        limit = limit,
        skip = skip,
        fields = fields,
        selector = selector,
        opts = Options,
        execution_stats = #execution_stats{executionStartTime = {0, 0, 0}}
    },
    meck:expect(mango_selector_text, convert, [selector], meck:val(<<>>)),
    meck:expect(chttpd_stats, incr_rows, [0], meck:val(ok)),
    meck:expect(chttpd_stats, incr_reads, [0], meck:val(ok)),
    ?assertEqual({ok, {acc, 1}}, execute(Cursor, fun foo:add_key_only/2, {acc, 0})),
    ?assertEqual(0, meck:num_calls(couch_stats, increment_counter, [[mango, docs_examined]])),
    ?assertEqual(0, meck:num_calls(couch_stats, increment_counter, [[mango, results_returned]])).

t_execute_no_results(_) ->
    Limit = 10,
    Skip = 0,
    IdxDDoc = <<"ddoc">>,
    IdxName = <<"index">>,
    Sort = [<<"field1">>, <<"field2">>],
    Options = [{partition, partition}, {sort, {Sort}}, {bookmark, [bookmark, 0]}],
    Cursor = #cursor{
        db = db,
        index = #idx{ddoc = IdxDDoc, name = IdxName},
        limit = Limit,
        skip = Skip,
        fields = fields,
        selector = selector,
        opts = Options,
        execution_stats = #execution_stats{executionStartTime = {0, 0, 0}}
    },
    QueryArgs =
        #index_query_args{
            q = query,
            partition = partition,
            limit = Skip + Limit,
            bookmark = [bookmark, 0],
            sort = [<<"field1<type>">>, <<"field2<type>">>],
            raw_bookmark = true
        },
    meck:expect(
        dreyfus_fabric_search,
        go,
        [db_name, IdxDDoc, IdxName, QueryArgs],
        meck:val({ok, [bookmark, 1], undefined, [], undefined, undefined})
    ),
    meck:expect(mango_selector_text, convert, [selector], meck:val(query)),
    meck:expect(mango_selector, match, fun(selector, {doc, _}) -> true end),
    meck:expect(chttpd_stats, incr_rows, [0], meck:val(ok)),
    meck:expect(chttpd_stats, incr_reads, [0], meck:val(ok)),
    ?assertEqual({ok, {acc, 1}}, execute(Cursor, fun foo:add_key_only/2, {acc, 0})),
    ?assertEqual(0, meck:num_calls(dreyfus_fabric, get_json_docs, 2)).

t_execute_more_results(_) ->
    AllHits = 3,
    Options = [{partition, partition}, {sort, {[]}}, {bookmark, [bookmark, 0]}],
    Cursor = #cursor{
        db = db,
        index = #idx{ddoc = <<"ddoc">>, name = <<"index">>},
        limit = 10,
        skip = 0,
        fields = fields,
        selector = selector,
        opts = Options,
        execution_stats = #execution_stats{executionStartTime = {0, 0, 0}}
    },
    meck:expect(
        dreyfus_fabric_search,
        go,
        fun(db_name, <<"ddoc">>, <<"index">>, QueryArgs) ->
            #index_query_args{
                q = query,
                partition = partition,
                bookmark = [bookmark, B],
                sort = relevance,
                raw_bookmark = true
            } = QueryArgs,
            Hits =
                case B of
                    0 ->
                        Hit1 = #sortable{item = #hit{fields = [{<<"_id">>, {id, 1}}]}},
                        Hit2 = #sortable{item = #hit{fields = [{<<"_id">>, {id, not_found}}]}},
                        Hit3 = #sortable{item = #hit{fields = [{<<"_id">>, {id, not_found}}]}},
                        [Hit1, Hit2, Hit3];
                    4 ->
                        Hit4 = #sortable{item = #hit{fields = [{<<"_id">>, {id, 4}}]}},
                        Hit5 = #sortable{item = #hit{fields = [{<<"_id">>, {id, 5}}]}},
                        [Hit4, Hit5];
                    _ ->
                        []
                end,
            {ok, [bookmark, B + 1], undefined, Hits, undefined, undefined}
        end
    ),
    meck:expect(mango_selector_text, convert, [selector], meck:val(query)),
    meck:expect(mango_selector, match, fun(selector, {doc, _}) -> true end),
    meck:expect(chttpd_stats, incr_rows, [AllHits], meck:val(ok)),
    meck:expect(chttpd_stats, incr_reads, [AllHits], meck:val(ok)),
    ?assertEqual({ok, {acc, 4}}, execute(Cursor, fun foo:normal/2, {acc, 0})),
    ?assertEqual(AllHits, meck:num_calls(couch_stats, increment_counter, [[mango, docs_examined]])),
    ?assertEqual(
        AllHits, meck:num_calls(couch_stats, increment_counter, [[mango, results_returned]])
    ).

t_execute_unique_results(_) ->
    AllHits = 6,
    UniqueHits = 3,
    Options = [{partition, partition}, {sort, {[]}}, {bookmark, []}],
    Cursor = #cursor{
        db = db,
        index = #idx{ddoc = <<"ddoc">>, name = <<"index">>},
        limit = 10,
        skip = 0,
        fields = fields,
        selector = selector,
        opts = Options,
        execution_stats = #execution_stats{executionStartTime = {0, 0, 0}}
    },
    meck:expect(
        dreyfus_fabric_search,
        go,
        fun(db_name, <<"ddoc">>, <<"index">>, QueryArgs) ->
            #index_query_args{
                q = query,
                partition = partition,
                bookmark = B,
                sort = relevance,
                raw_bookmark = true
            } = QueryArgs,
            {Bookmark, Hits} =
                case B of
                    nil ->
                        Hit1 = #sortable{item = #hit{fields = [{<<"_id">>, {id, 1}}]}},
                        Hit2 = #sortable{item = #hit{fields = [{<<"_id">>, {id, 2}}]}},
                        Hit3 = #sortable{item = #hit{fields = [{<<"_id">>, {id, 3}}]}},
                        {[bookmark, 0], [Hit1, Hit2, Hit3]};
                    [bookmark, 3] ->
                        Hit1 = #sortable{item = #hit{fields = [{<<"_id">>, {id, 1}}]}},
                        Hit2 = #sortable{item = #hit{fields = [{<<"_id">>, {id, 2}}]}},
                        Hit3 = #sortable{item = #hit{fields = [{<<"_id">>, {id, 3}}]}},
                        {[bookmark, 4], [Hit3, Hit2, Hit1]};
                    [bookmark, N] ->
                        {[bookmark, N + 1], []}
                end,
            {ok, Bookmark, undefined, Hits, undefined, undefined}
        end
    ),
    meck:expect(mango_selector_text, convert, [selector], meck:val(query)),
    meck:expect(mango_selector, match, fun(selector, {doc, _}) -> true end),
    meck:expect(chttpd_stats, incr_rows, [AllHits], meck:val(ok)),
    meck:expect(chttpd_stats, incr_reads, [AllHits], meck:val(ok)),
    ?assertEqual({ok, {acc, 4}}, execute(Cursor, fun foo:normal/2, {acc, 0})),
    ?assertEqual(AllHits, meck:num_calls(couch_stats, increment_counter, [[mango, docs_examined]])),
    ?assertEqual(
        UniqueHits, meck:num_calls(couch_stats, increment_counter, [[mango, results_returned]])
    ).

t_execute_limit_cutoff(_) ->
    Limit = 2,
    Skip = 0,
    IdxName = <<"index">>,
    Sort = [{<<"field1">>, <<"desc">>}, {<<"field2">>, <<"asc">>}],
    Options = [{partition, partition}, {sort, {Sort}}, {bookmark, []}],
    Cursor = #cursor{
        db = db,
        index = #idx{ddoc = <<"_design/ddoc">>, name = IdxName},
        limit = Limit,
        skip = Skip,
        fields = fields,
        selector = selector,
        opts = Options,
        execution_stats = #execution_stats{executionStartTime = {0, 0, 0}}
    },
    QueryArgs =
        #index_query_args{
            q = query,
            partition = partition,
            limit = Skip + Limit,
            bookmark = nil,
            sort = [<<"-field1<type>">>, <<"field2<type>">>],
            raw_bookmark = true
        },
    Hit1 = #sortable{item = #hit{fields = [{<<"_id">>, {id, 1}}]}},
    Hit2 = #sortable{item = #hit{fields = [{<<"_id">>, {id, 2}}]}},
    Hit3 = #sortable{item = #hit{fields = [{<<"_id">>, {id, 3}}]}},
    Hits = [Hit1, Hit2, Hit3],
    meck:expect(
        dreyfus_fabric_search,
        go,
        [db_name, <<"ddoc">>, IdxName, QueryArgs],
        meck:val({ok, [bookmark, 1], undefined, Hits, undefined, undefined})
    ),
    meck:expect(mango_selector_text, convert, [selector], meck:val(query)),
    meck:expect(mango_selector, match, fun(selector, {doc, _}) -> true end),
    meck:expect(chttpd_stats, incr_rows, [Limit], meck:val(ok)),
    meck:expect(chttpd_stats, incr_reads, [Limit], meck:val(ok)),
    ?assertEqual({ok, {acc, 3}}, execute(Cursor, fun foo:normal/2, {acc, 0})),
    ?assertEqual(Limit, meck:num_calls(couch_stats, increment_counter, [[mango, docs_examined]])),
    ?assertEqual(
        Limit, meck:num_calls(couch_stats, increment_counter, [[mango, results_returned]])
    ).

t_execute_limit_cutoff_unique(_) ->
    Limit = 4,
    ActualHits = 3,
    AllHits = 6,
    Options = [{partition, partition}, {sort, {[]}}, {bookmark, []}],
    Cursor = #cursor{
        db = db,
        index = #idx{ddoc = <<"ddoc">>, name = <<"index">>},
        limit = Limit,
        skip = 0,
        fields = fields,
        selector = selector,
        opts = Options,
        execution_stats = #execution_stats{executionStartTime = {0, 0, 0}}
    },
    meck:expect(
        dreyfus_fabric_search,
        go,
        fun(db_name, <<"ddoc">>, <<"index">>, QueryArgs) ->
            #index_query_args{
                q = query,
                partition = partition,
                bookmark = B,
                sort = relevance,
                raw_bookmark = true
            } = QueryArgs,
            {Bookmark, Hits} =
                case B of
                    nil ->
                        Hit1 = #sortable{item = #hit{fields = [{<<"_id">>, {id, 1}}]}},
                        Hit2 = #sortable{item = #hit{fields = [{<<"_id">>, {id, 2}}]}},
                        Hit3 = #sortable{item = #hit{fields = [{<<"_id">>, {id, 3}}]}},
                        {[bookmark, 0], [Hit1, Hit2, Hit3]};
                    [bookmark, 3] ->
                        Hit1 = #sortable{item = #hit{fields = [{<<"_id">>, {id, 1}}]}},
                        Hit2 = #sortable{item = #hit{fields = [{<<"_id">>, {id, 2}}]}},
                        Hit3 = #sortable{item = #hit{fields = [{<<"_id">>, {id, 3}}]}},
                        {[bookmark, 4], [Hit3, Hit2, Hit1]};
                    [bookmark, 7] ->
                        {[bookmark, 8], []}
                end,
            {ok, Bookmark, undefined, Hits, undefined, undefined}
        end
    ),
    meck:expect(mango_selector_text, convert, [selector], meck:val(query)),
    meck:expect(mango_selector, match, fun(selector, {doc, _}) -> true end),
    meck:expect(chttpd_stats, incr_rows, [AllHits], meck:val(ok)),
    meck:expect(chttpd_stats, incr_reads, [AllHits], meck:val(ok)),
    ?assertEqual({ok, {acc, 4}}, execute(Cursor, fun foo:normal/2, {acc, 0})),
    ?assertEqual(AllHits, meck:num_calls(couch_stats, increment_counter, [[mango, docs_examined]])),
    ?assertEqual(
        ActualHits, meck:num_calls(couch_stats, increment_counter, [[mango, results_returned]])
    ).

t_execute_limit_zero(_) ->
    Limit = 0,
    Skip = 0,
    IdxName = <<"index">>,
    Options = [{partition, <<>>}, {sort, {[]}}, {bookmark, [bookmark, 0]}],
    Cursor = #cursor{
        db = db,
        index = #idx{ddoc = <<"_design/ddoc">>, name = IdxName},
        limit = Limit,
        skip = Skip,
        fields = fields,
        selector = selector,
        opts = Options,
        execution_stats = #execution_stats{executionStartTime = {0, 0, 0}}
    },
    QueryArgs =
        #index_query_args{
            q = query,
            partition = nil,
            limit = Skip + Limit,
            bookmark = [bookmark, 0],
            sort = relevance,
            raw_bookmark = true
        },
    Hit1 = #sortable{item = #hit{fields = [{<<"_id">>, {id, 1}}]}},
    Hit2 = #sortable{item = #hit{fields = [{<<"_id">>, {id, 2}}]}},
    Hit3 = #sortable{item = #hit{fields = [{<<"_id">>, {id, 3}}]}},
    Hits = [Hit1, Hit2, Hit3],
    meck:expect(
        dreyfus_fabric_search,
        go,
        [db_name, <<"ddoc">>, IdxName, QueryArgs],
        meck:val({ok, [bookmark, 1], undefined, Hits, undefined, undefined})
    ),
    meck:expect(mango_selector_text, convert, [selector], meck:val(query)),
    meck:expect(mango_selector, match, fun(selector, {doc, _}) -> true end),
    meck:expect(chttpd_stats, incr_rows, [Limit], meck:val(ok)),
    meck:expect(chttpd_stats, incr_reads, [Limit], meck:val(ok)),
    ?assertEqual({ok, {acc, 1}}, execute(Cursor, fun foo:add_key_only/2, {acc, 0})),
    ?assertEqual(1, meck:num_calls(couch_stats, increment_counter, [[mango, docs_examined]])),
    ?assertEqual(
        Limit, meck:num_calls(couch_stats, increment_counter, [[mango, results_returned]])
    ).

t_execute_limit_unique(_) ->
    Limit = 5,
    AllHits = 6,
    UniqueHits = 3,
    Options = [{partition, partition}, {sort, {[]}}, {bookmark, []}],
    Cursor = #cursor{
        db = db,
        index = #idx{ddoc = <<"ddoc">>, name = <<"index">>},
        limit = Limit,
        skip = 0,
        fields = fields,
        selector = selector,
        opts = Options,
        execution_stats = #execution_stats{executionStartTime = {0, 0, 0}}
    },
    meck:expect(
        dreyfus_fabric_search,
        go,
        fun(db_name, <<"ddoc">>, <<"index">>, QueryArgs) ->
            #index_query_args{
                q = query,
                partition = partition,
                bookmark = B,
                sort = relevance,
                raw_bookmark = true
            } = QueryArgs,
            {Bookmark, Hits} =
                case B of
                    nil ->
                        Hit1 = #sortable{item = #hit{fields = [{<<"_id">>, {id, 1}}]}},
                        Hit2 = #sortable{item = #hit{fields = [{<<"_id">>, {id, 2}}]}},
                        Hit3 = #sortable{item = #hit{fields = [{<<"_id">>, {id, 3}}]}},
                        {[bookmark, 0], [Hit1, Hit2, Hit3]};
                    [bookmark, 3] ->
                        Hit1 = #sortable{item = #hit{fields = [{<<"_id">>, {id, 1}}]}},
                        Hit2 = #sortable{item = #hit{fields = [{<<"_id">>, {id, 2}}]}},
                        Hit3 = #sortable{item = #hit{fields = [{<<"_id">>, {id, 3}}]}},
                        {[bookmark, 4], [Hit3, Hit2, Hit1]};
                    [bookmark, 7] ->
                        {[bookmark, 8], []}
                end,
            {ok, Bookmark, undefined, Hits, undefined, undefined}
        end
    ),
    meck:expect(mango_selector_text, convert, [selector], meck:val(query)),
    meck:expect(mango_selector, match, fun(selector, {doc, _}) -> true end),
    meck:expect(chttpd_stats, incr_rows, [AllHits], meck:val(ok)),
    meck:expect(chttpd_stats, incr_reads, [AllHits], meck:val(ok)),
    ?assertEqual({ok, {acc, 4}}, execute(Cursor, fun foo:normal/2, {acc, 0})),
    ?assertEqual(AllHits, meck:num_calls(couch_stats, increment_counter, [[mango, docs_examined]])),
    ?assertEqual(
        UniqueHits, meck:num_calls(couch_stats, increment_counter, [[mango, results_returned]])
    ).

t_execute_skip(_) ->
    UniqueHits = 3,
    Skip = 2,
    Options = [{partition, <<>>}, {sort, {[]}}, {bookmark, [bookmark, 0]}],
    Cursor = #cursor{
        db = db,
        index = #idx{ddoc = <<"_design/ddoc">>, name = <<"index">>},
        limit = 10,
        skip = Skip,
        fields = fields,
        selector = selector,
        opts = Options,
        execution_stats = #execution_stats{executionStartTime = {0, 0, 0}}
    },
    meck:expect(
        dreyfus_fabric_search,
        go,
        fun(db_name, <<"ddoc">>, <<"index">>, QueryArgs) ->
            #index_query_args{
                q = query,
                partition = nil,
                bookmark = [bookmark, B],
                sort = relevance,
                raw_bookmark = true
            } = QueryArgs,
            Hits =
                case B of
                    0 ->
                        Hit1 = #sortable{item = #hit{fields = [{<<"_id">>, {id, 1}}]}},
                        Hit2 = #sortable{item = #hit{fields = [{<<"_id">>, {id, 2}}]}},
                        Hit3 = #sortable{item = #hit{fields = [{<<"_id">>, {id, 3}}]}},
                        [Hit1, Hit2, Hit3];
                    _ ->
                        []
                end,
            {ok, [bookmark, B + 1], undefined, Hits, undefined, undefined}
        end
    ),
    meck:expect(mango_selector_text, convert, [selector], meck:val(query)),
    meck:expect(mango_selector, match, fun(selector, {doc, _}) -> true end),
    meck:expect(chttpd_stats, incr_rows, [UniqueHits], meck:val(ok)),
    meck:expect(chttpd_stats, incr_reads, [UniqueHits], meck:val(ok)),
    ?assertEqual({ok, {acc, 2}}, execute(Cursor, fun foo:normal/2, {acc, 0})),
    ?assertEqual(
        UniqueHits, meck:num_calls(couch_stats, increment_counter, [[mango, docs_examined]])
    ),
    ?assertEqual(
        UniqueHits - Skip,
        meck:num_calls(couch_stats, increment_counter, [[mango, results_returned]])
    ).

t_execute_skip_unique(_) ->
    AllHits = 6,
    UniqueHits = 3,
    Skip = 2,
    Options = [{partition, partition}, {sort, {[]}}, {bookmark, []}],
    Cursor = #cursor{
        db = db,
        index = #idx{ddoc = <<"ddoc">>, name = <<"index">>},
        limit = 10,
        skip = Skip,
        fields = fields,
        selector = selector,
        opts = Options,
        execution_stats = #execution_stats{executionStartTime = {0, 0, 0}}
    },
    meck:expect(
        dreyfus_fabric_search,
        go,
        fun(db_name, <<"ddoc">>, <<"index">>, QueryArgs) ->
            #index_query_args{
                q = query,
                partition = partition,
                bookmark = B,
                sort = relevance,
                raw_bookmark = true
            } = QueryArgs,
            {Bookmark, Hits} =
                case B of
                    nil ->
                        Hit1 = #sortable{item = #hit{fields = [{<<"_id">>, {id, 1}}]}},
                        Hit2 = #sortable{item = #hit{fields = [{<<"_id">>, {id, 2}}]}},
                        Hit3 = #sortable{item = #hit{fields = [{<<"_id">>, {id, 3}}]}},
                        {[bookmark, 0], [Hit1, Hit2, Hit3]};
                    [bookmark, 3] ->
                        Hit1 = #sortable{item = #hit{fields = [{<<"_id">>, {id, 1}}]}},
                        Hit2 = #sortable{item = #hit{fields = [{<<"_id">>, {id, 2}}]}},
                        Hit3 = #sortable{item = #hit{fields = [{<<"_id">>, {id, 3}}]}},
                        {[bookmark, 4], [Hit3, Hit2, Hit1]};
                    [bookmark, N] ->
                        {[bookmark, N + 1], []}
                end,
            {ok, Bookmark, undefined, Hits, undefined, undefined}
        end
    ),
    meck:expect(mango_selector_text, convert, [selector], meck:val(query)),
    meck:expect(mango_selector, match, fun(selector, {doc, _}) -> true end),
    meck:expect(chttpd_stats, incr_rows, [AllHits], meck:val(ok)),
    meck:expect(chttpd_stats, incr_reads, [AllHits], meck:val(ok)),
    ?assertEqual({ok, {acc, 2}}, execute(Cursor, fun foo:normal/2, {acc, 0})),
    ?assertEqual(AllHits, meck:num_calls(couch_stats, increment_counter, [[mango, docs_examined]])),
    ?assertEqual(
        UniqueHits - Skip,
        meck:num_calls(couch_stats, increment_counter, [[mango, results_returned]])
    ).

t_execute_no_matches(_) ->
    UniqueHits = 3,
    Matches = 0,
    Options = [{partition, partition}, {sort, {[]}}, {bookmark, [bookmark, 0]}],
    Cursor = #cursor{
        db = db,
        index = #idx{ddoc = <<"ddoc">>, name = <<"index">>},
        limit = 10,
        skip = 0,
        fields = fields,
        selector = selector,
        opts = Options,
        execution_stats = #execution_stats{executionStartTime = {0, 0, 0}}
    },
    meck:expect(
        dreyfus_fabric_search,
        go,
        fun(db_name, <<"ddoc">>, <<"index">>, QueryArgs) ->
            #index_query_args{
                q = query,
                partition = partition,
                bookmark = [bookmark, B],
                sort = relevance,
                raw_bookmark = true
            } = QueryArgs,
            Hits =
                case B of
                    0 ->
                        Hit1 = #sortable{item = #hit{fields = [{<<"_id">>, {id, 1}}]}},
                        Hit2 = #sortable{item = #hit{fields = [{<<"_id">>, {id, 2}}]}},
                        Hit3 = #sortable{item = #hit{fields = [{<<"_id">>, {id, 3}}]}},
                        [Hit1, Hit2, Hit3];
                    _ ->
                        []
                end,
            {ok, [bookmark, B + 1], undefined, Hits, undefined, undefined}
        end
    ),
    meck:expect(mango_selector_text, convert, [selector], meck:val(query)),
    meck:expect(mango_selector, match, fun(selector, {doc, _}) -> false end),
    meck:expect(chttpd_stats, incr_rows, [UniqueHits], meck:val(ok)),
    meck:expect(chttpd_stats, incr_reads, [UniqueHits], meck:val(ok)),
    ?assertEqual({ok, {acc, 1}}, execute(Cursor, fun foo:add_key_only/2, {acc, 0})),
    ?assertEqual(
        UniqueHits, meck:num_calls(couch_stats, increment_counter, [[mango, docs_examined]])
    ),
    ?assertEqual(
        Matches, meck:num_calls(couch_stats, increment_counter, [[mango, results_returned]])
    ).

t_execute_mixed_matches(_) ->
    UniqueHits = 3,
    Matches = 1,
    Options = [{partition, partition}, {sort, {[]}}, {bookmark, [bookmark, 0]}],
    Cursor = #cursor{
        db = db,
        index = #idx{ddoc = <<"ddoc">>, name = <<"index">>},
        limit = 10,
        skip = 0,
        fields = fields,
        selector = selector,
        opts = Options,
        execution_stats = #execution_stats{executionStartTime = {0, 0, 0}}
    },
    meck:expect(
        dreyfus_fabric_search,
        go,
        fun(db_name, <<"ddoc">>, <<"index">>, QueryArgs) ->
            #index_query_args{
                q = query,
                partition = partition,
                bookmark = [bookmark, B],
                sort = relevance,
                raw_bookmark = true
            } = QueryArgs,
            Hits =
                case B of
                    0 ->
                        Hit1 = #sortable{item = #hit{fields = [{<<"_id">>, {id, 1}}]}},
                        Hit2 = #sortable{item = #hit{fields = [{<<"_id">>, {id, 2}}]}},
                        Hit3 = #sortable{item = #hit{fields = [{<<"_id">>, {id, 3}}]}},
                        [Hit1, Hit2, Hit3];
                    _ ->
                        []
                end,
            {ok, [bookmark, B + 1], undefined, Hits, undefined, undefined}
        end
    ),
    meck:expect(mango_selector_text, convert, [selector], meck:val(query)),
    meck:expect(mango_selector, match, fun(selector, {doc, N}) -> N == 2 end),
    meck:expect(chttpd_stats, incr_rows, [UniqueHits], meck:val(ok)),
    meck:expect(chttpd_stats, incr_reads, [UniqueHits], meck:val(ok)),
    ?assertEqual({ok, {acc, 2}}, execute(Cursor, fun foo:normal/2, {acc, 0})),
    ?assertEqual(
        UniqueHits, meck:num_calls(couch_stats, increment_counter, [[mango, docs_examined]])
    ),
    ?assertEqual(
        Matches, meck:num_calls(couch_stats, increment_counter, [[mango, results_returned]])
    ).

t_execute_user_fun_returns_stop(_) ->
    UniqueHits = 3,
    Limit = 10,
    Skip = 0,
    IdxDDoc = <<"ddoc">>,
    IdxName = <<"index">>,
    Options = [{partition, partition}, {sort, {[]}}, {bookmark, [bookmark, 0]}],
    Cursor = #cursor{
        db = db,
        index = #idx{ddoc = IdxDDoc, name = IdxName},
        limit = Limit,
        skip = Skip,
        fields = fields,
        selector = selector,
        opts = Options,
        execution_stats = #execution_stats{executionStartTime = {0, 0, 0}}
    },
    QueryArgs =
        #index_query_args{
            q = query,
            partition = partition,
            limit = Skip + Limit,
            bookmark = [bookmark, 0],
            sort = relevance,
            raw_bookmark = true
        },
    Hit1 = #sortable{item = #hit{fields = [{<<"_id">>, {id, 1}}]}},
    Hit2 = #sortable{item = #hit{fields = [{<<"_id">>, {id, 2}}]}},
    Hit3 = #sortable{item = #hit{fields = [{<<"_id">>, {id, 3}}]}},
    Hits = [Hit1, Hit2, Hit3],
    meck:expect(
        dreyfus_fabric_search,
        go,
        [db_name, IdxDDoc, IdxName, QueryArgs],
        meck:val({ok, [bookmark, 1], undefined, Hits, undefined, undefined})
    ),
    meck:expect(mango_selector_text, convert, [selector], meck:val(query)),
    meck:expect(mango_selector, match, fun(selector, {doc, _}) -> true end),
    meck:expect(
        foo,
        stops,
        fun(Args, {acc, N}) ->
            case Args of
                {row, {final_doc, _}} -> ok;
                {add_key, bookmark, [bookmark, _]} -> ok
            end,
            Status =
                case N of
                    2 -> stop;
                    _ -> ok
                end,
            {Status, {acc, N + 1}}
        end
    ),
    meck:expect(chttpd_stats, incr_rows, [UniqueHits], meck:val(ok)),
    meck:expect(chttpd_stats, incr_reads, [UniqueHits], meck:val(ok)),
    ?assertEqual({ok, {acc, 4}}, execute(Cursor, fun foo:stops/2, {acc, 0})),
    ?assertEqual(
        UniqueHits, meck:num_calls(couch_stats, increment_counter, [[mango, docs_examined]])
    ),
    ?assertEqual(
        UniqueHits, meck:num_calls(couch_stats, increment_counter, [[mango, results_returned]])
    ).

t_execute_search_error(_) ->
    Limit = 10,
    Skip = 0,
    IdxDDoc = <<"ddoc">>,
    IdxName = <<"index">>,
    Options = [{partition, <<>>}, {sort, {[]}}, {bookmark, [bookmark, 0]}],
    Cursor = #cursor{
        db = db,
        index = #idx{ddoc = IdxDDoc, name = IdxName},
        limit = Limit,
        skip = Skip,
        fields = fields,
        selector = selector,
        opts = Options,
        execution_stats = #execution_stats{executionStartTime = {0, 0, 0}}
    },
    QueryArgs =
        #index_query_args{
            q = query,
            partition = nil,
            limit = Skip + Limit,
            bookmark = [bookmark, 0],
            sort = relevance,
            raw_bookmark = true
        },
    meck:expect(
        dreyfus_fabric_search,
        go,
        [db_name, IdxDDoc, IdxName, QueryArgs],
        meck:val({error, reason})
    ),
    meck:expect(mango_selector_text, convert, [selector], meck:val(query)),
    meck:expect(chttpd_stats, incr_rows, [0], meck:val(ok)),
    meck:expect(chttpd_stats, incr_reads, [0], meck:val(ok)),
    Exception = {mango_error, mango_cursor_text, {text_search_error, {error, reason}}},
    ?assertThrow(Exception, execute(Cursor, fun foo:normal/2, {acc, 0})),
    ?assertEqual(0, meck:num_calls(dreyfus_fabric, get_json_docs, 2)),
    ?assertEqual(0, meck:num_calls(foo, normal, 2)),
    ?assertEqual(0, meck:num_calls(couch_stats, increment_counter, [[mango, docs_examined]])),
    ?assertEqual(0, meck:num_calls(couch_stats, increment_counter, [[mango, results_returned]])).

explain_test_() ->
    {
        foreach,
        fun() -> ok end,
        fun(_) -> meck:unload() end,
        [
            ?TDEF_FE(t_explain)
        ]
    }.

t_explain(_) ->
    Options = [{partition, partition}, {sort, {[]}}],
    Cursor =
        #cursor{
            selector = selector,
            opts = Options
        },
    Response =
        [
            {query, converted_selector},
            {partition, partition},
            {sort, relevance}
        ],
    meck:expect(mango_selector_text, convert, [selector], meck:val(converted_selector)),
    ?assertEqual(Response, explain(Cursor)).

-endif.

-endif.
