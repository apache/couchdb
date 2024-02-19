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

-module(mango_cursor_nouveau).

-export([
    create/4,
    explain/1,
    execute/3
]).

-include_lib("couch/include/couch_db.hrl").
-include_lib("nouveau/include/nouveau.hrl").
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

create(Db, {Indexes, Trace}, Selector, Opts) ->
    Index =
        case Indexes of
            [Index0] ->
                Index0;
            _ ->
                ?MANGO_ERROR(multiple_nouveau_indexes)
        end,

    NouveauLimit = get_nouveau_limit(),
    Limit = erlang:min(NouveauLimit, couch_util:get_value(limit, Opts, mango_opts:default_limit())),
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
    QueryArgs = #{
        query => Query,
        partition => get_partition(Opts, null),
        sort => sort_query(Opts, Selector)
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
            JsonBM = nouveau_bookmark:pack(FinalBM),
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
        {ok, #{bookmark := Bookmark, <<"hits">> := []}} ->
            % If we don't have any results from the
            % query it means the request has paged through
            % all possible results and the request is over.
            NewCAcc = CAcc#cacc{bookmark = Bookmark},
            throw({stop, NewCAcc});
        {ok, #{bookmark := Bookmark, <<"hits">> := Hits}} ->
            NewCAcc = CAcc#cacc{bookmark = nouveau_bookmark:to_ejson(Bookmark)},
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
    case nouveau_fabric_search:go(DbName, DDocId, IdxName, QueryArgs) of
        {ok, SearchResults} ->
            {ok, SearchResults};
        {error, Reason} ->
            ?MANGO_ERROR({nouveau_search_error, {error, Reason}})
    end.

handle_hits(CAcc, []) ->
    {ok, CAcc};
handle_hits(CAcc0, [{Hit, Doc} | Rest]) ->
    CAcc1 = handle_hit(CAcc0, Hit, Doc),
    handle_hits(CAcc1, Rest).

handle_hit(CAcc0, Hit, not_found) ->
    update_bookmark(CAcc0, Hit);
handle_hit(CAcc0, Hit, Doc) ->
    #cacc{
        limit = Limit,
        skip = Skip,
        execution_stats = Stats,
        documents_seen = Seen
    } = CAcc0,
    CAcc1 = update_bookmark(CAcc0, Hit),
    Stats1 = mango_execution_stats:incr_docs_examined(Stats),
    couch_stats:increment_counter([mango, docs_examined]),
    CAcc2 = CAcc1#cacc{execution_stats = Stats1},
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
                            % We hit this case if the user specified with a
                            % zero limit. Notice that in this case we need
                            % to return the bookmark from before this match.
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

%% Convert Query to Nouveau sort specifications
%% Convert <<"Field">>, <<"desc">> to <<"-Field">>
%% and append to the nouveau query
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
            SField0 = mango_selector_text:append_sort_type(RawSortField, Selector),
            %% ugly fixup below
            SField =
                case SField0 of
                    <<Prefix:(size(SField0) - 8)/binary, "<number>">> ->
                        <<Prefix/binary, "<double>">>;
                    Else ->
                        Else
                end,
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
        [] -> null;
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

update_bookmark(CAcc, Hit) ->
    BM = CAcc#cacc.bookmark,
    DbName = CAcc#cacc.dbname,
    NewBM = nouveau_bookmark:update(DbName, BM, #{<<"hits">> => [Hit]}),
    CAcc#cacc{bookmark = NewBM}.

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
    QueryArgs#{
        bookmark => nouveau_bookmark:pack(Bookmark),
        limit => get_limit(CAcc)
    }.

get_limit(CAcc) ->
    erlang:min(get_nouveau_limit(), CAcc#cacc.limit + CAcc#cacc.skip).

get_nouveau_limit() ->
    config:get_integer("nouveau", "max_limit", 200).

get_json_docs(DbName, Hits) ->
    Ids = lists:map(
        fun(Hit) ->
            maps:get(<<"id">>, Hit)
        end,
        Hits
    ),
    % TODO: respect R query parameter (same as json indexes)
    {ok, Docs} = nouveau_fabric:get_json_docs(DbName, Ids),
    lists:zip(Hits, Docs).
