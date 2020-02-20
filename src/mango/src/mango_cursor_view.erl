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

-module(mango_cursor_view).

-export([
    create/4,
    explain/1,
    execute/3
]).

-export([
    handle_message/2,
    composite_indexes/2,
    choose_best_index/2
]).


-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").
-include_lib("fabric/include/fabric.hrl").

-include("mango_cursor.hrl").
-include("mango_idx_view.hrl").

-define(HEARTBEAT_INTERVAL_IN_USEC, 4000000).

create(Db, Indexes, Selector, Opts) ->
    FieldRanges = mango_idx_view:field_ranges(Selector),
    Composited = composite_indexes(Indexes, FieldRanges),
    {Index, IndexRanges} = choose_best_index(Db, Composited),

    Limit = couch_util:get_value(limit, Opts, mango_opts:default_limit()),
    Skip = couch_util:get_value(skip, Opts, 0),
    Fields = couch_util:get_value(fields, Opts, all_fields),
    Bookmark = couch_util:get_value(bookmark, Opts),

    {ok, #cursor{
        db = Db,
        index = Index,
        ranges = IndexRanges,
        selector = Selector,
        opts = Opts,
        limit = Limit,
        skip = Skip,
        fields = Fields,
        bookmark = Bookmark
    }}.


explain(Cursor) ->
    #{
        start_key := StartKey,
        end_key := EndKey,
        dir := Direction
    } = index_args(Cursor),

    [{args, {[
        {include_docs, true},
        {view_type, <<"fdb">>},
        {reduce, false},
        {partition, false},
        {start_key, maybe_replace_max_json(StartKey)},
        {end_key, maybe_replace_max_json(EndKey)},
        {direction, Direction},
        {stable, false},
        {update, true},
        {conflicts, false}
    ]}}].


% replace internal values that cannot
% be represented as a valid UTF-8 string
% with a token for JSON serialization
maybe_replace_max_json([]) ->
    [];

maybe_replace_max_json(?MAX_STR) ->
    <<"<MAX>">>;

maybe_replace_max_json([H | T] = EndKey) when is_list(EndKey) ->
    H1 = if H == ?MAX_JSON_OBJ -> <<"<MAX>">>;
            true -> H
    end,
    [H1 | maybe_replace_max_json(T)];

maybe_replace_max_json(EndKey) ->
    EndKey.

%% TODO: When supported, handle:
%% partitions
%% conflicts
index_args(#cursor{} = Cursor) ->
    #cursor{
        index = Idx,
        opts = Opts,
        bookmark = Bookmark
    } = Cursor,

    Args0 = #{
        start_key => mango_idx:start_key(Idx, Cursor#cursor.ranges),
        start_key_docid => <<>>,
        end_key => mango_idx:end_key(Idx, Cursor#cursor.ranges),
        end_key_docid => <<255>>,
        skip => 0
    },

    Sort = couch_util:get_value(sort, Opts, [<<"asc">>]),
    Args1 = case mango_sort:directions(Sort) of
        [<<"desc">> | _] ->
            #{
                start_key := SK,
                start_key_docid := SKDI,
                end_key := EK,
                end_key_docid := EKDI
            } = Args0,
            Args0#{
                dir => rev,
                start_key => EK,
                start_key_docid => EKDI,
                end_key => SK,
                end_key_docid => SKDI
            };
        _ ->
            Args0#{dir => fwd}
    end,
    mango_json_bookmark:update_args(Bookmark, Args1).


execute(#cursor{db = Db, execution_stats = Stats} = Cursor0, UserFun, UserAcc) ->
    Cursor = Cursor0#cursor{
        user_fun = UserFun,
        user_acc = UserAcc,
        execution_stats = mango_execution_stats:log_start(Stats)
    },
    case Cursor#cursor.ranges of
        [empty] ->
            % empty indicates unsatisfiable ranges, so don't perform search
            {ok, UserAcc};
        _ ->
            Args = index_args(Cursor),
            CB = fun ?MODULE:handle_message/2,
            Result = mango_fdb:query(Db, CB, Cursor, Args),
            case Result of
                {ok, LastCursor} ->
                    NewBookmark = mango_json_bookmark:create(LastCursor),
                    Arg = {add_key, bookmark, NewBookmark},
                    #cursor{
                        opts = Opts,
                        execution_stats = Stats0,
                        user_acc = FinalUserAcc0
                    } = LastCursor,
                    {_Go, FinalUserAcc1} = UserFun(Arg, FinalUserAcc0),
                    FinalUserAcc2 = mango_execution_stats:maybe_add_stats(Opts, UserFun, Stats0, FinalUserAcc1),
                    FinalUserAcc3 = mango_cursor:maybe_add_warning(UserFun, Cursor, FinalUserAcc2),
                    {ok, FinalUserAcc3};
                {error, Reason} ->
                    {error, Reason}
            end
    end.


% Any of these indexes may be a composite index. For each
% index find the most specific set of fields for each
% index. Ie, if an index has columns a, b, c, d, then
% check FieldRanges for a, b, c, and d and return
% the longest prefix of columns found.
composite_indexes(Indexes, FieldRanges) ->
    lists:foldl(fun(Idx, Acc) ->
        Cols = mango_idx:columns(Idx),
        Prefix = composite_prefix(Cols, FieldRanges),
        % Calcuate the difference between the FieldRanges/Selector
        % and the Prefix. We want to select the index with a prefix
        % that is as close to the FieldRanges as possible
        PrefixDifference = length(FieldRanges) - length(Prefix),
        [{Idx, Prefix, PrefixDifference} | Acc]
    end, [], Indexes).


composite_prefix([], _) ->
    [];
composite_prefix([Col | Rest], Ranges) ->
    case lists:keyfind(Col, 1, Ranges) of
        {Col, Range} ->
            [Range | composite_prefix(Rest, Ranges)];
        false ->
            []
    end.


% The query planner
% First choose the index with the lowest difference between its
% Prefix and the FieldRanges. If that is equal, then
% choose the index with the least number of
% fields in the index. If we still cannot break the tie,
% then choose alphabetically based on ddocId.
% Return the first element's Index and IndexRanges.
%
% In the future we can look into doing a cached parallel
% reduce view read on each index with the ranges to find
% the one that has the fewest number of rows or something.
choose_best_index(_DbName, IndexRanges) ->
    Cmp = fun({IdxA, _PrefixA, PrefixDifferenceA}, {IdxB, _PrefixB, PrefixDifferenceB}) ->
        case PrefixDifferenceA - PrefixDifferenceB of
            N when N < 0 -> true;
            N when N == 0 ->
                ColsLenA = length(mango_idx:columns(IdxA)),
                ColsLenB = length(mango_idx:columns(IdxB)),
                case ColsLenA - ColsLenB of
                    M when M < 0 ->
                        true;
                    M when M == 0 ->
                        % We have no other way to choose, so at this point
                        % select the index based on (dbname, ddocid, view_name) triple
                        IdxA =< IdxB;
                    _ ->
                        false
                end;
            _ ->
                false
        end
    end,
    {SelectedIndex, SelectedIndexRanges, _} = hd(lists:sort(Cmp, IndexRanges)),
    {SelectedIndex, SelectedIndexRanges}.


handle_message({meta, _}, Cursor) ->
    {ok, Cursor};
handle_message({doc, Key, Doc}, Cursor) ->
    case match_doc(Cursor, Doc) of
        {ok, Doc, {execution_stats, ExecutionStats1}} ->
            Cursor1 = Cursor#cursor {
                execution_stats = ExecutionStats1
            },
            {Props} = Doc,
            Cursor2 = update_bookmark_keys(Cursor1, {Key, Props}),
            FinalDoc = mango_fields:extract(Doc, Cursor2#cursor.fields),
            handle_doc(Cursor2, FinalDoc);
        {no_match, _, {execution_stats, ExecutionStats1}} ->
            Cursor1 = Cursor#cursor {
                execution_stats = ExecutionStats1
            },
            {ok, Cursor1};
        Error ->
            couch_log:error("~s :: Error loading doc: ~p", [?MODULE, Error]),
            {ok, Cursor}
    end;
handle_message(complete, Cursor) ->
    {ok, Cursor};
handle_message({error, Reason}, _Cursor) ->
    {error, Reason}.


handle_doc(#cursor{skip = S} = C, _) when S > 0 ->
    {ok, C#cursor{skip = S - 1}};
handle_doc(#cursor{limit = L, execution_stats = Stats} = C, Doc) when L > 0 ->
    UserFun = C#cursor.user_fun,
    UserAcc = C#cursor.user_acc,
    {Go, NewAcc} = UserFun({row, Doc}, UserAcc),
    {Go, C#cursor{
        user_acc = NewAcc,
        limit = L - 1,
        execution_stats = mango_execution_stats:incr_results_returned(Stats)
    }};
handle_doc(C, _Doc) ->
    {stop, C}.


match_doc(Cursor, Doc) ->
    ExecStats = Cursor#cursor.execution_stats,
    Selector = Cursor#cursor.selector,
    ExecStats1 = mango_execution_stats:incr_docs_examined(ExecStats, 1),

    case mango_selector:match(Selector, Doc) of
        true ->
            {ok, Doc, {execution_stats, ExecStats1}};
        false ->
            {no_match, Doc, {execution_stats, ExecStats1}}
    end.


update_bookmark_keys(#cursor{limit = Limit} = Cursor, {Key, Props})
        when Limit > 0 ->
    Id = couch_util:get_value(<<"_id">>, Props),
   Cursor#cursor {
        bookmark_docid = Id,
        bookmark_key = Key
    };
update_bookmark_keys(Cursor, _Props) ->
    Cursor.
