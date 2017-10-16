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
    handle_all_docs_message/2,
    composite_indexes/2,
    choose_best_index/2
]).


-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").
-include("mango_cursor.hrl").
-include("mango_idx_view.hrl").

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
    #cursor{
        opts = Opts
    } = Cursor,

    BaseArgs = base_args(Cursor),
    Args = apply_opts(Opts, BaseArgs),

    [{mrargs, {[
        {include_docs, Args#mrargs.include_docs},
        {view_type, Args#mrargs.view_type},
        {reduce, Args#mrargs.reduce},
        {start_key, Args#mrargs.start_key},
        {end_key, maybe_replace_max_json(Args#mrargs.end_key)},
        {direction, Args#mrargs.direction},
        {stable, Args#mrargs.stable},
        {update, Args#mrargs.update}
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

base_args(#cursor{index = Idx} = Cursor) ->
    #mrargs{
        view_type = map,
        reduce = false,
        start_key = mango_idx:start_key(Idx, Cursor#cursor.ranges),
        end_key = mango_idx:end_key(Idx, Cursor#cursor.ranges),
        include_docs = true
    }.


execute(#cursor{db = Db, index = Idx, execution_stats = Stats} = Cursor0, UserFun, UserAcc) ->
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
            BaseArgs = base_args(Cursor),
            #cursor{opts = Opts, bookmark = Bookmark} = Cursor,
            Args0 = apply_opts(Opts, BaseArgs),
            Args = mango_json_bookmark:update_args(Bookmark, Args0), 
            UserCtx = couch_util:get_value(user_ctx, Opts, #user_ctx{}),
            DbOpts = [{user_ctx, UserCtx}],
            Result = case mango_idx:def(Idx) of
                all_docs ->
                    CB = fun ?MODULE:handle_all_docs_message/2,
                    fabric:all_docs(Db, DbOpts, CB, Cursor, Args);
                _ ->
                    CB = fun ?MODULE:handle_message/2,
                    % Normal view
                    DDoc = ddocid(Idx),
                    Name = mango_idx:name(Idx),
                    fabric:query_view(Db, DbOpts, DDoc, Name, CB, Cursor, Args)
            end,
            case Result of
                {ok, LastCursor} ->
                    NewBookmark = mango_json_bookmark:create(LastCursor),
                    Arg = {add_key, bookmark, NewBookmark},
                    {_Go, FinalUserAcc} = UserFun(Arg, LastCursor#cursor.user_acc),
                    Stats0 = LastCursor#cursor.execution_stats,
                    FinalUserAcc0 = mango_execution_stats:maybe_add_stats(Opts, UserFun, Stats0, FinalUserAcc),
                    FinalUserAcc1 = mango_cursor:maybe_add_warning(UserFun, Idx, FinalUserAcc0),
                    {ok, FinalUserAcc1};
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
handle_message({row, Props}, Cursor) ->
    case doc_member(Cursor#cursor.db, Props, Cursor#cursor.opts, Cursor#cursor.execution_stats) of
        {ok, Doc, {execution_stats, ExecutionStats1}} ->
            Cursor1 = Cursor#cursor {
                execution_stats = ExecutionStats1
            },
            case mango_selector:match(Cursor1#cursor.selector, Doc) of
                true ->
                    Cursor2 = update_bookmark_keys(Cursor1, Props),
                    FinalDoc = mango_fields:extract(Doc, Cursor2#cursor.fields),
                    handle_doc(Cursor2, FinalDoc);
                false ->
                    {ok, Cursor1}
            end;
        Error ->
            couch_log:error("~s :: Error loading doc: ~p", [?MODULE, Error]),
            {ok, Cursor}
    end;
handle_message(complete, Cursor) ->
    {ok, Cursor};
handle_message({error, Reason}, _Cursor) ->
    {error, Reason}.


handle_all_docs_message({row, Props}, Cursor) ->
    case is_design_doc(Props) of
        true -> {ok, Cursor};
        false -> handle_message({row, Props}, Cursor)
    end;
handle_all_docs_message(Message, Cursor) ->
    handle_message(Message, Cursor).


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


ddocid(Idx) ->
    case mango_idx:ddoc(Idx) of
        <<"_design/", Rest/binary>> ->
            Rest;
        Else ->
            Else
    end.


apply_opts([], Args) ->
    Args;
apply_opts([{r, RStr} | Rest], Args) ->
    IncludeDocs = case list_to_integer(RStr) of
        1 ->
            true;
        R when R > 1 ->
            % We don't load the doc in the view query because
            % we have to do a quorum read in the coordinator
            % so there's no point.
            false
    end,
    NewArgs = Args#mrargs{include_docs = IncludeDocs},
    apply_opts(Rest, NewArgs);
apply_opts([{conflicts, true} | Rest], Args) ->
    % I need to patch things so that views can specify
    % parameters when loading the docs from disk
    apply_opts(Rest, Args);
apply_opts([{conflicts, false} | Rest], Args) ->
    % Ignored cause default
    apply_opts(Rest, Args);
apply_opts([{sort, Sort} | Rest], Args) ->
    % We only support single direction sorts
    % so nothing fancy here.
    case mango_sort:directions(Sort) of
        [] ->
            apply_opts(Rest, Args);
        [<<"asc">> | _] ->
            apply_opts(Rest, Args);
        [<<"desc">> | _] ->
            SK = Args#mrargs.start_key,
            SKDI = Args#mrargs.start_key_docid,
            EK = Args#mrargs.end_key,
            EKDI = Args#mrargs.end_key_docid,
            NewArgs = Args#mrargs{
                direction = rev,
                start_key = EK,
                start_key_docid = EKDI,
                end_key = SK,
                end_key_docid = SKDI
            },
            apply_opts(Rest, NewArgs)
    end;
apply_opts([{stale, ok} | Rest], Args) ->
    NewArgs = Args#mrargs{
        stable = true,
        update = false
    },
    apply_opts(Rest, NewArgs);
apply_opts([{stable, true} | Rest], Args) ->
    NewArgs = Args#mrargs{
        stable = true
    },
    apply_opts(Rest, NewArgs);
apply_opts([{update, false} | Rest], Args) ->
    NewArgs = Args#mrargs{
        update = false
    },
    apply_opts(Rest, NewArgs);
apply_opts([{_, _} | Rest], Args) ->
    % Ignore unknown options
    apply_opts(Rest, Args).


doc_member(Db, RowProps, Opts, ExecutionStats) ->
    case couch_util:get_value(doc, RowProps) of
        {DocProps} ->
            ExecutionStats1 = mango_execution_stats:incr_docs_examined(ExecutionStats),
            {ok, {DocProps}, {execution_stats, ExecutionStats1}};
        undefined ->
            ExecutionStats1 = mango_execution_stats:incr_quorum_docs_examined(ExecutionStats),
            Id = couch_util:get_value(id, RowProps),
            case mango_util:defer(fabric, open_doc, [Db, Id, Opts]) of
                {ok, #doc{}=Doc} ->
                    {ok, couch_doc:to_json_obj(Doc, []), {execution_stats, ExecutionStats1}};
                Else ->
                    Else
            end
    end.

is_design_doc(RowProps) ->
    case couch_util:get_value(id, RowProps) of
        <<"_design/", _/binary>> -> true;
        _ -> false
    end.


update_bookmark_keys(#cursor{limit = Limit} = Cursor, Props) when Limit > 0 ->
    Id = couch_util:get_value(id, Props), 
    Key = couch_util:get_value(key, Props), 
    Cursor#cursor {
        bookmark_docid = Id,
        bookmark_key = Key
    };
update_bookmark_keys(Cursor, _Props) ->
    Cursor.
