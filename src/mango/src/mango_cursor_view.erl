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


create(Db, Indexes, Selector, Opts) ->
    FieldRanges = mango_idx_view:field_ranges(Selector),
    Composited = composite_indexes(Indexes, FieldRanges),
    {Index, IndexRanges, _ColPrefixLength} = choose_best_index(Db, Composited),

    Limit = couch_util:get_value(limit, Opts, mango_opts:default_limit()),
    Skip = couch_util:get_value(skip, Opts, 0),
    Fields = couch_util:get_value(fields, Opts, all_fields),

    {ok, #cursor{
        db = Db,
        index = Index,
        ranges = IndexRanges,
        selector = Selector,
        opts = Opts,
        limit = Limit,
        skip = Skip,
        fields = Fields
    }}.


explain(Cursor) ->
    #cursor{
        index = Idx,
        ranges = Ranges
    } = Cursor,
    case Ranges of
        [empty] ->
            [{range, empty}];
        _ ->
            [{range, {[
            {start_key, mango_idx:start_key(Idx, Ranges)},
            {end_key, mango_idx:end_key(Idx, Ranges)}
        ]}}]
    end.


execute(#cursor{db = Db, index = Idx} = Cursor0, UserFun, UserAcc) ->
    Cursor = Cursor0#cursor{
        user_fun = UserFun,
        user_acc = UserAcc
    },
    case Cursor#cursor.ranges of
        [empty] ->
            % empty indicates unsatisfiable ranges, so don't perform search
            {ok, UserAcc};
        _ ->
            BaseArgs = #mrargs{
                view_type = map,
                reduce = false,
                start_key = mango_idx:start_key(Idx, Cursor#cursor.ranges),
                end_key = mango_idx:end_key(Idx, Cursor#cursor.ranges),
                include_docs = true
            },
            Args = apply_opts(Cursor#cursor.opts, BaseArgs),
            {ok, LastCursor} = case mango_idx:def(Idx) of
                all_docs ->
                    CB = fun ?MODULE:handle_all_docs_message/2,
                    fabric:all_docs(Db, CB, Cursor, Args);
                _ ->
                    CB = fun ?MODULE:handle_message/2,
                    % Normal view
                    DDoc = ddocid(Idx),
                    Name = mango_idx:name(Idx),
                    fabric:query_view(Db, DDoc, Name, CB, Cursor, Args)
            end,
            {ok, LastCursor#cursor.user_acc}
    end.


% Any of these indexes may be a composite index. For each
% index find the most specific set of fields for each
% index. Ie, if an index has columns a, b, c, d, then
% check FieldRanges for a, b, c, and d and return
% the longest prefix of columns found.
composite_indexes(Indexes, FieldRanges) ->
    FieldKeys = [Key || {Key, _} <- FieldRanges],
    SortedIndexes = lists:foldl(fun(Idx, Acc) ->
        Cols = mango_idx:columns(Idx),
        Prefix = composite_prefix(Cols, FieldRanges),
        % create a ColPrefixLength based on how close the number of fields
        % the index has to the number of Prefixes the index can use
        ColPrefixLength = abs(length(Cols) - length(FieldKeys)),
        [{Idx, Prefix, ColPrefixLength} | Acc]
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


% Low and behold our query planner. Or something.
% So stupid, but we can fix this up later. First
% pass: Sort the IndexRanges by (num_columns, idx_name)
% and return the first element. Yes. Its going to
% be that dumb for now.
%
% In the future we can look into doing a cached parallel
% reduce view read on each index with the ranges to find
% the one that has the fewest number of rows or something.
choose_best_index(_DbName, IndexRanges) ->
    Cmp = fun({IdxA, PrefixA, ColPrefixLengthA}, {IdxB, PrefixB, ColPrefixLengthB}) ->
        case ColPrefixLengthA - ColPrefixLengthB of
            N when N < 0 -> true;
            N when N == 0 ->
                % We have no other way to choose, so at this point 
                % select index based on (dbname, ddocid, view_name) triple
                IdxA =< IdxB;
            _ ->
                false
        end
    end,
    hd(lists:sort(Cmp, IndexRanges)).


handle_message({meta, _}, Cursor) ->
    {ok, Cursor};
handle_message({row, Props}, Cursor) ->
    case doc_member(Cursor#cursor.db, Props, Cursor#cursor.opts) of
        {ok, Doc} ->
            case mango_selector:match(Cursor#cursor.selector, Doc) of
                true ->
                    FinalDoc = mango_fields:extract(Doc, Cursor#cursor.fields),
                    handle_doc(Cursor, FinalDoc);
                false ->
                    {ok, Cursor}
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
handle_doc(#cursor{limit = L} = C, Doc) when L > 0 ->
    UserFun = C#cursor.user_fun,
    UserAcc = C#cursor.user_acc,
    {Go, NewAcc} = UserFun({row, Doc}, UserAcc),
    {Go, C#cursor{
        user_acc = NewAcc,
        limit = L - 1
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
apply_opts([{_, _} | Rest], Args) ->
    % Ignore unknown options
    apply_opts(Rest, Args).


doc_member(Db, RowProps, Opts) ->
    case couch_util:get_value(doc, RowProps) of
        {DocProps} ->
            {ok, {DocProps}};
        undefined ->
            Id = couch_util:get_value(id, RowProps),
            case mango_util:defer(fabric, open_doc, [Db, Id, Opts]) of
                {ok, #doc{}=Doc} ->
                    {ok, couch_doc:to_json_obj(Doc, [])};
                Else ->
                    Else
            end
    end.

is_design_doc(RowProps) ->
    case couch_util:get_value(id, RowProps) of
        <<"_design/", _/binary>> -> true;
        _ -> false
    end.