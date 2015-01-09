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
    execute/3
]).

-export([
    handle_message/2
]).


-include_lib("couch/include/couch_db.hrl").
-include("mango_cursor.hrl").


create(Db, Indexes, Selector, Opts) ->
    FieldRanges = mango_idx_view:field_ranges(Selector),
    Composited = composite_indexes(Indexes, FieldRanges),
    {Index, IndexRanges} = choose_best_index(Db, Composited),

    Limit = couch_util:get_value(limit, Opts, 10000000000),
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


execute(#cursor{db = Db, index = Idx} = Cursor0, UserFun, UserAcc) ->
    Cursor = Cursor0#cursor{
        user_fun = UserFun,
        user_acc = UserAcc
    },
    BaseArgs = #view_query_args{
        view_type = red_map,
        start_key = mango_idx:start_key(Idx, Cursor#cursor.ranges),
        end_key = mango_idx:end_key(Idx, Cursor#cursor.ranges),
        include_docs = true
    },
    Args = apply_opts(Cursor#cursor.opts, BaseArgs),
    CB = fun ?MODULE:handle_message/2,
    {ok, LastCursor} = case mango_idx:def(Idx) of
        all_docs ->
            %twig:log(err, "Query: ~s all_docs~n  ~p", [Db#db.name, Args]),
            fabric:all_docs(Db, CB, Cursor, Args);
        _ ->
            % Normal view
            DDoc = ddocid(Idx),
            Name = mango_idx:name(Idx),
            %twig:log(err, "Query: ~s ~s ~s~n  ~p", [Db#db.name, DDoc, Name, Args]),
            fabric:query_view(Db, DDoc, Name, CB, Cursor, Args)
    end,
    {ok, LastCursor#cursor.user_acc}.


% Any of these indexes may be a composite index. For each
% index find the most specific set of fields for each
% index. Ie, if an index has columns a, b, c, d, then
% check FieldRanges for a, b, c, and d and return
% the longest prefix of columns found.
composite_indexes(Indexes, FieldRanges) ->
    lists:foldl(fun(Idx, Acc) ->
        Cols = mango_idx:columns(Idx),
        Prefix = composite_prefix(Cols, FieldRanges),
        [{Idx, Prefix} | Acc]
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
    Cmp = fun({A1, A2}, {B1, B2}) ->
        case length(A2) - length(B2) of
            N when N < 0 -> true;
            N when N == 0 ->
                % This is a really bad sort and will end
                % up preferring indices based on the
                % (dbname, ddocid, view_name) triple
                A1 =< B1;
            _ ->
                false
        end
    end,
    hd(lists:sort(Cmp, IndexRanges)).


handle_message({total_and_offset, _, _} = _TO, Cursor) ->
    %twig:log(err, "TOTAL AND OFFSET: ~p", [_TO]),
    {ok, Cursor};
handle_message({row, {Props}}, Cursor) ->
    %twig:log(err, "ROW: ~p", [Props]),
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
            twig:log(err, "~s :: Error loading doc: ~p", [?MODULE, Error]),
            {ok, Cursor}
    end;
handle_message(complete, Cursor) ->
    %twig:log(err, "COMPLETE", []),
    {ok, Cursor};
handle_message({error, Reason}, _Cursor) ->
    %twig:log(err, "ERROR: ~p", [Reason]),
    {error, Reason}.


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
    NewArgs = Args#view_query_args{include_docs = IncludeDocs},
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
            SK = Args#view_query_args.start_key,
            SKDI = Args#view_query_args.start_docid,
            EK = Args#view_query_args.end_key,
            EKDI = Args#view_query_args.end_docid,
            NewArgs = Args#view_query_args{
                direction = rev,
                start_key = EK,
                start_docid = EKDI,
                end_key = SK,
                end_docid = SKDI
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
