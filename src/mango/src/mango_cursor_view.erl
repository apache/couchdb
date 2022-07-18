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
    view_cb/2,
    handle_message/2,
    handle_all_docs_message/2,
    composite_indexes/2,
    choose_best_index/1
]).

-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").
-include_lib("fabric/include/fabric.hrl").

-include("mango.hrl").
-include("mango_cursor.hrl").
-include("mango_idx_view.hrl").

-define(HEARTBEAT_INTERVAL_IN_USEC, 4000000).

-type message() ::
    {meta, _}
    | {row, row_properties()}
    | {execution_stats, shard_stats()}
    | {stop, #cursor{}}
    | {complete, #cursor{}}
    | {error, any()}.

% viewcbargs wraps up the arguments that view_cb uses into a single
% entry in the mrargs.extra list. We use a Map to allow us to later
% add fields without having old messages causing errors/crashes.

-type viewcbargs() ::
    #{
        selector => selector(),
        fields => fields(),
        covering_index => maybe(#idx{})
    }.

-spec viewcbargs_new(Selector, Fields, CoveringIndex) -> ViewCBArgs when
    Selector :: selector(),
    Fields :: fields(),
    CoveringIndex :: maybe(#idx{}),
    ViewCBArgs :: viewcbargs().
viewcbargs_new(Selector, Fields, CoveringIndex) ->
    #{
        selector => Selector,
        fields => Fields,
        covering_index => CoveringIndex
    }.

-spec viewcbargs_get(Key, Args) -> maybe(term()) when
    Key :: selector | fields | covering_index,
    Args :: viewcbargs().
viewcbargs_get(selector, Args) when is_map(Args) ->
    maps:get(selector, Args, undefined);
viewcbargs_get(fields, Args) when is_map(Args) ->
    maps:get(fields, Args, undefined);
viewcbargs_get(covering_index, Args) when is_map(Args) ->
    maps:get(covering_index, Args, undefined).

-spec shard_stats_get(Key, Args) -> Stat when
    Key :: docs_examined | keys_examined,
    Args :: shard_stats_v2(),
    Stat :: non_neg_integer().
shard_stats_get(docs_examined, Args) when is_map(Args) ->
    maps:get(docs_examined, Args, 0);
shard_stats_get(keys_examined, Args) when is_map(Args) ->
    maps:get(keys_examined, Args, 0).

-spec create(Db, {Indexes, Trace}, Selector, Options) -> {ok, #cursor{}} when
    Db :: database(),
    Indexes :: [#idx{}],
    Trace :: trace(),
    Selector :: selector(),
    Options :: cursor_options().
create(Db, {Indexes, Trace0}, Selector, Opts) ->
    FieldRanges = mango_idx_view:field_ranges(Selector),
    Composited = composite_indexes(Indexes, FieldRanges),
    {{Index, IndexRanges}, SortedIndexRanges} = choose_best_index(Composited),

    Limit = couch_util:get_value(limit, Opts, mango_opts:default_limit()),
    Skip = couch_util:get_value(skip, Opts, 0),
    Fields = couch_util:get_value(fields, Opts, all_fields),
    Bookmark = couch_util:get_value(bookmark, Opts),

    IndexRanges1 = mango_cursor:maybe_noop_range(Selector, IndexRanges),
    Trace = maps:merge(Trace0, #{sorted_index_ranges => SortedIndexRanges}),

    {ok, #cursor{
        db = Db,
        index = Index,
        ranges = IndexRanges1,
        trace = Trace,
        selector = Selector,
        opts = Opts,
        limit = Limit,
        skip = Skip,
        fields = Fields,
        bookmark = Bookmark
    }}.

-spec required_fields(#cursor{}) -> fields().
required_fields(#cursor{fields = all_fields}) ->
    all_fields;
required_fields(#cursor{fields = Fields, selector = Selector}) ->
    lists:usort(Fields ++ mango_selector:fields(Selector)).

-spec apply_cursor_opts(#cursor{}) -> {#mrargs{}, boolean()}.
apply_cursor_opts(#cursor{} = Cursor) ->
    #cursor{index = Index, opts = Opts} = Cursor,
    BaseArgs = base_args(Cursor),
    Args0 = apply_opts(Opts, BaseArgs),
    Fields = required_fields(Cursor),
    Args = consider_index_coverage(Index, Fields, Args0),
    Covering = mango_idx_view:covers(Index, Fields),
    {Args, Covering}.

-spec explain(#cursor{}) -> nonempty_list(term()).
explain(Cursor) ->
    {Args, Covering} = apply_cursor_opts(Cursor),
    [
        {mrargs,
            {[
                {include_docs, Args#mrargs.include_docs},
                {view_type, Args#mrargs.view_type},
                {reduce, Args#mrargs.reduce},
                {partition, couch_mrview_util:get_extra(Args, partition, null)},
                {start_key, maybe_replace_max_json(Args#mrargs.start_key)},
                {end_key, maybe_replace_max_json(Args#mrargs.end_key)},
                {direction, Args#mrargs.direction},
                {stable, Args#mrargs.stable},
                {update, Args#mrargs.update},
                {conflicts, Args#mrargs.conflicts}
            ]}},
        {covering, Covering}
    ].

% replace internal values that cannot
% be represented as a valid UTF-8 string
% with a token for JSON serialization
maybe_replace_max_json([]) ->
    [];
maybe_replace_max_json(?MAX_STR) ->
    <<"<MAX>">>;
maybe_replace_max_json([H | T] = EndKey) when is_list(EndKey) ->
    H1 =
        if
            H == ?MAX_JSON_OBJ -> <<"<MAX>">>;
            true -> H
        end,
    [H1 | maybe_replace_max_json(T)];
maybe_replace_max_json(EndKey) ->
    EndKey.

-spec base_args(#cursor{}) -> #mrargs{}.
base_args(#cursor{index = Idx, selector = Selector, fields = Fields} = Cursor) ->
    {StartKey, EndKey} =
        case Cursor#cursor.ranges of
            [empty] ->
                {null, null};
            _ ->
                {
                    mango_idx:start_key(Idx, Cursor#cursor.ranges),
                    mango_idx:end_key(Idx, Cursor#cursor.ranges)
                }
        end,
    #mrargs{
        view_type = map,
        reduce = false,
        start_key = StartKey,
        end_key = EndKey,
        include_docs = true,
        extra = [
            % view_cb pushes down post hoc matching and field extraction to
            % the shard.
            {callback, {?MODULE, view_cb}},
            % TODO remove selector. It supports older nodes during version upgrades.
            {selector, Selector},
            {callback_args, viewcbargs_new(Selector, Fields, undefined)},

            {ignore_partition_query_limit, true},

            % Request execution statistics in a map.  The purpose of this option is
            % to maintain interoperability on version upgrades.
            % TODO remove this option in a later version.
            {execution_stats_map, true}
        ]
    }.

-spec execute(#cursor{}, UserFunction, UserAccumulator) -> Result when
    UserFunction :: fun(),
    UserAccumulator :: any(),
    Result :: {ok, UserAccumulator} | {error, any()}.
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
            {Args0, _Covered} = apply_cursor_opts(Cursor),
            #cursor{opts = Opts, bookmark = Bookmark} = Cursor,
            Args = mango_json_bookmark:update_args(Bookmark, Args0),
            UserCtx = couch_util:get_value(user_ctx, Opts, #user_ctx{}),
            DbOpts = [{user_ctx, UserCtx}],
            Result =
                case mango_idx:def(Idx) of
                    all_docs ->
                        couch_stats:increment_counter([mango_cursor, view, all_docs]),
                        CB = fun ?MODULE:handle_all_docs_message/2,
                        fabric:all_docs(Db, DbOpts, CB, Cursor, Args);
                    _ ->
                        couch_stats:increment_counter([mango_cursor, view, idx]),
                        CB = fun ?MODULE:handle_message/2,
                        % Normal view
                        DDoc = ddocid(Idx),
                        Name = mango_idx:name(Idx),
                        fabric:query_view(Db, DbOpts, DDoc, Name, CB, Cursor, Args)
                end,
            case Result of
                {ok, #cursor{} = LastCursor} ->
                    NewBookmark = mango_json_bookmark:create(LastCursor),
                    Arg = {add_key, bookmark, NewBookmark},
                    {_Go, FinalUserAcc} = UserFun(Arg, LastCursor#cursor.user_acc),
                    Stats0 = LastCursor#cursor.execution_stats,
                    {FinalUserAcc0, Stats1} = mango_execution_stats:maybe_add_stats(
                        Opts, UserFun, Stats0, FinalUserAcc
                    ),
                    %% This needs Stats1 as log_end is called in maybe_add_stats
                    mango_execution_stats:log_stats(Stats1),
                    FinalUserAcc1 = mango_cursor:maybe_add_warning(
                        UserFun, Cursor, Stats1, FinalUserAcc0
                    ),
                    {ok, FinalUserAcc1};
                {ok, Error} when is_tuple(Error) ->
                    % fabric_view_all_docs turns {error, Resp} results into {ok, Resp}
                    % for some reason. If we didn't get a proper cursor record, assume
                    % it's an error and pass it through
                    {error, Error};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

% Any of these indexes may be a composite index. For each
% index find the most specific set of fields for each
% index. Ie, if an index has columns a, b, c, d, then
% check FieldRanges for a, b, c, and d and return
% the longest prefix of columns found.
-spec composite_indexes([#idx{}], [{field(), range()}]) -> [{#idx{}, [range()], integer()}].
composite_indexes(Indexes, FieldRanges) ->
    lists:map(
        fun(Idx) ->
            Cols = mango_idx:columns(Idx),
            Prefix = composite_prefix(Cols, FieldRanges),
            % Calculate the difference between the FieldRanges/Selector
            % and the Prefix. We want to select the index with a prefix
            % that is as close to the FieldRanges as possible
            PrefixDifference = length(FieldRanges) - length(Prefix),
            {Idx, Prefix, PrefixDifference}
        end,
        Indexes
    ).

-spec composite_prefix([field()], [{field(), range()}]) -> [range()].
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
% then choose alphabetically based on (dbname, ddocid, view_name).
% Return the first element's Index and IndexRanges.
%
% In the future we can look into doing a cached parallel
% reduce view read on each index with the ranges to find
% the one that has the fewest number of rows or something.
-spec choose_best_index(IndexRanges) -> {Selection, IndexRanges} when
    IndexRanges :: nonempty_list({#idx{}, [range()], integer()}),
    Selection :: {#idx{}, [range()]}.
choose_best_index(IndexRanges) ->
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
                        % Restrict the comparison to the (dbname, ddocid, view_name)
                        % triple -- in case of their equivalence, the original order
                        % will be maintained.
                        #idx{dbname = DbNameA, ddoc = DDocA, name = NameA} = IdxA,
                        #idx{dbname = DbNameB, ddoc = DDocB, name = NameB} = IdxB,
                        {DbNameA, DDocA, NameA} =< {DbNameB, DDocB, NameB};
                    _ ->
                        false
                end;
            _ ->
                false
        end
    end,
    SortedIndexRanges = lists:sort(Cmp, IndexRanges),
    {SelectedIndex, SelectedIndexRanges, _} = hd(SortedIndexRanges),
    {{SelectedIndex, SelectedIndexRanges}, SortedIndexRanges}.

-spec view_cb
    (Message, #mrargs{}) -> Response when
        Message :: {meta, any()} | {row, row_properties()} | complete,
        Response :: {ok, #mrargs{}};
    (ok, ddoc_updated) -> any().
view_cb({meta, Meta}, Acc) ->
    % Map function starting
    mango_execution_stats:shard_init(),
    set_mango_msg_timestamp(),
    ok = rexi:stream2({meta, Meta}),
    {ok, Acc};
view_cb({row, Row}, #mrargs{extra = Options} = Acc) ->
    mango_execution_stats:shard_incr_keys_examined(),
    couch_stats:increment_counter([mango, keys_examined]),
    ViewRow = #view_row{
        id = couch_util:get_value(id, Row),
        key = couch_util:get_value(key, Row),
        doc = couch_util:get_value(doc, Row)
    },
    % This supports receiving our "arguments" either as just the `selector`
    % or in the new record in `callback_args`. This is to support mid-upgrade
    % clusters where the non-upgraded coordinator nodes will send the older style.
    % TODO remove this in a couple of couchdb versions.
    {Selector, Fields, CoveringIndex} =
        case couch_util:get_value(callback_args, Options) of
            % old style
            undefined ->
                {couch_util:get_value(selector, Options), undefined, undefined};
            % new style - assume a viewcbargs
            Args = #{} ->
                {
                    viewcbargs_get(selector, Args),
                    viewcbargs_get(fields, Args),
                    viewcbargs_get(covering_index, Args)
                }
        end,
    Process =
        fun(Doc) ->
            % slightly abuse the doc field in the view response here,
            % because we may return something other than the full document:
            % we may have projected the requested `fields` from the query.
            % However, this oddness is confined to being visible in this module.
            case match_and_extract_doc(Doc, Selector, Fields) of
                {match, FinalDoc} ->
                    FinalViewRow = ViewRow#view_row{doc = FinalDoc},
                    ok = rexi:stream2(FinalViewRow),
                    set_mango_msg_timestamp();
                {no_match, undefined} ->
                    maybe_send_mango_ping()
            end
        end,
    case {ViewRow#view_row.doc, CoveringIndex} of
        {null, _} ->
            maybe_send_mango_ping();
        {undefined, Index = #idx{}} ->
            Doc = derive_doc_from_index(Index, ViewRow),
            Process(Doc);
        {undefined, _} ->
            % include_docs=false. Use quorum fetch at coordinator
            ok = rexi:stream2(ViewRow),
            set_mango_msg_timestamp();
        {Doc, _} ->
            mango_execution_stats:shard_incr_docs_examined(),
            couch_stats:increment_counter([mango, docs_examined]),
            Process(Doc)
    end,
    {ok, Acc};
view_cb(complete, #mrargs{extra = Options} = Acc) ->
    ShardStats = mango_execution_stats:shard_get_stats(),
    Stats =
        case couch_util:get_value(execution_stats_map, Options, false) of
            true ->
                ShardStats;
            false ->
                DocsExamined = maps:get(docs_examined, ShardStats),
                {docs_examined, DocsExamined}
        end,
    % Send shard-level execution stats
    ok = rexi:stream2({execution_stats, Stats}),
    % Finish view output
    ok = rexi:stream_last(complete),
    {ok, Acc};
view_cb(ok, ddoc_updated) ->
    rexi:reply({ok, ddoc_updated}).

%% match_and_extract_doc checks whether Doc matches Selector. If it does,
%% extract Fields and return {match, FinalDoc}; otherwise return {no_match, undefined}.
-spec match_and_extract_doc(Doc, Selector, Fields) -> Result when
    Doc :: ejson(),
    Selector :: selector(),
    Fields :: maybe(fields()),
    Result :: {match, term()} | {no_match, undefined}.
match_and_extract_doc(Doc, Selector, Fields) ->
    case mango_selector:match(Selector, Doc) of
        true ->
            FinalDoc = mango_fields:extract(Doc, Fields),
            {match, FinalDoc};
        false ->
            {no_match, undefined}
    end.

-spec derive_doc_from_index(#idx{}, #view_row{}) -> term().
derive_doc_from_index(Index, #view_row{id = DocId, key = KeyData}) ->
    Keys =
        case KeyData of
            {p, _Partition, KeyValues} -> KeyValues;
            KeyValues -> KeyValues
        end,
    Columns = mango_idx:columns(Index),
    lists:foldr(
        fun({Column, Key}, Doc) -> mango_doc:set_field(Doc, Column, Key) end,
        mango_doc:set_field({[]}, <<"_id">>, DocId),
        lists:zip(Columns, Keys)
    ).

-spec maybe_send_mango_ping() -> ok | term().
maybe_send_mango_ping() ->
    Current = os:timestamp(),
    LastPing = get(mango_last_msg_timestamp),
    % Fabric will timeout if it has not heard a response from a worker node
    % after 5 seconds. Send a ping every 4 seconds so the timeout doesn't happen.
    case timer:now_diff(Current, LastPing) > ?HEARTBEAT_INTERVAL_IN_USEC of
        false ->
            ok;
        true ->
            rexi:ping(),
            set_mango_msg_timestamp()
    end.

-spec set_mango_msg_timestamp() -> term().
set_mango_msg_timestamp() ->
    put(mango_last_msg_timestamp, os:timestamp()).

-spec handle_message(message(), #cursor{}) -> Response when
    Response ::
        {ok, #cursor{}}
        | {error, any()}.
handle_message({meta, _}, Cursor) ->
    {ok, Cursor};
handle_message({row, Props}, Cursor) ->
    case doc_member_and_extract(Cursor, Props) of
        {ok, Doc, {execution_stats, Stats}} ->
            Cursor1 = Cursor#cursor{
                execution_stats = Stats
            },
            Cursor2 = update_bookmark_keys(Cursor1, Props),
            handle_doc(Cursor2, Doc);
        {no_match, _, {execution_stats, Stats}} ->
            Cursor1 = Cursor#cursor{
                execution_stats = Stats
            },
            {ok, Cursor1};
        Error ->
            couch_log:error("~s :: Error loading doc: ~p", [?MODULE, Error]),
            {ok, Cursor}
    end;
handle_message({execution_stats, {docs_examined, DocsExamined}}, Cursor0) ->
    #cursor{execution_stats = Stats} = Cursor0,
    Cursor = Cursor0#cursor{
        execution_stats = mango_execution_stats:incr_docs_examined(Stats, DocsExamined)
    },
    {ok, Cursor};
handle_message({execution_stats, #{} = ShardStats}, Cursor0) ->
    DocsExamined = shard_stats_get(docs_examined, ShardStats),
    KeysExamined = shard_stats_get(keys_examined, ShardStats),
    #cursor{execution_stats = Stats0} = Cursor0,
    Stats1 = mango_execution_stats:incr_docs_examined(Stats0, DocsExamined),
    Stats = mango_execution_stats:incr_keys_examined(Stats1, KeysExamined),
    Cursor = Cursor0#cursor{execution_stats = Stats},
    {ok, Cursor};
handle_message(complete, Cursor) ->
    {ok, Cursor};
handle_message({error, Reason}, _Cursor) ->
    {error, Reason}.

-spec handle_all_docs_message(message(), #cursor{}) -> Response when
    Response ::
        {ok, #cursor{}}
        | {error, any()}.
handle_all_docs_message({row, Props}, Cursor) ->
    case is_design_doc(Props) of
        true -> {ok, Cursor};
        false -> handle_message({row, Props}, Cursor)
    end;
handle_all_docs_message(Message, Cursor) ->
    handle_message(Message, Cursor).

-spec handle_doc(#cursor{}, doc()) -> Response when
    Response :: {ok, #cursor{}} | {stop, #cursor{}}.
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

-spec ddocid(#idx{}) -> binary().
ddocid(Idx) ->
    case mango_idx:ddoc(Idx) of
        <<"_design/", Rest/binary>> ->
            Rest;
        Else ->
            Else
    end.

-spec apply_opts(cursor_options(), #mrargs{}) -> #mrargs{}.
apply_opts([], Args) ->
    Args;
apply_opts([{r, RStr} | Rest], Args) ->
    IncludeDocs =
        case list_to_integer(RStr) of
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
    NewArgs = Args#mrargs{conflicts = true},
    apply_opts(Rest, NewArgs);
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
apply_opts([{partition, <<>>} | Rest], Args) ->
    apply_opts(Rest, Args);
apply_opts([{partition, Partition} | Rest], Args) when is_binary(Partition) ->
    NewArgs = couch_mrview_util:set_extra(Args, partition, Partition),
    apply_opts(Rest, NewArgs);
apply_opts([{_, _} | Rest], Args) ->
    % Ignore unknown options
    apply_opts(Rest, Args).

-spec consider_index_coverage(#idx{}, fields(), #mrargs{}) -> #mrargs{}.
consider_index_coverage(Index, Fields, #mrargs{include_docs = IncludeDocs0} = Args0) ->
    Covering = mango_idx_view:covers(Index, Fields),
    Args = Args0#mrargs{include_docs = IncludeDocs0 andalso (not Covering)},
    #mrargs{include_docs = IncludeDocs, extra = Extra0} = Args,
    case
        {
            IncludeDocs,
            Covering,
            couch_util:get_value(callback_args, Extra0)
        }
    of
        {false, true, ViewCBArgs0} when ViewCBArgs0 =/= undefined ->
            VCBSelector = viewcbargs_get(selector, ViewCBArgs0),
            VCBFields = viewcbargs_get(fields, ViewCBArgs0),
            ViewCBArgs = viewcbargs_new(VCBSelector, VCBFields, Index),
            Extra = couch_util:set_value(callback_args, Extra0, ViewCBArgs),
            Args#mrargs{extra = Extra};
        _ ->
            Args
    end.

-spec doc_member_and_extract(#cursor{}, row_properties()) -> Result when
    Result ::
        {ok | no_match, term(), {execution_stats, shard_stats()}}
        | {no_match, null, {execution_stats, shard_stats()}}
        | any().
doc_member_and_extract(Cursor, RowProps) ->
    Db = Cursor#cursor.db,
    Opts = Cursor#cursor.opts,
    ExecutionStats = Cursor#cursor.execution_stats,
    Selector = Cursor#cursor.selector,
    case couch_util:get_value(doc, RowProps) of
        {DocProps} ->
            % If the query doesn't request quorum doc read via r>1,
            % match_and_extract_doc/3 is executed in view_cb, ie, locally
            % on the shard. We only receive back the final result for the query.
            % TODO during upgrade, some nodes will not be processing `fields`
            % on the shard because they're old, so re-execute here just in case.
            % Remove this later, same time as the duplicate extract at the coordinator.
            DocProps2 = mango_fields:extract({DocProps}, Cursor#cursor.fields),
            {ok, DocProps2, {execution_stats, ExecutionStats}};
        undefined ->
            % an undefined doc was returned, indicating we should
            % perform a quorum fetch
            ExecutionStats1 = mango_execution_stats:incr_quorum_docs_examined(ExecutionStats),
            couch_stats:increment_counter([mango, quorum_docs_examined]),
            Id = couch_util:get_value(id, RowProps),
            case mango_util:defer(fabric, open_doc, [Db, Id, Opts]) of
                {ok, #doc{} = DocProps} ->
                    Doc = couch_doc:to_json_obj(DocProps, []),
                    case match_and_extract_doc(Doc, Selector, Cursor#cursor.fields) of
                        {match, FinalDoc} ->
                            {ok, FinalDoc, {execution_stats, ExecutionStats1}};
                        {no_match, undefined} ->
                            {no_match, Doc, {execution_stats, ExecutionStats1}}
                    end;
                Else ->
                    Else
            end;
        _ ->
            % no doc, no match
            {no_match, null, {execution_stats, ExecutionStats}}
    end.

-spec is_design_doc(row_properties()) -> boolean().
is_design_doc(RowProps) ->
    case couch_util:get_value(id, RowProps) of
        <<"_design/", _/binary>> -> true;
        _ -> false
    end.

-spec update_bookmark_keys(#cursor{}, row_properties()) -> #cursor{}.
update_bookmark_keys(#cursor{limit = Limit} = Cursor, Props) when Limit > 0 ->
    Id = couch_util:get_value(id, Props),
    Key = couch_util:get_value(key, Props),
    Cursor#cursor{
        bookmark_docid = Id,
        bookmark_key = Key
    };
update_bookmark_keys(Cursor, _Props) ->
    Cursor.

%%%%%%%% module tests below %%%%%%%%

-ifdef(TEST).
-include_lib("couch/include/couch_eunit.hrl").

viewcbargs_test() ->
    ViewCBArgs = viewcbargs_new(selector, fields, index),
    ?assertEqual(selector, viewcbargs_get(selector, ViewCBArgs)),
    ?assertEqual(fields, viewcbargs_get(fields, ViewCBArgs)),
    ?assertEqual(index, viewcbargs_get(covering_index, ViewCBArgs)),
    ?assertError(function_clause, viewcbargs_get(something_else, ViewCBArgs)).

maybe_replace_max_json_test() ->
    ?assertEqual([], maybe_replace_max_json([])),
    ?assertEqual(<<"<MAX>">>, maybe_replace_max_json(?MAX_STR)),
    ?assertEqual(
        [val1, val2, <<"<MAX>">>, val3], maybe_replace_max_json([val1, val2, ?MAX_JSON_OBJ, val3])
    ),
    ?assertEqual(something, maybe_replace_max_json(something)).

base_opts_test() ->
    Index =
        #idx{
            type = <<"json">>,
            def = {[{<<"fields">>, {[{field1, undefined}, {field2, undefined}]}}]}
        },
    Fields = [field1, field2],
    Cursor =
        #cursor{
            index = Index,
            selector = selector,
            fields = Fields,
            ranges = [{'$gte', start_key, '$lte', end_key}]
        },
    Extra =
        [
            {callback, {mango_cursor_view, view_cb}},
            {selector, selector},
            {callback_args, #{
                selector => selector,
                fields => Fields,
                covering_index => undefined
            }},
            {ignore_partition_query_limit, true},
            {execution_stats_map, true}
        ],
    MRArgs =
        #mrargs{
            view_type = map,
            reduce = false,
            start_key = [start_key],
            end_key = [end_key, ?MAX_JSON_OBJ],
            include_docs = true,
            extra = Extra
        },
    ?assertEqual(MRArgs, base_args(Cursor)).

apply_opts_empty_test() ->
    ?assertEqual(args, apply_opts([], args)).

apply_opts_r_test() ->
    Args = #mrargs{},
    ArgsWithDocs = Args#mrargs{include_docs = true},
    ?assertEqual(ArgsWithDocs, apply_opts([{r, "1"}], Args)),
    ArgsWithoutDocs = Args#mrargs{include_docs = false},
    ?assertEqual(ArgsWithoutDocs, apply_opts([{r, "3"}], Args)).

apply_opts_conflicts_test() ->
    Args = #mrargs{},
    ArgsWithConflicts = Args#mrargs{conflicts = true},
    ?assertEqual(ArgsWithConflicts, apply_opts([{conflicts, true}], Args)),
    ArgsWithoutConflicts = Args#mrargs{conflicts = undefined},
    ?assertEqual(ArgsWithoutConflicts, apply_opts([{conflicts, false}], Args)).

apply_opts_sort_test() ->
    Args =
        #mrargs{
            start_key = start_key,
            start_key_docid = start_key_docid,
            end_key = end_key,
            end_key_docid = end_key_docid
        },
    ?assertEqual(Args, apply_opts([{sort, {[]}}], Args)),
    ?assertEqual(Args, apply_opts([{sort, {[{field1, <<"asc">>}]}}], Args)),
    ?assertEqual(Args, apply_opts([{sort, {[{field1, <<"asc">>}, {field2, <<"desc">>}]}}], Args)),
    ArgsWithSort =
        Args#mrargs{
            direction = rev,
            start_key = end_key,
            start_key_docid = end_key_docid,
            end_key = start_key,
            end_key_docid = start_key_docid
        },
    ?assertEqual(ArgsWithSort, apply_opts([{sort, {[{field1, <<"desc">>}]}}], Args)).

apply_opts_stale_test() ->
    Args = #mrargs{},
    ArgsWithStale = Args#mrargs{stable = true, update = false},
    ?assertEqual(ArgsWithStale, apply_opts([{stale, ok}], Args)).

apply_opts_stable_test() ->
    Args = #mrargs{},
    ArgsWithStable = Args#mrargs{stable = true},
    ?assertEqual(ArgsWithStable, apply_opts([{stable, true}], Args)),
    ArgsWithoutStable = Args#mrargs{stable = false},
    ?assertEqual(ArgsWithoutStable, apply_opts([{stable, false}], Args)).

apply_opts_update_test() ->
    Args = #mrargs{},
    ArgsWithUpdate = Args#mrargs{update = true},
    ?assertEqual(ArgsWithUpdate, apply_opts([{update, true}], Args)),
    ArgsWithoutUpdate = Args#mrargs{update = false},
    ?assertEqual(ArgsWithoutUpdate, apply_opts([{update, false}], Args)).

apply_opts_partition_test() ->
    Args = #mrargs{},
    ArgsWithPartition = Args#mrargs{extra = [{partition, <<"partition">>}]},
    ?assertEqual(ArgsWithPartition, apply_opts([{partition, <<"partition">>}], Args)),
    ArgsWithoutPartition = Args#mrargs{extra = []},
    ?assertEqual(ArgsWithoutPartition, apply_opts([{partition, <<>>}], Args)).

consider_index_coverage_positive_test() ->
    Index =
        #idx{
            type = <<"json">>,
            def = {[{<<"fields">>, {[]}}]}
        },
    Fields = [<<"_id">>],
    MRArgs =
        #mrargs{
            include_docs = true,
            extra = [{callback_args, viewcbargs_new(selector, fields, undefined)}]
        },
    MRArgsRef =
        MRArgs#mrargs{
            include_docs = false,
            extra = [{callback_args, viewcbargs_new(selector, fields, Index)}]
        },
    ?assertEqual(MRArgsRef, consider_index_coverage(Index, Fields, MRArgs)),
    MRArgs1 = MRArgs#mrargs{include_docs = false},
    ?assertEqual(MRArgsRef, consider_index_coverage(Index, Fields, MRArgs1)).

consider_index_coverage_negative_test() ->
    Index = undefined,
    Fields = all_fields,
    MRArgs = #mrargs{include_docs = true},
    ?assertEqual(MRArgs, consider_index_coverage(Index, Fields, MRArgs)),
    MRArgs1 = #mrargs{include_docs = false},
    ?assertEqual(MRArgs1, consider_index_coverage(Index, Fields, MRArgs1)),
    % no extra attributes hence no effect
    Index1 =
        #idx{
            type = <<"json">>,
            def = {[{<<"fields">>, {[]}}]}
        },
    MRArgs2 = #mrargs{include_docs = false},
    ?assertEqual(MRArgs1, consider_index_coverage(Index1, [<<"_id">>], MRArgs2)).

derive_doc_from_index_test() ->
    Index =
        #idx{
            type = <<"json">>,
            def = {[{<<"fields">>, {[{<<"field1">>, undefined}, {<<"field2">>, undefined}]}}]}
        },
    DocId = doc_id,
    Keys = [key1, key2],
    ViewRow = #view_row{id = DocId, key = Keys},
    Doc = {[{<<"_id">>, DocId}, {<<"field2">>, key2}, {<<"field1">>, key1}]},
    ?assertEqual(Doc, derive_doc_from_index(Index, ViewRow)).

derive_doc_from_index_partitioned_test() ->
    Index =
        #idx{
            type = <<"json">>,
            def = {[{<<"fields">>, {[{<<"field1">>, undefined}, {<<"field2">>, undefined}]}}]}
        },
    DocId = doc_id,
    Keys = [key1, key2],
    ViewRow = #view_row{id = DocId, key = {p, partition, Keys}},
    Doc = {[{<<"_id">>, DocId}, {<<"field2">>, key2}, {<<"field1">>, key1}]},
    ?assertEqual(Doc, derive_doc_from_index(Index, ViewRow)).

composite_indexes_test() ->
    ?assertEqual([], composite_indexes([], [])),
    Index1 =
        #idx{
            type = <<"json">>,
            def = {[{<<"fields">>, {[{field1, undefined}, {field2, undefined}]}}]}
        },
    Index2 =
        #idx{
            type = <<"json">>,
            def = {[{<<"fields">>, {[{field1, undefined}, {field3, undefined}, {field4, range4}]}}]}
        },
    Index3 =
        #idx{
            type = <<"json">>,
            def = {[{<<"fields">>, {[{field3, undefined}, {field4, undefined}]}}]}
        },
    Indexes = [Index1, Index2, Index3],
    Ranges = [{field1, range1}, {field3, range3}, {field4, range4}],
    Result = [
        {Index1, [range1], 2}, {Index2, [range1, range3, range4], 0}, {Index3, [range3, range4], 1}
    ],
    ?assertEqual(Result, composite_indexes(Indexes, Ranges)).

create_test() ->
    Index = #idx{type = <<"json">>, def = {[{<<"fields">>, {[]}}]}},
    Indexes = [Index],
    Trace = #{},
    Ranges = [],
    Selector = {[]},
    Options = [{limit, limit}, {skip, skip}, {fields, fields}, {bookmark, bookmark}],
    Cursor =
        #cursor{
            db = db,
            index = Index,
            ranges = Ranges,
            selector = Selector,
            opts = Options,
            limit = limit,
            skip = skip,
            fields = fields,
            bookmark = bookmark,
            trace = #{sorted_index_ranges => [{Index, [], 0}]}
        },
    ?assertEqual({ok, Cursor}, create(db, {Indexes, Trace}, Selector, Options)).

to_selector(Map) ->
    test_util:as_selector(Map).

required_fields_all_fields_test() ->
    Cursor = #cursor{fields = all_fields},
    ?assertEqual(all_fields, required_fields(Cursor)).

required_fields_disjoint_fields_test() ->
    Fields1 = [<<"field1">>, <<"field2">>, <<"field3">>],
    Selector1 = to_selector(#{}),
    Cursor1 = #cursor{fields = Fields1, selector = Selector1},
    ?assertEqual([<<"field1">>, <<"field2">>, <<"field3">>], required_fields(Cursor1)),
    Fields2 = [<<"field1">>, <<"field2">>],
    Selector2 = to_selector(#{<<"field3">> => undefined, <<"field4">> => undefined}),
    Cursor2 = #cursor{fields = Fields2, selector = to_selector(Selector2)},
    ?assertEqual(
        [<<"field1">>, <<"field2">>, <<"field3">>, <<"field4">>], required_fields(Cursor2)
    ).

required_fields_overlapping_fields_test() ->
    Fields1 = [<<"field1">>, <<"field2">>, <<"field3">>],
    Selector1 = to_selector(#{<<"field3">> => undefined, <<"field4">> => undefined}),
    Cursor1 = #cursor{fields = Fields1, selector = Selector1},
    ?assertEqual(
        [<<"field1">>, <<"field2">>, <<"field3">>, <<"field4">>], required_fields(Cursor1)
    ),
    Fields2 = [<<"field3">>, <<"field1">>, <<"field2">>],
    Selector2 = to_selector(#{<<"field4">> => undefined, <<"field1">> => undefined}),
    Cursor2 = #cursor{fields = Fields2, selector = Selector2},
    ?assertEqual(
        [<<"field1">>, <<"field2">>, <<"field3">>, <<"field4">>], required_fields(Cursor2)
    ).

explain_test() ->
    Cursor =
        #cursor{
            ranges = [empty],
            fields = all_fields,
            opts = []
        },
    Response =
        [
            {mrargs,
                {[
                    {include_docs, true},
                    {view_type, map},
                    {reduce, false},
                    {partition, null},
                    {start_key, null},
                    {end_key, null},
                    {direction, fwd},
                    {stable, false},
                    {update, true},
                    {conflicts, undefined}
                ]}},
            {covering, false}
        ],
    ?assertEqual(Response, explain(Cursor)).

execute_test_() ->
    {
        foreach,
        fun() ->
            meck:new(foo, [non_strict]),
            meck:new(fabric)
        end,
        fun(_) ->
            meck:unload(fabric),
            meck:unload(foo)
        end,
        [
            ?TDEF_FE(t_execute_empty),
            ?TDEF_FE(t_execute_ok_all_docs),
            ?TDEF_FE(t_execute_ok_all_docs_with_execution_stats),
            ?TDEF_FE(t_execute_ok_query_view),
            ?TDEF_FE(t_execute_error_1),
            ?TDEF_FE(t_execute_error_2)
        ]
    }.

t_execute_empty(_) ->
    Cursor = #cursor{ranges = [empty]},
    meck:expect(fabric, all_docs, ['_', '_', '_', '_', '_'], meck:val(error)),
    meck:expect(fabric, query_view, ['_', '_', '_', '_', '_', '_'], meck:val(error)),
    ?assertEqual({ok, accumulator}, execute(Cursor, undefined, accumulator)),
    ?assertNot(meck:called(fabric, all_docs, '_')),
    ?assertNot(meck:called(fabric, query_view, '_')).

t_execute_ok_all_docs(_) ->
    Bookmark = bookmark,
    UserFnParameters = [{add_key, bookmark, Bookmark}, accumulator],
    meck:expect(foo, bar, UserFnParameters, meck:val({undefined, updated_accumulator})),
    Index = #idx{type = <<"json">>, def = all_docs},
    Selector = {[]},
    Fields = all_fields,
    Cursor =
        #cursor{
            index = Index,
            db = db,
            selector = Selector,
            fields = Fields,
            ranges = [{'$gte', start_key, '$lte', end_key}],
            opts = [{user_ctx, user_ctx}],
            bookmark = nil
        },
    Cursor1 =
        Cursor#cursor{
            user_acc = accumulator,
            user_fun = fun foo:bar/2,
            execution_stats = '_'
        },
    Cursor2 =
        Cursor1#cursor{
            bookmark = Bookmark,
            bookmark_docid = undefined,
            bookmark_key = undefined,
            execution_stats = #execution_stats{executionStartTime = {0, 0, 0}}
        },
    Extra =
        [
            {callback, {mango_cursor_view, view_cb}},
            {selector, Selector},
            {callback_args, #{
                selector => Selector,
                fields => Fields,
                covering_index => undefined
            }},
            {ignore_partition_query_limit, true},
            {execution_stats_map, true}
        ],
    Args =
        #mrargs{
            view_type = map,
            reduce = false,
            start_key = [start_key],
            end_key = [end_key, ?MAX_JSON_OBJ],
            include_docs = true,
            extra = Extra
        },
    Parameters = [
        db, [{user_ctx, user_ctx}], fun mango_cursor_view:handle_all_docs_message/2, Cursor1, Args
    ],
    meck:expect(fabric, all_docs, Parameters, meck:val({ok, Cursor2})),
    ?assertEqual({ok, updated_accumulator}, execute(Cursor, fun foo:bar/2, accumulator)),
    ?assert(meck:called(fabric, all_docs, '_')).

t_execute_ok_query_view(_) ->
    Bookmark = bookmark,
    UserFnParameters = [{add_key, bookmark, Bookmark}, accumulator],
    meck:expect(foo, bar, UserFnParameters, meck:val({undefined, updated_accumulator})),
    Index =
        #idx{
            type = <<"json">>,
            ddoc = <<"_design/ghibli">>,
            name = index_name,
            def = {[{<<"fields">>, {[{field1, undefined}]}}]}
        },
    Selector = {[]},
    Fields = all_fields,
    Cursor =
        #cursor{
            index = Index,
            db = db,
            selector = Selector,
            fields = Fields,
            ranges = [{'$gte', start_key, '$lte', end_key}],
            opts = [{user_ctx, user_ctx}],
            bookmark = nil
        },
    Cursor1 =
        Cursor#cursor{
            user_acc = accumulator,
            user_fun = fun foo:bar/2,
            execution_stats = '_'
        },
    Cursor2 =
        Cursor1#cursor{
            bookmark = Bookmark,
            bookmark_docid = undefined,
            bookmark_key = undefined,
            execution_stats = #execution_stats{executionStartTime = {0, 0, 0}}
        },
    Extra =
        [
            {callback, {mango_cursor_view, view_cb}},
            {selector, Selector},
            {callback_args, #{
                selector => Selector,
                fields => Fields,
                covering_index => undefined
            }},
            {ignore_partition_query_limit, true},
            {execution_stats_map, true}
        ],
    Args =
        #mrargs{
            view_type = map,
            reduce = false,
            start_key = [start_key],
            end_key = [end_key, ?MAX_JSON_OBJ],
            include_docs = true,
            extra = Extra
        },
    Parameters = [
        db,
        [{user_ctx, user_ctx}],
        <<"ghibli">>,
        index_name,
        fun mango_cursor_view:handle_message/2,
        Cursor1,
        Args
    ],
    meck:expect(fabric, query_view, Parameters, meck:val({ok, Cursor2})),
    ?assertEqual({ok, updated_accumulator}, execute(Cursor, fun foo:bar/2, accumulator)),
    ?assert(meck:called(fabric, query_view, '_')).

t_execute_ok_all_docs_with_execution_stats(_) ->
    Bookmark = bookmark,
    Stats =
        {[
            {total_keys_examined, 0},
            {total_docs_examined, 0},
            {total_quorum_docs_examined, 0},
            {results_returned, 0},
            {execution_time_ms, '_'}
        ]},
    UserFnDefinition =
        [
            {[{add_key, bookmark, Bookmark}, accumulator], {undefined, updated_accumulator1}},
            {
                [{add_key, execution_stats, Stats}, updated_accumulator1],
                {undefined, updated_accumulator2}
            }
        ],
    meck:expect(foo, bar, UserFnDefinition),
    Index = #idx{type = <<"json">>, def = all_docs},
    Selector = {[]},
    Fields = all_fields,
    Cursor =
        #cursor{
            index = Index,
            db = db,
            selector = Selector,
            fields = Fields,
            ranges = [{'$gte', start_key, '$lte', end_key}],
            opts = [{user_ctx, user_ctx}, {execution_stats, true}],
            bookmark = nil
        },
    Cursor1 =
        Cursor#cursor{
            user_acc = accumulator,
            user_fun = fun foo:bar/2,
            execution_stats = '_'
        },
    Cursor2 =
        Cursor1#cursor{
            bookmark = Bookmark,
            bookmark_docid = undefined,
            bookmark_key = undefined,
            execution_stats = #execution_stats{executionStartTime = {0, 0, 0}}
        },
    Extra =
        [
            {callback, {mango_cursor_view, view_cb}},
            {selector, Selector},
            {callback_args, #{
                selector => Selector,
                fields => Fields,
                covering_index => undefined
            }},
            {ignore_partition_query_limit, true},
            {execution_stats_map, true}
        ],
    Args =
        #mrargs{
            view_type = map,
            reduce = false,
            start_key = [start_key],
            end_key = [end_key, ?MAX_JSON_OBJ],
            include_docs = true,
            extra = Extra
        },
    Parameters = [
        db, [{user_ctx, user_ctx}], fun mango_cursor_view:handle_all_docs_message/2, Cursor1, Args
    ],
    meck:expect(fabric, all_docs, Parameters, meck:val({ok, Cursor2})),
    ?assertEqual({ok, updated_accumulator2}, execute(Cursor, fun foo:bar/2, accumulator)),
    ?assert(meck:called(fabric, all_docs, '_')).

t_execute_error_1(_) ->
    Cursor =
        #cursor{
            index = #idx{type = <<"json">>, ddoc = <<"_design/ghibli">>, name = index_name},
            db = db,
            selector = {[]},
            fields = all_fields,
            ranges = [{'$gte', start_key, '$lte', end_key}],
            opts = [{user_ctx, user_ctx}],
            bookmark = nil
        },
    Parameters = [
        db, '_', <<"ghibli">>, index_name, fun mango_cursor_view:handle_message/2, '_', '_'
    ],
    meck:expect(fabric, query_view, Parameters, meck:val({error, reason})),
    ?assertEqual({error, reason}, execute(Cursor, undefined, accumulator)).

t_execute_error_2(_) ->
    Cursor =
        #cursor{
            index = #idx{type = <<"json">>, ddoc = <<"_design/ghibli">>, name = index_name},
            db = db,
            selector = {[]},
            fields = all_fields,
            ranges = [{'$gte', start_key, '$lte', end_key}],
            opts = [{user_ctx, user_ctx}],
            bookmark = nil
        },
    Parameters = [
        db, '_', <<"ghibli">>, index_name, fun mango_cursor_view:handle_message/2, '_', '_'
    ],
    meck:expect(fabric, query_view, Parameters, meck:val({ok, {error, reason}})),
    ?assertEqual({error, {error, reason}}, execute(Cursor, undefined, accumulator)).

view_cb_test_() ->
    {
        foreach,
        fun() ->
            meck:new(rexi)
        end,
        fun(_) ->
            meck:unload(rexi)
        end,
        [
            ?TDEF_FE(t_view_cb_meta),
            ?TDEF_FE(t_view_cb_row_matching_regular_doc),
            ?TDEF_FE(t_view_cb_row_non_matching_regular_doc),
            ?TDEF_FE(t_view_cb_row_null_doc),
            ?TDEF_FE(t_view_cb_row_missing_doc_triggers_quorum_fetch),
            ?TDEF_FE(t_view_cb_row_matching_covered_doc),
            ?TDEF_FE(t_view_cb_row_non_matching_covered_doc),
            ?TDEF_FE(t_view_cb_row_backwards_compatible),
            ?TDEF_FE(t_view_cb_complete_shard_stats_v1),
            ?TDEF_FE(t_view_cb_complete_shard_stats_v2),
            ?TDEF_FE(t_view_cb_ok)
        ]
    }.

t_view_cb_meta(_) ->
    meck:expect(rexi, stream2, [{meta, meta}], meck:val(ok)),
    ?assertEqual({ok, accumulator}, view_cb({meta, meta}, accumulator)),
    ?assert(meck:called(rexi, stream2, '_')).

t_view_cb_row_matching_regular_doc(_) ->
    Row = [{id, id}, {key, key}, {doc, doc}],
    Result = #view_row{id = id, key = key, doc = doc},
    meck:expect(rexi, stream2, [Result], meck:val(ok)),
    Accumulator =
        #mrargs{
            extra = [
                {callback_args, #{
                    selector => {[]},
                    fields => all_fields,
                    covering_index => undefined
                }}
            ]
        },
    mango_execution_stats:shard_init(),
    ?assertEqual({ok, Accumulator}, view_cb({row, Row}, Accumulator)),
    ?assert(meck:called(rexi, stream2, '_')).

t_view_cb_row_non_matching_regular_doc(_) ->
    Doc = {[]},
    Row = [{id, id}, {key, key}, {doc, Doc}],
    meck:expect(rexi, stream2, ['_'], undefined),
    Accumulator =
        #mrargs{
            extra = [
                {callback_args, #{
                    selector => {[{<<"field">>, {[{<<"$exists">>, true}]}}]},
                    fields => all_fields,
                    covering_index => undefined
                }}
            ]
        },
    mango_execution_stats:shard_init(),
    put(mango_last_msg_timestamp, os:timestamp()),
    ?assertEqual({ok, Accumulator}, view_cb({row, Row}, Accumulator)),
    ?assertNot(meck:called(rexi, stream2, '_')).

t_view_cb_row_null_doc(_) ->
    Row = [{id, id}, {key, key}, {doc, null}],
    meck:expect(rexi, stream2, ['_'], undefined),
    Accumulator =
        #mrargs{
            extra = [
                {callback_args, #{
                    selector => {[]},
                    fields => all_fields,
                    covering_index => undefined
                }}
            ]
        },
    mango_execution_stats:shard_init(),
    put(mango_last_msg_timestamp, os:timestamp()),
    ?assertEqual({ok, Accumulator}, view_cb({row, Row}, Accumulator)),
    ?assertNot(meck:called(rexi, stream2, '_')).

t_view_cb_row_missing_doc_triggers_quorum_fetch(_) ->
    Row = [{id, id}, {key, key}, {doc, undefined}],
    ViewRow = #view_row{id = id, key = key, doc = undefined},
    meck:expect(rexi, stream2, [ViewRow], meck:val(ok)),
    Accumulator =
        #mrargs{
            extra = [
                {callback_args, #{
                    selector => {[]},
                    fields => all_fields,
                    covering_index => undefined
                }}
            ]
        },
    mango_execution_stats:shard_init(),
    ?assertEqual({ok, Accumulator}, view_cb({row, Row}, Accumulator)),
    ?assert(meck:called(rexi, stream2, '_')).

t_view_cb_row_matching_covered_doc(_) ->
    Keys = [key1, key2],
    Row = [{id, id}, {key, Keys}, {doc, undefined}],
    Doc = {[{<<"field1">>, key1}, {<<"field2">>, key2}]},
    Result = #view_row{id = id, key = Keys, doc = Doc},
    Fields = [<<"field1">>, <<"field2">>],
    Index =
        #idx{
            type = <<"json">>,
            def = {[{<<"fields">>, {[{<<"field1">>, undefined}, {<<"field2">>, undefined}]}}]}
        },
    meck:expect(rexi, stream2, [Result], meck:val(ok)),
    Accumulator =
        #mrargs{
            extra = [
                {callback_args, #{
                    selector => {[]},
                    fields => Fields,
                    covering_index => Index
                }}
            ]
        },
    mango_execution_stats:shard_init(),
    ?assertEqual({ok, Accumulator}, view_cb({row, Row}, Accumulator)),
    ?assert(meck:called(rexi, stream2, '_')).

t_view_cb_row_non_matching_covered_doc(_) ->
    Row = [{id, id}, {key, [key1, key2]}, {doc, undefined}],
    Fields = [<<"field1">>, <<"field2">>],
    Index =
        #idx{
            type = <<"json">>,
            def = {[{<<"fields">>, {[{<<"field1">>, undefined}, {<<"field2">>, undefined}]}}]}
        },
    meck:expect(rexi, stream2, ['_'], undefined),
    Accumulator =
        #mrargs{
            extra = [
                {callback_args, #{
                    selector => {[{<<"field">>, {[{<<"$exists">>, true}]}}]},
                    fields => Fields,
                    covering_index => Index
                }}
            ]
        },
    mango_execution_stats:shard_init(),
    put(mango_last_msg_timestamp, os:timestamp()),
    ?assertEqual({ok, Accumulator}, view_cb({row, Row}, Accumulator)),
    ?assertNot(meck:called(rexi, stream2, '_')).

t_view_cb_row_backwards_compatible(_) ->
    Row = [{id, id}, {key, key}, {doc, null}],
    meck:expect(rexi, stream2, ['_'], undefined),
    Accumulator = #mrargs{extra = [{selector, {[]}}]},
    mango_execution_stats:shard_init(),
    put(mango_last_msg_timestamp, os:timestamp()),
    ?assertEqual({ok, Accumulator}, view_cb({row, Row}, Accumulator)),
    ?assertNot(meck:called(rexi, stream2, '_')).

t_view_cb_complete_shard_stats_v1(_) ->
    meck:expect(rexi, stream2, [{execution_stats, {docs_examined, '_'}}], meck:val(ok)),
    meck:expect(rexi, stream_last, [complete], meck:val(ok)),
    Accumulator = #mrargs{},
    mango_execution_stats:shard_init(),
    ?assertEqual({ok, Accumulator}, view_cb(complete, Accumulator)),
    ?assert(meck:called(rexi, stream2, '_')),
    ?assert(meck:called(rexi, stream_last, '_')).

t_view_cb_complete_shard_stats_v2(_) ->
    ShardStats = #{docs_examined => '_', keys_examined => '_'},
    meck:expect(rexi, stream2, [{execution_stats, ShardStats}], meck:val(ok)),
    meck:expect(rexi, stream_last, [complete], meck:val(ok)),
    Accumulator = #mrargs{extra = [{execution_stats_map, true}]},
    mango_execution_stats:shard_init(),
    ?assertEqual({ok, Accumulator}, view_cb(complete, Accumulator)),
    ?assert(meck:called(rexi, stream2, '_')),
    ?assert(meck:called(rexi, stream_last, '_')).

t_view_cb_ok(_) ->
    meck:expect(rexi, reply, [{ok, ddoc_updated}], meck:val(ok)),
    view_cb(ok, ddoc_updated),
    ?assert(meck:called(rexi, reply, '_')).

maybe_send_mango_ping_test_() ->
    {
        foreach,
        fun() ->
            meck:new(rexi)
        end,
        fun(_) ->
            meck:unload(rexi)
        end,
        [
            ?TDEF_FE(t_maybe_send_mango_ping_nop),
            ?TDEF_FE(t_maybe_send_mango_ping_happens)
        ]
    }.

t_maybe_send_mango_ping_nop(_) ->
    put(mango_last_msg_timestamp, os:timestamp()),
    meck:expect(rexi, ping, [], meck:val(error)),
    ?assertEqual(ok, maybe_send_mango_ping()),
    ?assertNot(meck:called(rexi, ping, '_')).

t_maybe_send_mango_ping_happens(_) ->
    put(mango_last_msg_timestamp, {0, 0, 0}),
    meck:expect(rexi, ping, [], meck:val(ok)),
    maybe_send_mango_ping(),
    ?assert(meck:called(rexi, ping, '_')),
    Timestamp = get(mango_last_msg_timestamp),
    ?assertNotEqual(Timestamp, {0, 0, 0}).

ddocid_test() ->
    ?assertEqual(<<"name">>, ddocid(#idx{ddoc = <<"_design/name">>})),
    ?assertEqual(something_else, ddocid(#idx{ddoc = something_else})).

is_design_doc_test() ->
    ?assert(is_design_doc([{id, <<"_design/name">>}])),
    ?assertNot(is_design_doc([{id, something_else}])).

handle_message_test_() ->
    {
        foreach,
        fun() ->
            meck:new(foo, [non_strict])
        end,
        fun(_) ->
            meck:unload(foo)
        end,
        [
            ?TDEF_FE(t_handle_message_meta),
            ?TDEF_FE(t_handle_message_row_ok_above_limit),
            ?TDEF_FE(t_handle_message_row_ok_at_limit),
            ?TDEF_FE(t_handle_message_row_ok_skip),
            ?TDEF_FE(t_handle_message_row_ok_triggers_quorum_fetch_match),
            ?TDEF_FE(t_handle_message_row_ok_triggers_quorum_fetch_no_match),
            ?TDEF_FE(t_handle_message_row_no_match),
            ?TDEF_FE(t_handle_message_row_error),
            ?TDEF_FE(t_handle_message_execution_stats_v1),
            ?TDEF_FE(t_handle_message_execution_stats_v2),
            ?TDEF_FE(t_handle_message_complete),
            ?TDEF_FE(t_handle_message_error)
        ]
    }.

t_handle_message_meta(_) ->
    ?assertEqual({ok, cursor}, handle_message({meta, undefined}, cursor)).

t_handle_message_row_ok_above_limit(_) ->
    Doc = {[{<<"field1">>, value1}, {<<"field2">>, value2}]},
    meck:expect(foo, bar, [{row, Doc}, accumulator], meck:val({go, updated_accumulator})),
    Cursor =
        #cursor{
            execution_stats = #execution_stats{resultsReturned = 0},
            fields = all_fields,
            limit = 9,
            user_acc = accumulator,
            user_fun = fun foo:bar/2
        },
    Row = [{id, id}, {key, key}, {doc, Doc}],
    Cursor1 =
        Cursor#cursor{
            execution_stats = #execution_stats{resultsReturned = 1},
            limit = 8,
            user_acc = updated_accumulator,
            bookmark_docid = id,
            bookmark_key = key
        },
    ?assertEqual({go, Cursor1}, handle_message({row, Row}, Cursor)).

t_handle_message_row_ok_at_limit(_) ->
    Cursor =
        #cursor{
            execution_stats = #execution_stats{resultsReturned = n},
            fields = all_fields,
            limit = 0
        },
    Row = [{doc, {[]}}],
    ?assertEqual({stop, Cursor}, handle_message({row, Row}, Cursor)).

t_handle_message_row_ok_skip(_) ->
    Cursor =
        #cursor{
            execution_stats = #execution_stats{resultsReturned = n},
            fields = all_fields,
            skip = 8
        },
    Row = [{doc, {[]}}],
    Cursor1 = Cursor#cursor{skip = 7},
    ?assertEqual({ok, Cursor1}, handle_message({row, Row}, Cursor)).

t_handle_message_row_ok_triggers_quorum_fetch_match(_) ->
    Doc = #doc{id = id, body = {[{<<"field">>, something}]}},
    Object = {[{<<"_id">>, id}, {<<"field">>, something}]},
    meck:expect(foo, bar, [{row, Object}, accumulator], meck:val({go, updated_accumulator})),
    Cursor =
        #cursor{
            db = db,
            opts = opts,
            execution_stats =
                #execution_stats{
                    totalQuorumDocsExamined = 0,
                    resultsReturned = 0
                },
            fields = all_fields,
            selector = {[{<<"field">>, {[{<<"$exists">>, true}]}}]},
            user_fun = fun foo:bar/2,
            user_acc = accumulator,
            limit = 1
        },
    Row = [{id, id}, {doc, undefined}],
    Cursor1 =
        Cursor#cursor{
            execution_stats =
                #execution_stats{
                    totalQuorumDocsExamined = 1,
                    resultsReturned = 1
                },
            user_acc = updated_accumulator,
            limit = 0,
            bookmark_docid = id
        },
    meck:expect(mango_util, defer, [fabric, open_doc, [db, id, opts]], meck:val({ok, Doc})),
    ?assertEqual({go, Cursor1}, handle_message({row, Row}, Cursor)),
    ?assert(meck:called(mango_util, defer, '_')),
    meck:delete(mango_util, defer, 3).

t_handle_message_row_ok_triggers_quorum_fetch_no_match(_) ->
    Cursor =
        #cursor{
            db = db,
            opts = opts,
            execution_stats = #execution_stats{totalQuorumDocsExamined = 0},
            fields = all_fields,
            selector = {[{<<"field">>, {[{<<"$exists">>, true}]}}]}
        },
    Row = [{id, id}, {doc, undefined}],
    Cursor1 =
        Cursor#cursor{
            execution_stats = #execution_stats{totalQuorumDocsExamined = 1}
        },
    Doc = #doc{id = id, body = {[]}},
    meck:expect(mango_util, defer, [fabric, open_doc, [db, id, opts]], meck:val({ok, Doc})),
    ?assertEqual({ok, Cursor1}, handle_message({row, Row}, Cursor)),
    ?assert(meck:called(mango_util, defer, '_')),
    meck:delete(mango_util, defer, 3).

t_handle_message_row_no_match(_) ->
    Cursor =
        #cursor{
            execution_stats = #execution_stats{resultsReturned = n}
        },
    Row = [{doc, null}],
    ?assertEqual({ok, Cursor}, handle_message({row, Row}, Cursor)).

t_handle_message_row_error(_) ->
    Cursor =
        #cursor{
            db = db,
            opts = opts,
            execution_stats = #execution_stats{totalQuorumDocsExamined = 0}
        },
    Row = [{id, id}, {doc, undefined}],
    meck:expect(mango_util, defer, [fabric, open_doc, [db, id, opts]], meck:val(error)),
    meck:expect(couch_log, error, ['_', [mango_cursor_view, error]], meck:val(ok)),
    ?assertEqual({ok, Cursor}, handle_message({row, Row}, Cursor)),
    ?assert(meck:called(mango_util, defer, '_')),
    ?assert(meck:called(couch_log, error, '_')),
    meck:delete(mango_util, defer, 3),
    meck:delete(couch_log, error, 2).

t_handle_message_execution_stats_v1(_) ->
    ShardStats = {docs_examined, 42},
    ExecutionStats = #execution_stats{totalDocsExamined = 11},
    ExecutionStats1 = #execution_stats{totalDocsExamined = 53},
    Cursor = #cursor{execution_stats = ExecutionStats},
    Cursor1 = #cursor{execution_stats = ExecutionStats1},
    ?assertEqual({ok, Cursor1}, handle_message({execution_stats, ShardStats}, Cursor)).

t_handle_message_execution_stats_v2(_) ->
    ShardStats = #{docs_examined => 42, keys_examined => 53},
    ExecutionStats = #execution_stats{totalDocsExamined = 11, totalKeysExamined = 22},
    ExecutionStats1 = #execution_stats{totalDocsExamined = 53, totalKeysExamined = 75},
    Cursor = #cursor{execution_stats = ExecutionStats},
    Cursor1 = #cursor{execution_stats = ExecutionStats1},
    ?assertEqual({ok, Cursor1}, handle_message({execution_stats, ShardStats}, Cursor)).

t_handle_message_complete(_) ->
    ?assertEqual({ok, cursor}, handle_message(complete, cursor)).

t_handle_message_error(_) ->
    ?assertEqual({error, reason}, handle_message({error, reason}, undefined)).

handle_all_docs_message_ddoc_test() ->
    Row = [{id, <<"_design/foobar">>}],
    ?assertEqual({ok, cursor}, handle_all_docs_message({row, Row}, cursor)).

handle_all_docs_message_row_test() ->
    Cursor =
        #cursor{
            execution_stats = #execution_stats{resultsReturned = n}
        },
    Row = [{doc, null}],
    ?assertEqual({ok, Cursor}, handle_all_docs_message({row, Row}, Cursor)).

handle_all_docs_message_regular_test() ->
    ?assertEqual(handle_message(complete, cursor), handle_all_docs_message(complete, cursor)).

%% Test the doc_member_and_extract bypasses the selector check if it receives
%% a document in RowProps.doc.
does_not_refetch_doc_with_value_test() ->
    Cursor = #cursor{
        db = <<"db">>,
        opts = [],
        execution_stats = #execution_stats{},
        selector = mango_selector:normalize({[{<<"user_id">>, <<"1234">>}]})
    },
    RowProps = [
        {id, <<"b06aadcf-cd0f-4ca6-9f7e-2c993e48d4c4">>},
        {key, <<"b06aadcf-cd0f-4ca6-9f7e-2c993e48d4c4">>},
        {doc,
            {
                [
                    {<<"_id">>, <<"b06aadcf-cd0f-4ca6-9f7e-2c993e48d4c4">>},
                    {<<"_rev">>, <<"1-a954fe2308f14307756067b0e18c2968">>},
                    {<<"user_id">>, 11}
                ]
            }}
    ],
    {Match, _, _} = doc_member_and_extract(Cursor, RowProps),
    ?assertEqual(Match, ok).

%% Test that field filtering is duplicated in doc_member_and_extract even when
%% returning a value via RowProps.doc (ie, should have been done on the shard).
%% This is needed temporarily for mixed version upgrades, as some shards may
%% not have performed the field extraction. This can be later removed.
doc_member_and_extract_fields_test() ->
    Cursor = #cursor{
        db = <<"db">>,
        opts = [],
        execution_stats = #execution_stats{},
        %% no selector here as we should be bypassing this in the case of
        %% shard level selector application.
        fields = [<<"user_id">>, <<"a_non_existent_field">>]
    },
    RowProps = [
        {id, <<"b06aadcf-cd0f-4ca6-9f7e-2c993e48d4c4">>},
        {key, <<"b06aadcf-cd0f-4ca6-9f7e-2c993e48d4c4">>},
        {doc,
            {
                [
                    {<<"_id">>, <<"b06aadcf-cd0f-4ca6-9f7e-2c993e48d4c4">>},
                    {<<"_rev">>, <<"1-a954fe2308f14307756067b0e18c2968">>},
                    {<<"user_id">>, 11}
                ]
            }}
    ],
    {Match, Doc, _} = doc_member_and_extract(Cursor, RowProps),
    ?assertEqual(ok, Match),
    ?assertEqual({[{<<"user_id">>, 11}]}, Doc).

%% match_and_extract_doc should return full Doc when Doc matches Selector and
%% Fields is undefined.
match_and_extract_doc_match_test() ->
    Doc = {[{<<"_id">>, <<"myid">>}, {<<"_rev">>, <<"myrev">>}, {<<"user_id">>, 11}]},
    Selector = mango_selector:normalize({[{<<"user_id">>, 11}]}),
    Fields = undefined,
    {Match, FinalDoc} = match_and_extract_doc(Doc, Selector, Fields),
    ?assertEqual(match, Match),
    ?assertEqual(Doc, FinalDoc).

%% match_and_extract_doc should return projected Doc when Doc matches Selector
%% and Fields is a list of fields.
match_and_extract_doc_matchextract_test() ->
    Doc = {[{<<"_id">>, <<"myid">>}, {<<"_rev">>, <<"myrev">>}, {<<"user_id">>, 11}]},
    Selector = mango_selector:normalize({[{<<"user_id">>, 11}]}),
    Fields = [<<"_id">>, <<"user_id">>],
    {Match, FinalDoc} = match_and_extract_doc(Doc, Selector, Fields),
    ?assertEqual(match, Match),
    ?assertEqual({[{<<"_id">>, <<"myid">>}, {<<"user_id">>, 11}]}, FinalDoc).

%% match_and_extract_doc should return no document when Doc does not match
%% Selector.
match_and_extract_doc_nomatch_test() ->
    Doc = {[{<<"_id">>, <<"myid">>}, {<<"_rev">>, <<"myrev">>}, {<<"user_id">>, 11}]},
    Selector = mango_selector:normalize({[{<<"user_id">>, <<"1234">>}]}),
    Fields = undefined,
    {Match, FinalDoc} = match_and_extract_doc(Doc, Selector, Fields),
    ?assertEqual(no_match, Match),
    ?assertEqual(undefined, FinalDoc).

%% match_and_extract_doc should return no document when Doc does not match
%% Selector even if Fields is defined.
match_and_extract_doc_nomatch_fields_test() ->
    Doc = {[{<<"_id">>, <<"myid">>}, {<<"_rev">>, <<"myrev">>}, {<<"user_id">>, 11}]},
    Selector = mango_selector:normalize({[{<<"user_id">>, 1234}]}),
    Fields = [<<"_id">>, <<"user_id">>],
    {Match, FinalDoc} = match_and_extract_doc(Doc, Selector, Fields),
    ?assertEqual(no_match, Match),
    ?assertEqual(undefined, FinalDoc).

%% Query planner tests:
%% - there should be no comparison for a singleton list, with a trivial result
choose_best_index_with_singleton_test() ->
    IndexRanges = [{index, ranges, undefined}],
    Result = {{index, ranges}, IndexRanges},
    ?assertEqual(Result, choose_best_index([{index, ranges, undefined}])).

%% - choose the index with the lowest difference between its prefix and field ranges
choose_best_index_lowest_difference_test() ->
    IndexRanges1 =
        [
            {index1, ranges1, 3},
            {index2, ranges2, 2},
            {index3, ranges3, 1}
        ],
    SortedIndexRanges1 =
        [
            {index3, ranges3, 1},
            {index2, ranges2, 2},
            {index1, ranges1, 3}
        ],
    Result1 = {{index3, ranges3}, SortedIndexRanges1},
    ?assertEqual(Result1, choose_best_index(IndexRanges1)),
    IndexRanges2 =
        [
            {index1, ranges1, 3},
            {index2, ranges2, 1},
            {index3, ranges3, 2}
        ],
    SortedIndexRanges2 =
        [
            {index2, ranges2, 1},
            {index3, ranges3, 2},
            {index1, ranges1, 3}
        ],
    Result2 = {{index2, ranges2}, SortedIndexRanges2},
    ?assertEqual(Result2, choose_best_index(IndexRanges2)).

%% - if that is equal, choose the index with the least number of fields in the index
choose_best_index_least_number_of_fields_test() ->
    Index = json_index(dbname, design_document, index_name),
    [Index1, Index2, Index3] = [with_dummy_columns(Index, N) || N <- [6, 3, 9]],
    IndexRanges =
        [
            {Index1, ranges1, 1},
            {Index2, ranges2, 1},
            {Index3, ranges3, 1}
        ],
    SortedIndexRanges =
        [
            {Index2, ranges2, 1},
            {Index1, ranges1, 1},
            {Index3, ranges3, 1}
        ],
    Result = {{Index2, ranges2}, SortedIndexRanges},
    ?assertEqual(Result, choose_best_index(IndexRanges)).

%% - otherwise, choose alphabetically based on the index properties:
choose_best_index_lowest_index_triple_test() ->
    WithSomeColumns = fun(Idx) -> with_dummy_columns(Idx, 3) end,

    % - database name
    Index1 = WithSomeColumns(json_index(<<"db_a">>, <<"_design/c">>, <<"B">>)),
    Index2 = WithSomeColumns(json_index(<<"db_b">>, <<"_design/b">>, <<"C">>)),
    Index3 = WithSomeColumns(json_index(<<"db_c">>, <<"_design/a">>, <<"A">>)),
    IndexRanges1 =
        [
            {Index1, ranges1, 1},
            {Index2, ranges2, 1},
            {Index3, ranges3, 1}
        ],
    SortedIndexRanges1 =
        [
            {Index1, ranges1, 1},
            {Index2, ranges2, 1},
            {Index3, ranges3, 1}
        ],
    Result1 = {{Index1, ranges1}, SortedIndexRanges1},
    ?assertEqual(Result1, choose_best_index(IndexRanges1)),

    % - if that is equal, design document name
    Index4 = WithSomeColumns(json_index(<<"db_a">>, <<"_design/c">>, <<"B">>)),
    Index5 = WithSomeColumns(json_index(<<"db_a">>, <<"_design/b">>, <<"C">>)),
    Index6 = WithSomeColumns(json_index(<<"db_a">>, <<"_design/a">>, <<"A">>)),
    IndexRanges2 =
        [
            {Index4, ranges4, 1},
            {Index5, ranges5, 1},
            {Index6, ranges6, 1}
        ],
    SortedIndexRanges2 =
        [
            {Index6, ranges6, 1},
            {Index5, ranges5, 1},
            {Index4, ranges4, 1}
        ],
    Result2 = {{Index6, ranges6}, SortedIndexRanges2},
    ?assertEqual(Result2, choose_best_index(IndexRanges2)),

    % - otherwise, index name
    Index7 = WithSomeColumns(json_index(<<"db_a">>, <<"_design/a">>, <<"B">>)),
    Index8 = WithSomeColumns(json_index(<<"db_a">>, <<"_design/a">>, <<"C">>)),
    Index9 = WithSomeColumns(json_index(<<"db_a">>, <<"_design/a">>, <<"A">>)),
    IndexRanges3 =
        [
            {Index7, ranges7, 1},
            {Index8, ranges8, 1},
            {Index9, ranges9, 1}
        ],
    SortedIndexRanges3 =
        [
            {Index9, ranges9, 1},
            {Index7, ranges7, 1},
            {Index8, ranges8, 1}
        ],
    Result3 = {{Index9, ranges9}, SortedIndexRanges3},
    ?assertEqual(Result3, choose_best_index(IndexRanges3)).

json_index(DbName, DesignDoc, Name) ->
    #idx{
        dbname = DbName,
        ddoc = DesignDoc,
        name = Name,
        type = <<"json">>
    }.

with_dummy_columns(Index, Count) ->
    Columns =
        {[{<<"field", (integer_to_binary(I))/binary>>, undefined} || I <- lists:seq(1, Count)]},
    Index#idx{def = {[{<<"fields">>, Columns}]}}.

update_bookmark_keys_test() ->
    Cursor0 = #cursor{limit = 0},
    ?assertEqual(Cursor0, update_bookmark_keys(Cursor0, undefined)),
    Cursor1 = #cursor{limit = 1},
    Row = [{id, id}, {key, key}],
    UpdatedCursor1 = Cursor1#cursor{bookmark_docid = id, bookmark_key = key},
    ?assertEqual(UpdatedCursor1, update_bookmark_keys(Cursor1, Row)).
-endif.
