-module(mango_cursor).

-behavior(gen_server).


-export([
    create/4,
    close/1,
    next/1
]).

-export([
    start_link/1
]).

-export([
    init/1,
    terminate/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3
]).


-record(st, {
    dbname,
    index,
    ranges,
    selector,
    opts,
    ctx,
    worker,
    waiter,
    batch
}).


-define(SUPERVISOR, mango_cursor_sup).


create(DbName, NonNormSelector, Opts, Ctx) ->
    Selector = mango_selector:normalize(NonNormSelector),
    twig:log(err, "Selector: ~p", [Selector]),
    IndexFields = mango_selector:index_fields(Selector),
    twig:log(err, "Index Fields: ~p", [IndexFields]),
    ExistingIndexes = mango_index:list(DbName, Opts, Ctx),
    twig:log(err, "Existing Indexes: ~p", [ExistingIndexes]),
    UsableIndexes = find_usable_indexes(IndexFields, ExistingIndexes),
    twig:log(err, "Usable Indexes: ~p", [UsableIndexes]),
    FieldRanges = find_field_ranges(Selector, IndexFields),
    twig:log(err, "Field Ranges: ~p", [FieldRanges]),
    Composited = composite_indexes(UsableIndexes, FieldRanges),
    twig:log(err, "Composited: ~p", [Composited]),
    {Index, Ranges} = choose_best_index(DbName, Composited),
    twig:log(err, "Index: ~p~nRanges: ~p", [Index, Ranges]),
    St = #st{
        index = Index,
        ranges = Ranges,
        selector = Selector,
        opts = Opts,
        ctx = Ctx
    },
    {ok, Pid} = supervisor:start_child(?SUPERVISOR, [St]),
    mango_cursor_id:register(Pid).


close(Id) ->
    {ok, Pid} = mango_cursor_id:lookup(Id),
    gen_server:cast(Pid, close).


next(Id) ->
    % This timeout shoudl be controlled by the opts
    % passed to create/4. We should pass infinity here
    % and then have the gen_server reply back with a
    % timeout if it doesn't get the next batch in time.
    {ok, Pid} = mango_cursor_id:lookup(Id),
    gen_server:call(Pid, next, 600000).


start_link(St) ->
    gen_server:start_link(?MODULE, St, []).


init(#st{}=St) ->
    {ok, St}.


terminate(_Reason, St) ->
    if St#st.worker == undefined -> ok; true ->
        exit(St#st.worker, closing)
    end,
    ok.


handle_call(next, From, #st{worker=undefined}=St) ->
    {ok, Pid} = spawn_worker(St),
    handle_call(next, From, St#st{worker=Pid});

handle_call(next, From, #st{waiter=undefined, batch=undefined}=St) ->
    {noreply, St#st{waiter=From}};

handle_call(next, _From, #st{waiter=undefined, batch=Batch}=St) ->
    St#st.worker ! {self(), next},
    {reply, {ok, Batch}, St#st{batch=undefined}};

handle_call(Msg, _From, St) ->
    {stop, {invalid_call, Msg}, {invalid_call, Msg}, St}.


handle_cast(close, St) ->
    if St#st.waiter == undefined -> ok; true ->
        gen_server:reply(St#st.waiter, closing)
    end,
    {stop, normal, St#st{waiter=undefined}};
    
handle_cast(Msg, St) ->
    {stop, {invalid_cast, Msg}, St}.


handle_info({Pid, batch, Batch}, #st{worker=Pid, waiter=W, batch=B}=St) when
        W == undefined, B == undefined ->
    {noreply, St#st{batch = Batch}};

handle_info({Pid, batch, Batch}, #st{worker=Pid, waiter=W, batch=B}=St) when
        W /= undefined, B == undefined ->
    gen_server:reply(W, {ok, Batch}),
    case St#st.worker of
        undefined ->
            {stop, normal, St#st{waiter=undefined}};
        _ ->
            St#st.worker ! {self(), next},
            {noreply, St#st{waiter=undefined}}
    end;

handle_info({'DOWN', _, _, Pid, normal}, #st{worker=Pid, batch=B}=St) when
        B /= undefined ->
    {noreply, St#st{worker=undefined}};

handle_info({'DOWN', _, _, Pid, Reason}, #st{worker=Pid}=St) ->
    twig:log(err, "Cursor worker died: ~p ~p", [Reason, St]),
    {stop, Reason, St};

handle_info(Msg, St) ->
    {stop, {invalid_info, Msg}, St}.


code_change(_OldVsn, St, _Extra) ->
    {ok, St}.


% Find the intersection between the Possible and Existing
% indexes.
find_usable_indexes([], _) ->
    throw({no_usable_index, query_unsupported});
find_usable_indexes(Possible, []) ->
    throw({no_usable_index, Possible});
find_usable_indexes(Possible, Existing) ->
    Usable = lists:foldl(fun(Idx, Acc) ->
        [Col0 | _] = mango_index:columns(Idx),
        case lists:member(Col0, Possible) of
            true ->
                [Idx | Acc];
            false ->
                Acc
        end
    end, [], Existing),
    if length(Usable) > 0 -> ok; true ->
        throw({no_usable_index, Possible})
    end,
    Usable.


% For each field, return {Field, Range}
find_field_ranges(Selector, Fields) ->
    find_field_ranges(Selector, Fields, []).

find_field_ranges(_Selector, [], Acc) ->
    lists:reverse(Acc);
find_field_ranges(Selector, [Field | Rest], Acc) ->
    case mango_selector:range(Selector, Field) of
        empty ->
            [{Field, empty}];
        Range ->
            find_field_ranges(Selector, Rest, [{Field, Range} | Acc])
    end.


% Any of these indexes may be a composite index. For each
% index find the most specific set of fields for each
% index. Ie, if an index has columns a, b, c, d, then
% check FieldRanges for a, b, c, and d and return
% the longest prefix of columns found.
composite_indexes(Indexes, FieldRanges) ->
    lists:foldl(fun(Idx, Acc) ->
        Cols = mango_index:columns(Idx),
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


spawn_worker(#st{index=Idx}=St) ->
    Mod = mango_index:cursor_mod(Idx),
    Mod:init(Idx, St#st.ranges, St#st.selector, St#st.opts, St#st.ctx).
