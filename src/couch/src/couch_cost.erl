-module(couch_cost).

-export([
    inc_doc/0, inc_doc/1, inc_doc/2,
    inc_ioq/0, inc_ioq/1, inc_ioq/2,
    inc_get_node/1,
    inc_db_open/0,
    inc_js_filter/0, inc_js_filter/1, inc_js_filter/2,
    inc_js_filtered_docs/1, inc_js_filtered_docs/2,
    inc_changes_processed/0, inc_changes_processed/1, inc_changes_processed/2
    %%io_bytes_read/1, io_bytes_read/2,
    %%io_bytes_written/1, io_bytes_written/2,
    %%inc_js_evals/0, inc_js_evals/1, inc_js_evals/2
]).

-export([
    get_cost/0,
    get_costs/0,
    accumulate_costs/1
]).


-record(cost, {
    db_open = 0,
    docs_read = 0,
    changes_processed = 0,
    ioq_calls = 0,
    io_bytes_read = 0,
    io_bytes_written = 0,
    js_evals = 0,
    js_filter = 0,
    js_filtered_docs = 0,
    get_kv_node = 0,
    get_kp_node = 0
}).


get_cost() ->
    case get(cost_accounting_context) of
        undefined ->
            Cost = #cost{},
            update_cost(Cost),
            Cost;
        #cost{}=Cost ->
            Cost
    end.


get_costs() ->
    case get(cost_accounting_context_aggregation) of
        undefined ->
            Costs = [],
            put(cost_accounting_context_aggregation, Costs),
            Costs;
        Costs when is_list(Costs) ->
            Costs
    end.


update_cost(#cost{}=Cost) ->
    put(cost_accounting_context, Cost).


accumulate_costs(#cost{}=Cost) ->
    Costs = get_costs(),
    put(cost_accounting_context_aggregation, [Cost | Costs]).


inc_doc() -> inc_doc(1).
inc_doc(N) -> inc_doc(N, get_cost()).
inc_doc(N, #cost{docs_read=DR0}=Cost) -> update_cost(Cost#cost{docs_read=DR0+N}).


inc_ioq() -> inc_ioq(1).
inc_ioq(N) -> inc_ioq(N, get_cost()).
inc_ioq(N, #cost{ioq_calls=IOQ0}=Cost) -> update_cost(Cost#cost{ioq_calls=IOQ0+N}).


inc_get_node(Type) when Type =:= kp_node orelse Type =:= kv_node ->
%%inc_get_node(Type) when Type =:= kp_node; Type =:= kv_node ->
    update_cost(inc(Type, get_cost())).


inc_js_filter() -> inc_js_filter(1).
inc_js_filter(N) -> inc_js_filter(N, get_cost()).
inc_js_filter(N, #cost{}=Cost) -> update_cost(inc(js_filter, Cost, N)).


inc_js_filtered_docs(N) -> inc_js_filtered_docs(N, get_cost()).
inc_js_filtered_docs(N, #cost{}=Cost) -> update_cost(inc(js_filtered_docs, Cost, N)).


inc_changes_processed() -> inc_changes_processed(1).
inc_changes_processed(N) -> inc_changes_processed(N, get_cost()).
inc_changes_processed(N, #cost{}=Cost) -> update_cost(inc(changes_processed, Cost, N)).


inc_db_open() -> inc_db_open(1).
inc_db_open(N) -> inc_db_open(N, get_cost()).
inc_db_open(N, #cost{}=Cost) -> update_cost(inc(db_open, Cost, N)).


inc(Key, Cost) ->
    inc(Key, Cost, 1).


inc(kp_node, #cost{get_kp_node=GKP}=Cost, N) ->
    Cost#cost{get_kp_node = GKP + N};
inc(kv_node, #cost{get_kv_node=GKV}=Cost, N) ->
    Cost#cost{get_kv_node = GKV + N};
inc(changes_processed, #cost{changes_processed=CP}=Cost, N) ->
    Cost#cost{changes_processed = CP + N};
inc(db_open, #cost{db_open=DBO}=Cost, N) ->
    Cost#cost{db_open = DBO + N};
inc(js_filter, #cost{js_filter=JSF}=Cost, N) ->
    Cost#cost{js_filter = JSF + N};
inc(js_filtered_docs, #cost{js_filtered_docs=JSFD}=Cost, N) ->
    Cost#cost{js_filtered_docs = JSFD + N}.
