-module(couch_cost).

-export([
    inc_doc/0, inc_doc/1, inc_doc/2,
    inc_ioq/0, inc_ioq/1, inc_ioq/2,
    inc_get_node/1
    %%io_bytes_read/1, io_bytes_read/2,
    %%io_bytes_written/1, io_bytes_written/2,
    %%inc_js_evals/0, inc_js_evals/1, inc_js_evals/2
]).

-export([
    get_cost/0
]).

-record(cost, {
    docs_read = 0,
    ioq_calls = 0,
    io_bytes_read = 0,
    io_bytes_written = 0,
    js_evals = 0,
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

update_cost(#cost{}=Cost) ->
    put(cost_accounting_context, Cost).

inc_doc() -> inc_doc(1).
inc_doc(N) -> inc_doc(N, get_cost()).
inc_doc(N, #cost{docs_read=DR0}=Cost) -> update_cost(Cost#cost{docs_read=DR0+N}).

inc_ioq() -> inc_ioq(1).
inc_ioq(N) -> inc_ioq(N, get_cost()).
inc_ioq(N, #cost{ioq_calls=IOQ0}=Cost) -> update_cost(Cost#cost{ioq_calls=IOQ0+N}).


inc_get_node(Type) when Type =:= kp_node orelse Type =:= kv_node ->
%%inc_get_node(Type) when Type =:= kp_node; Type =:= kv_node ->
    update_cost(inc(Type, get_cost())).


inc(Key, Cost) ->
    inc(Key, Cost, 1).


inc(kp_node, #cost{get_kp_node=GKP}=Cost, N) ->
    Cost#cost{get_kp_node = GKP + N};
inc(kv_node, #cost{get_kv_node=GKV}=Cost, N) ->
    Cost#cost{get_kp_node = GKV + N}.
