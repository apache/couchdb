-include_lib("eunit/include/eunit.hrl").
-include_lib("mem3/include/mem3.hrl").

-record(collector, {
    query_args,
    callback,
    counters,
    buffer_size,
    blocked = [],
    total_rows = 0,
    offset = 0,
    rows = [],
    skip,
    limit,
    keys,
    os_proc,
    reducer,
    lang,
    sorted,
    user_acc
}).

-record(view_row, {key, id, value, doc, worker}).
