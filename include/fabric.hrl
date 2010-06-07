-define(FABRIC, true).

-ifndef(COUCH).
-include("../../couch/src/couch_db.hrl").
-endif.

-ifndef(MEMBERSHIP).
-include("../../membership/include/membership.hrl").
-endif.

-include_lib("eunit/include/eunit.hrl").

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
    stop_fun,
    keydict,
    user_acc
}).

-record(view_row, {key, id, value, doc, worker}).
