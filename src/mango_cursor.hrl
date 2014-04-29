
-record(cursor, {
    db,
    index,
    ranges,
    selector,
    opts,
    limit = 10000000000,
    skip = 0,
    fields = undefined,
    user_fun,
    user_acc
}).