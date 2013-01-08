% Copyright 2013 Cloudant. All rights reserved.

-define(CUSTODIAN_UNDER_N(N), <<"
  function(doc) {
    var i;
    if (!doc.by_range) return;
    for (i in doc.by_range) {
      if (doc.by_range[i].length < ", N/binary, ") {
        emit([doc._id, i], 1);
      }
    }
  }
">>).

-define(CUSTODIAN_BY_NODE_RANGE, <<"
  function(doc) {
    var i, j;
    if (!doc.by_range) return;
    for (i in doc.by_range) {
      for (j in doc.by_range[i]) {
        emit([doc.by_range[i][j], doc._id, i], 1);
      }
    }
  }
">>).
