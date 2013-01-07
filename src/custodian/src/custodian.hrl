% Copyright 2013 Cloudant. All rights reserved.

-define(CUSTODIAN_MAP1, <<"
  function(doc) {
    var i;
    if (!doc.by_range) return;
    for (i in doc.by_range) {
      if (doc.by_range[i].length < 3) {
        emit([doc._id, i], null);
      }
    }
  }
">>).

-define(CUSTODIAN_MAP2, <<"
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
