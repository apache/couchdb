% Copyright 2013 Cloudant. All rights reserved.

-define(CUSTODIAN_UNDER_N(N),
<<"function(doc) {
  var i;
  if (!doc.by_range) return;
  for (i in doc.by_range) {
    if (doc.by_range[i].length < ", N/binary, ") {
      emit([doc._id, i], 1);
    }
  }
}
">>).

-define(CUSTODIAN_BY_NODE_RANGE,
<<"function(doc) {
  var i, j;
  if (!doc.by_range) return;
  for (i in doc.by_range) {
    for (j in doc.by_range[i]) {
      emit([doc.by_range[i][j], doc._id, i], 1);
    }
  }
}
">>).

-define(CUSTODIAN_VALIDATION,
<<"function(newDoc, oldDoc) {
  var i, range, node;
  if(newDoc['_id'].substring(0, 8) === \"_design/\") return;
  if (!newDoc.by_node) {
    throw({forbidden: \"by_node is mandatory\"});
  }
  if (!newDoc.by_range) {
    throw({forbidden: \"by_range is mandatory\"});
  }
  for (node in newDoc.by_node) {
    for (i in newDoc.by_node[node]) {
      range = newDoc.by_node[node][i];
      if(!newDoc.by_range[range]) {
        throw({forbidden: \"by_range for \" + range + \" is missing\"});
      }
      if(newDoc.by_range[range].indexOf(node) === -1) {
        throw({forbidden : \"by_range for \" + range + \" is missing \" + node});
      }
    }
  }
  for (range in newDoc.by_range) {
    for (i in newDoc.by_range[range]) {
      node = newDoc.by_range[range][i];
      if(!newDoc.by_node[node]) {
        throw({forbidden: \"by_node for \" + node + \" is missing\"});
      }
      if (newDoc.by_node[node].indexOf(range) === -1) {
        throw({forbidden: \"by_node for \" + node + \" is missing \" + range});
      }
    }
  }
}
">>).
