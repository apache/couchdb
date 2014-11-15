// Licensed under the Apache License, Version 2.0 (the "License"); you may not
// use this file except in compliance with the License. You may obtain a copy of
// the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
// WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
// License for the specific language governing permissions and limitations under
// the License.

couchTests.reduce_false_temp = function(debug) {
  var db = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"false"});
  db.deleteDb();
  db.createDb();
  if (debug) debugger;

  var numDocs = 5;
  var docs = makeDocs(1,numDocs + 1);
  db.bulkSave(docs);
  var summate = function(N) {return (N+1)*N/2;};

  var mapFun = "function (doc) { emit(doc.integer, doc.integer); }";
  var reduceFun = "function (keys, values) { return sum(values); }";

  // Test that the reduce works
  var res = db.query(mapFun, reduceFun);
  T(res.rows.length == 1 && res.rows[0].value == summate(5));

  //Test that we get our docs back
  res = db.query(mapFun, reduceFun, {reduce: false});
  T(res.rows.length == 5);
  for(var i=0; i<5; i++) {
    T(res.rows[i].value == i+1);
  }
};
