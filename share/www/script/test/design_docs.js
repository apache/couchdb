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

couchTests.design_docs = function(debug) {
  var db = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"false"});
  db.deleteDb();
  db.createDb();
  if (debug) debugger;

  run_on_modified_server(
    [{section: "query_server_config",
      key: "reduce_limit",
      value: "false"}],
function() {

  var numDocs = 500;

  function makebigstring(power) {
    var str = "a";
    while(power-- > 0) {
      str = str + str;
    }
    return str;
  }

  var designDoc = {
    _id:"_design/test", // turn off couch.js id escaping?
    language: "javascript",
    views: {
      all_docs_twice: {map: "function(doc) { emit(doc.integer, null); emit(doc.integer, null) }"},
      no_docs: {map: "function(doc) {}"},
      single_doc: {map: "function(doc) { if (doc._id == \"1\") { emit(1, null) }}"},
      summate: {map:"function (doc) {emit(doc.integer, doc.integer)};",
                reduce:"function (keys, values) { return sum(values); };"},
      summate2: {map:"function (doc) {emit(doc.integer, doc.integer)};",
                reduce:"function (keys, values) { return sum(values); };"},
      huge_src_and_results: {map: "function(doc) { if (doc._id == \"1\") { emit(\"" + makebigstring(16) + "\", null) }}",
                reduce:"function (keys, values) { return \"" + makebigstring(16) + "\"; };"}
    }
  }
  T(db.save(designDoc).ok);

  // test that we get design doc info back
  var dinfo = db.designInfo("_design/test");
  TEquals("test", dinfo.name);
  var vinfo = dinfo.view_index;
  TEquals(51, vinfo.disk_size);
  TEquals(false, vinfo.compact_running);
  TEquals("3f88e53b303e2342e49a66c538c30679", vinfo.signature);

  db.bulkSave(makeDocs(1, numDocs + 1));

  // test that the _all_docs view returns correctly with keys
  var results = db.allDocs({startkey:"_design", endkey:"_design0"});
  T(results.rows.length == 1);

  for (var loop = 0; loop < 2; loop++) {
    var rows = db.view("test/all_docs_twice").rows;
    for (var i = 0; i < numDocs; i++) {
      T(rows[2*i].key == i+1);
      T(rows[(2*i)+1].key == i+1);
    }
    T(db.view("test/no_docs").total_rows == 0)
    T(db.view("test/single_doc").total_rows == 1)
    T(db.ensureFullCommit().ok);
    restartServer();
  };

  // test when language not specified, Javascript is implied
  var designDoc2 = {
    _id:"_design/test2",
    // language: "javascript",
    views: {
      single_doc: {map: "function(doc) { if (doc._id == \"1\") { emit(1, null) }}"}
    }
  };

  T(db.save(designDoc2).ok);
  T(db.view("test2/single_doc").total_rows == 1);

  var summate = function(N) {return (N+1)*N/2;};
  var result = db.view("test/summate");
  T(result.rows[0].value == summate(numDocs));

  result = db.view("test/summate", {startkey:4,endkey:4});
  T(result.rows[0].value == 4);

  result = db.view("test/summate", {startkey:4,endkey:5});
  T(result.rows[0].value == 9);

  result = db.view("test/summate", {startkey:4,endkey:6});
  T(result.rows[0].value == 15);

  // Verify that a shared index (view def is an exact copy of "summate")
  // does not confuse the reduce stage
  result = db.view("test/summate2", {startkey:4,endkey:6});
  T(result.rows[0].value == 15);

  for(var i=1; i<numDocs/2; i+=30) {
    result = db.view("test/summate", {startkey:i,endkey:numDocs-i});
    T(result.rows[0].value == summate(numDocs-i) - summate(i-1));
  }

  T(db.deleteDoc(designDoc).ok);
  T(db.open(designDoc._id) == null);
  T(db.view("test/no_docs") == null);

  T(db.ensureFullCommit().ok);
  restartServer();
  T(db.open(designDoc._id) == null);
  T(db.view("test/no_docs") == null);

  // trigger ddoc cleanup
  T(db.viewCleanup().ok);

});
};
