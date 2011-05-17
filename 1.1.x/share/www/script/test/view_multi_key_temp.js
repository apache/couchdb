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

couchTests.view_multi_key_temp = function(debug) {
  var db = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"false"});
  db.deleteDb();
  db.createDb();
  if (debug) debugger;

  var docs = makeDocs(0, 100);
  db.bulkSave(docs);

  var queryFun = function(doc) { emit(doc.integer, doc.integer) };
  var reduceFun = function (keys, values) { return sum(values); };

  var keys = [10,15,30,37,50];
  var rows = db.query(queryFun, null, {}, keys).rows;
  for(var i=0; i<rows.length; i++) {
    T(keys.indexOf(rows[i].key) != -1);
    T(rows[i].key == rows[i].value);
  }

  var reduce = db.query(queryFun, reduceFun, {group:true}, keys).rows;
  for(var i=0; i<reduce.length; i++) {
    T(keys.indexOf(reduce[i].key) != -1);
    T(reduce[i].key == reduce[i].value);
  }
};
