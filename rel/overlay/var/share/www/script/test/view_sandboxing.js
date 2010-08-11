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

couchTests.view_sandboxing = function(debug) {
    var db = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"false"});
    db.deleteDb();
    db.createDb();
    if (debug) debugger;

    var doc = {integer: 1, string: "1", array: [1, 2, 3]};
    T(db.save(doc).ok);
/*
    // make sure that attempting to change the document throws an error
    var results = db.query(function(doc) {
      doc.integer = 2;
      emit(null, doc);
    });
    T(results.total_rows == 0);

    var results = db.query(function(doc) {
      doc.array[0] = 0;
      emit(null, doc);
    });
    T(results.total_rows == 0);
*/
    // make sure that a view cannot invoke interpreter internals such as the
    // garbage collector
    var results = db.query(function(doc) {
      gc();
      emit(null, doc);
    });
    T(results.total_rows == 0);

    // make sure that a view cannot access the map_funs array defined used by
    // the view server
    var results = db.query(function(doc) { map_funs.push(1); emit(null, doc) });
    T(results.total_rows == 0);

    // make sure that a view cannot access the map_results array defined used by
    // the view server
    var results = db.query(function(doc) { map_results.push(1); emit(null, doc) });
    T(results.total_rows == 0);
};
