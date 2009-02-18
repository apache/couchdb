// Licensed under the Apache License, Version 2.0 (the "License"); you may not
// use this file except in compliance with the License.  You may obtain a copy
// of the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
// WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
// License for the specific language governing permissions and limitations under
// the License.

couchTests.view_errors = function(debug) {
  var db = new CouchDB("test_suite_db");
  db.deleteDb();
  db.createDb();
  if (debug) debugger;

  var doc = {integer: 1, string: "1", array: [1, 2, 3]};
  T(db.save(doc).ok);

  // emitting a key value that is undefined should result in that row not
  // being included in the view results
  var results = db.query(function(doc) {
    emit(doc.undef, null);
  });
  T(results.total_rows == 0);

  // if a view function throws an exception, its results are not included in
  // the view index, but the view does not itself raise an error
  var results = db.query(function(doc) {
    doc.undef(); // throws an error
  });
  T(results.total_rows == 0);

  // if a view function includes an undefined value in the emitted key or
  // value, an error is logged and the result is not included in the view
  // index, and the view itself does not raise an error
  var results = db.query(function(doc) {
    emit([doc._id, doc.undef], null);
  });
  T(results.total_rows == 0);
  
  // querying a view with invalid params should give a resonable error message
  var xhr = CouchDB.request("POST", "/test_suite_db/_temp_view?startkey=foo", {
    headers: {"Content-Type": "application/json"},
    body: JSON.stringify({language: "javascript", 
      map : "function(doc){emit(doc.integer)}"
    })
  });
  T(JSON.parse(xhr.responseText).error == "invalid_json");
};
