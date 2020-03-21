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
couchTests.elixir = true;
couchTests.design_docs_query = function(debug) {
  return console.log('done in test/elixir/test/design_docs_query_test.exs');

  var db_name = get_random_db_name();
  var db = new CouchDB(db_name, {"X-Couch-Full-Commit":"false"});
  db.createDb();
  if (debug) debugger;

  var docs = makeDocs(5);

  // create the docs
  var results = db.bulkSave(docs);
  T(results.length == 5);
  for (var i = 0; i < 5; i++) {
    T(results[i].id == docs[i]._id);
  }

  // create the ddocs
  for (var i = 0; i < 5; i++) {
    T(db.save({
      _id : "_design/ddoc0" + (i+1).toString(),
      views : {
        "testing" : {
          "map" : "function(){emit(1,1)}"
        }
      }
    }).ok);
  }

  // test design_docs
  var path = "/" + db_name + "/_design_docs?";
  var xhr_AllDDocs = CouchDB.request("GET", path);
  T(xhr_AllDDocs.status == 200, "standard get should be 200");
  var allDDocs = JSON.parse(xhr_AllDDocs.responseText);
  TEquals(5, allDDocs.total_rows, "total_rows mismatch");
  TEquals(5, allDDocs.rows.length, "amount of rows mismatch");

  // test key="_design/ddoc03"
  var xhr = CouchDB.request("GET", path + "key=\"_design/ddoc03\"");
  T(xhr.status = 200, "standard get should be 200");
  var result = JSON.parse(xhr.responseText);
  TEquals(1, result.rows.length, "amount of rows mismatch");
  TEquals("_design/ddoc03", result.rows[0].key, "key test");

  // test descending=true
  var xhr = CouchDB.request("GET", path + "descending=true");
  T(xhr.status == 200, "standard get should be 200");
  var result = JSON.parse(xhr.responseText);
  TEquals(5, result.rows.length, "amount of rows mismatch");
  TEquals("_design/ddoc05", result.rows[0].key, "descending test");

  // test descending=false
  var xhr = CouchDB.request("GET", path + "descending=false");
  T(xhr.status == 200, "standard get should be 200");
  var result = JSON.parse(xhr.responseText);
  TEquals(5, result.rows.length, "amount of rows mismatch");
  TEquals("_design/ddoc01", result.rows[0].key, "descending test");

  // test end_key="_design/ddoc03"
  var xhr = CouchDB.request("GET", path + "end_key=\"_design/ddoc03\"");
  T(xhr.status = 200, "standard get should be 200");
  var result = JSON.parse(xhr.responseText);
  TEquals(3, result.rows.length, "amount of rows mismatch");
  TEquals("_design/ddoc03", result.rows[2].key, "end_key test");

  // test endkey="_design/ddoc03"
  var xhr = CouchDB.request("GET", path + "endkey=\"_design/ddoc03\"");
  T(xhr.status = 200, "standard get should be 200");
  var result = JSON.parse(xhr.responseText);
  TEquals(3, result.rows.length, "amount of rows mismatch");
  TEquals("_design/ddoc03", result.rows[2].key, "endkey test");

  // test start_key="_design/ddoc03"
  var xhr = CouchDB.request("GET", path + "start_key=\"_design/ddoc03\"");
  T(xhr.status = 200, "standard get should be 200");
  var result = JSON.parse(xhr.responseText);
  TEquals(3, result.rows.length, "amount of rows mismatch");
  TEquals("_design/ddoc03", result.rows[0].key, "start_key test");

  // test startkey="_design/ddoc03"
  var xhr = CouchDB.request("GET", path + "startkey=\"_design/ddoc03\"");
  T(xhr.status = 200, "standard get should be 200");
  var result = JSON.parse(xhr.responseText);
  TEquals(3, result.rows.length, "amount of rows mismatch");
  TEquals("_design/ddoc03", result.rows[0].key, "startkey test");

  // test end_key="_design/ddoc03"&inclusive_end=true
  var xhr = CouchDB.request("GET", path + "end_key=\"_design/ddoc03\"&inclusive_end=true");
  T(xhr.status = 200, "standard get should be 200");
  var result = JSON.parse(xhr.responseText);
  TEquals(3, result.rows.length, "amount of rows mismatch");
  TEquals("_design/ddoc03", result.rows[2].key, "end_key and inclusive_end test");

  // test end_key="_design/ddoc03"&inclusive_end=false
  var xhr = CouchDB.request("GET", path + "end_key=\"_design/ddoc03\"&inclusive_end=false");
  T(xhr.status = 200, "standard get should be 200");
  var result = JSON.parse(xhr.responseText);
  TEquals(2, result.rows.length, "amount of rows mismatch");
  TEquals("_design/ddoc02", result.rows[1].key, "end_key and inclusive_end test");

  // test end_key="_design/ddoc03"&inclusive_end=false&descending=true
  var xhr = CouchDB.request("GET", path +
                            "end_key=\"_design/ddoc03\"&inclusive_end=false&descending=true");
  T(xhr.status = 200, "standard get should be 200");
  var result = JSON.parse(xhr.responseText);
  TEquals(2, result.rows.length, "amount of rows mismatch");
  TEquals("_design/ddoc04", result.rows[1].key, "end_key, inclusive_end and descending test");

  // test end_key="_design/ddoc05"&limit=2
  var xhr = CouchDB.request("GET", path +
                            "end_key=\"_design/ddoc05\"&limit=2");
  T(xhr.status = 200, "standard get should be 200");
  var result = JSON.parse(xhr.responseText);
  TEquals(2, result.rows.length, "amount of rows mismatch");
  TEquals("_design/ddoc02", result.rows[1].key, "end_key and limit test");

  // test end_key="_design/ddoc05"&skip=2
  var xhr = CouchDB.request("GET", path +
                            "end_key=\"_design/ddoc05\"&skip=2");
  T(xhr.status = 200, "standard get should be 200");
  var result = JSON.parse(xhr.responseText);
  TEquals(3, result.rows.length, "amount of rows mismatch");
  TEquals("_design/ddoc03", result.rows[0].key, "end_key and skip test");
  TEquals("_design/ddoc05", result.rows[2].key, "end_key and skip test");

  // test end_key="_design/ddoc05"&update_seq=true
  var xhr = CouchDB.request("GET", path +
                            "end_key=\"_design/ddoc05\"&update_seq=true");
  T(xhr.status = 200, "standard get should be 200");
  var result = JSON.parse(xhr.responseText);
  T(result.update_seq);

  // test POST with keys
  var xhr = CouchDB.request("POST", path, {
    headers: {"Content-Type": "application/json"},
    body: JSON.stringify({"keys" : ["_design/ddoc02", "_design/ddoc03"]})
  });
  T(xhr.status = 200, "standard get should be 200");
  var result = JSON.parse(xhr.responseText);
  TEquals(2, result.rows.length, "amount of rows mismatch");
  TEquals("_design/ddoc03", result.rows[1].key, "POST test");

  db.deleteDb();
};
