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

couchTests.copy_doc = function(debug) {
  var db = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"false"});
  db.deleteDb();
  db.createDb();
  if (debug) debugger;

  // copy a doc
  var ok = db.save({_id:"doc_to_be_copied",v:1}).ok;
  TEquals(true, ok, "Should return ok:true");
  var xhr = CouchDB.request("COPY", "/test_suite_db/doc_to_be_copied", {
    headers: {"Destination":"doc_that_was_copied"}
  });

  TEquals(true, JSON.parse(xhr.responseText).ok, "Should return ok:true");

  TEquals(201, xhr.status, "Should return 201 status");
  TEquals(1, db.open("doc_that_was_copied").v, "Should have value 1");

  // COPY with existing target
  var ok = db.save({_id:"doc_to_be_copied2",v:1}).ok;
  TEquals(true, ok, "Should return ok:true");
  var doc = db.save({_id:"doc_to_be_overwritten",v:2});
  TEquals(true, doc.ok, "Should return ok:true");

  // error condition
  var xhr = CouchDB.request("COPY", "/test_suite_db/doc_to_be_copied2", {
      headers: {"Destination":"doc_to_be_overwritten"}
  });
  TEquals(409, xhr.status, "Should return 409 status"); // conflict

  var xhr = CouchDB.request("COPY", "/test_suite_db/doc_to_be_copied2");
  TEquals(400, xhr.status, "Should return 400 status");
  TEquals("Destination header is mandatory for COPY.", JSON.parse(xhr.responseText).reason,
    "Should report missing destination header");

  var xhr = CouchDB.request("COPY", "/test_suite_db/doc_to_be_copied2", {
    headers: {
      "Destination": "http://localhost:5984/test_suite_db/doc_to_be_written"
  }});
  TEquals(400, xhr.status, "Should return 400 status");
  TEquals("Destination URL must be relative.", JSON.parse(xhr.responseText).reason,
    "Should report invalid destination header");

  var rev = db.open("doc_to_be_overwritten")._rev;
  var xhr = CouchDB.request("COPY", "/test_suite_db/doc_to_be_copied2", {
    headers: {"Destination":"doc_to_be_overwritten?rev=" + rev}
  });
  TEquals(201, xhr.status, "Should return 201 status");

  var over = db.open("doc_to_be_overwritten");
  T(rev != over._rev);
  TEquals(1, over.v, "Should be value 1");
};
