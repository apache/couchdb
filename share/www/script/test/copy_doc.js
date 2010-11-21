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
  T(db.save({_id:"doc_to_be_copied",v:1}).ok);
  var xhr = CouchDB.request("COPY", "/test_suite_db/doc_to_be_copied", {
    headers: {"Destination":"doc_that_was_copied"}
  });

    T(JSON.parse(xhr.responseText).ok);

  T(xhr.status == 201);
  T(db.open("doc_that_was_copied").v == 1);

  // COPY with existing target
  T(db.save({_id:"doc_to_be_copied2",v:1}).ok);
  var doc = db.save({_id:"doc_to_be_overwritten",v:2});
  T(doc.ok);

  // error condition
  var xhr = CouchDB.request("COPY", "/test_suite_db/doc_to_be_copied2", {
      headers: {"Destination":"doc_to_be_overwritten"}
  });
  T(xhr.status == 409); // conflict

  var xhr = CouchDB.request("COPY", "/test_suite_db/doc_to_be_copied2");
  T(xhr.status == 400); // bad request (no Destination header)

  var rev = db.open("doc_to_be_overwritten")._rev;
  var xhr = CouchDB.request("COPY", "/test_suite_db/doc_to_be_copied2", {
    headers: {"Destination":"doc_to_be_overwritten?rev=" + rev}
  });
  T(xhr.status == 201);

  var over = db.open("doc_to_be_overwritten");
  T(rev != over._rev);
  T(over.v == 1);
};
