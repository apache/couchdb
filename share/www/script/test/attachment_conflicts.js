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

// Do some edit conflict detection tests for attachments.
couchTests.attachment_conflicts = function(debug) {

  var dbA = new CouchDB("test_suite_db_a", {"X-Couch-Full-Commit":"false"});
  var dbB = new CouchDB("test_suite_db_b", {"X-Couch-Full-Commit":"false"});
  dbA.deleteDb();
  dbA.createDb();
  dbB.deleteDb();
  dbB.createDb();

  if (debug) debugger;

  T(dbA.save({"_id":"doc", "foo":"bar"}).ok);

  // create conflict
  T(CouchDB.replicate("test_suite_db_a", "test_suite_db_b").ok);

  var doc = dbA.open("doc");
  var rev11 = doc._rev;
    T(dbA.save({"_id":"doc", "foo":"bar2","_rev":rev11}).ok);

  doc = dbB.open("doc");
  var rev12 = doc._rev;
    T(dbB.save({"_id":"doc", "foo":"bar3","_rev":rev12}).ok);

  T(CouchDB.replicate("test_suite_db_a", "test_suite_db_b").ok);

  // the attachment
  var bin_data = "JHAPDO*AU£PN ){(3u[d 93DQ9¡€])}    ææøo'∂ƒæ≤çæππ•¥∫¶®#†π¶®¥π€ª®˙π8np";

  doc = dbB.open("doc");
  var rev13 = doc._rev;

  // test that we can can attach to conflicting documents
  var xhr = CouchDB.request("PUT", "/test_suite_db_b/doc/attachment.txt", {
    headers: {
      "Content-Type": "text/plain;charset=utf-8",
      "If-Match": rev13
    },
    body: bin_data
  });
  T(xhr.status == 201);

};
