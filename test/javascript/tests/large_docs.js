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

couchTests.large_docs = function(debug) {
  var db = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"false"});
  db.deleteDb();
  db.createDb();
  if (debug) debugger;

  var longtext = "0123456789\n";

  for (var i=0; i<10; i++) {
    longtext = longtext + longtext
  }
  T(db.save({"longtest":longtext}).ok);
  T(db.save({"longtest":longtext}).ok);
  T(db.save({"longtest":longtext}).ok);
  T(db.save({"longtest":longtext}).ok);

  // query all documents, and return the doc.foo member as a key.
  results = db.query(function(doc){
      emit(null, doc.longtest);
  });
};
