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

couchTests.recreate_doc = function(debug) {
  var db = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"false"});
  db.deleteDb();
  db.createDb();
  if (debug) debugger;

  // First create a new document with the ID "foo", and delete it again
  var doc = {_id: "foo", a: "bar", b: 42};
  T(db.save(doc).ok);
  T(db.deleteDoc(doc).ok);

  // Now create a new document with the same ID, save it, and then modify it
  // This should work fine, but currently results in a conflict error, at
  // least "sometimes"
  for (var i = 0; i < 10; i++) {
    doc = {_id: "foo"};
    T(db.save(doc).ok);
    doc = db.open("foo");
    doc.a = "baz";
    T(db.save(doc).ok);
    T(db.deleteDoc(doc).rev != undefined);
  }
};
