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

// Do some edit conflict detection tests
couchTests.conflicts = function(debug) {
  var db = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"false"});
  db.deleteDb();
  db.createDb();
  if (debug) debugger;

  // create a doc and save
  var doc = {_id:"foo",a:1,b:1};
  T(db.save(doc).ok);

  // reopen
  var doc2 = db.open(doc._id);

  // ensure the revisions are the same
  T(doc._id == doc2._id && doc._rev == doc2._rev);

  // edit the documents.
  doc.a = 2;
  doc2.a = 3;

  // save one document
  T(db.save(doc).ok);

  // save the other document
  try {
    db.save(doc2);  // this should generate a conflict exception
    T("no save conflict 1" && false); // we shouldn't hit here
  } catch (e) {
    T(e.error == "conflict");
  }

  var bySeq = db.allDocsBySeq();

  T( bySeq.rows.length == 1)

  // Now clear out the _rev member and save. This indicates this document is
  // new, not based on an existing revision.
  doc2._rev = undefined;
  try {
    db.save(doc2); // this should generate a conflict exception
    T("no save conflict 2" && false); // we shouldn't hit here
  } catch (e) {
    T(e.error == "conflict");
  }

  // Now delete the document from the database
  T(db.deleteDoc(doc).ok);

  T(db.save(doc2).ok);  // we can save a new document over a deletion without
                        // knowing the deletion rev.
};
