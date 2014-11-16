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

  var changes = db.changes();

  T(changes.results.length == 1);

  // Now clear out the _rev member and save. This indicates this document is
  // new, not based on an existing revision.
  doc2._rev = undefined;
  try {
    db.save(doc2); // this should generate a conflict exception
    T("no save conflict 2" && false); // we shouldn't hit here
  } catch (e) {
    T(e.error == "conflict");
  }

  // Make a few bad requests, specifying conflicting revs
  // ?rev doesn't match body
  var xhr = CouchDB.request("PUT", "/test_suite_db/foo?rev=1-foobar", {
    body : JSON.stringify(doc)
  });
  T(xhr.status == 400);

  // If-Match doesn't match body
  xhr = CouchDB.request("PUT", "/test_suite_db/foo", {
    headers: {"If-Match": "1-foobar"},
    body: JSON.stringify(doc)
  });
  T(xhr.status == 400);

  // ?rev= doesn't match If-Match
  xhr = CouchDB.request("PUT", "/test_suite_db/foo?rev=1-boobaz", {
    headers: {"If-Match": "1-foobar"},
    body: JSON.stringify(doc2)
  });
  T(xhr.status == 400);

  // Now update the document using ?rev=
  xhr = CouchDB.request("PUT", "/test_suite_db/foo?rev=" + doc._rev, {
    body: JSON.stringify(doc)
  });
  T(xhr.status == 201);

  // reopen
  var doc = db.open(doc._id);

  // Now delete the document from the database
  T(db.deleteDoc(doc).ok);

  T(db.save(doc2).ok);  // we can save a new document over a deletion without
                        // knowing the deletion rev.

  // Verify COUCHDB-1178
  var r1 = {"_id":"doc","foo":"bar"};
  var r2 = {"_id":"doc","foo":"baz","_rev":"1-4c6114c65e295552ab1019e2b046b10e"};
  var r3 = {"_id":"doc","foo":"bam","_rev":"2-cfcd6781f13994bde69a1c3320bfdadb"};
  var r4 = {"_id":"doc","foo":"bat","_rev":"3-cc2f3210d779aef595cd4738be0ef8ff"};

  T(db.save({"_id":"_design/couchdb-1178","validate_doc_update":"function(){}"}).ok);
  T(db.save(r1).ok);
  T(db.save(r2).ok);
  T(db.save(r3).ok);

  T(db.compact().ok);
  while (db.info().compact_running) {};

  TEquals({"_id":"doc",
        "_rev":"3-cc2f3210d779aef595cd4738be0ef8ff",
        "foo":"bam",
        "_revisions":{"start":3,
          "ids":["cc2f3210d779aef595cd4738be0ef8ff",
                 "cfcd6781f13994bde69a1c3320bfdadb",
                                      "4c6114c65e295552ab1019e2b046b10e"]}},
    db.open("doc", {"revs": true}));
  TEquals([], db.bulkSave([r4, r3, r2], {"new_edits":false}), "no failures");

};
