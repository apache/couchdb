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
  var result = db.save(doc);
  T(result.ok);
  var firstRev = result.rev;
  T(db.deleteDoc(doc).ok);

  // Now create a new document with the same ID, save it, and then modify it
  for (var i = 0; i < 10; i++) {
    doc = {_id: "foo"};
    T(db.save(doc).ok);
    doc = db.open("foo");
    doc.a = "baz";
    T(db.save(doc).ok);
    T(db.deleteDoc(doc).rev != undefined);
  }

  try {
    // COUCHDB-292 now attempt to save the document with a prev that's since
    // been deleted and this should generate a conflict exception
    db.save({_id:"foo", _rev:firstRev, bar:1});
    T("no save conflict 1" && false); // we shouldn't hit here
  } catch (e) {
    T(e.error == "conflict");
  }
  
  var binAttDoc = {
    _id: "foo",
    _rev:firstRev,
    _attachments:{
      "foo.txt": {
        content_type:"text/plain",
        data: "VGhpcyBpcyBhIGJhc2U2NCBlbmNvZGVkIHRleHQ="
      }
    }
  };
  try {
    // same as before, but with binary
    db.save(binAttDoc);
    T("no save conflict 2" && false); // we shouldn't hit here
  } catch (e) {
    T(e.error == "conflict");
  }


  try {
    // random non-existant prev rev
    db.save({_id:"foo", _rev:"1-asfafasdf", bar:1});
    T("no save conflict 3" && false); // we shouldn't hit here
  } catch (e) {
    T(e.error == "conflict");
  }
  
  try {
    // random non-existant prev rev with bin
    binAttDoc._rev = "1-aasasfasdf";
    db.save(binAttDoc);
    T("no save conflict 4" && false); // we shouldn't hit here
  } catch (e) {
    T(e.error == "conflict");
  }

  db.deleteDb();
  db.createDb();

  // COUCHDB-1265
  // Resuscitate an unavailable old revision and make sure that it
  // doesn't introduce duplicates into the _changes feed.
  
  var doc = {_id: "bar", count: 0};
  T(db.save(doc).ok);
  var ghost = {_id: "bar", _rev: doc._rev, count: doc.count};
  for(var i = 0; i < 2; i++) {
    doc.count += 1;
    T(db.save(doc).ok);
  }

  // Compact so that the old revision to be resuscitated will be
  // in the rev_tree as ?REV_MISSING
  db.compact();
  while(db.info().compact_running) {}

  // Saving the ghost here puts it back in the rev_tree in such
  // a way as to create a new update_seq but without changing a
  // leaf revision. This would cause the #full_doc_info{} and
  // #doc_info{} records to diverge in their idea of what the
  // doc's update_seq is and end up introducing a duplicate in
  // the _changes feed the next time this doc is updated.
  T(db.save(ghost, {new_edits: false}).ok);

  // The duplicate would have been introduce here becuase the #doc_info{}
  // would not have been removed correctly.
  T(db.save(doc).ok);

  // And finally assert that there are no duplicates in _changes.
  var req = CouchDB.request("GET", "/test_suite_db/_changes");
  var resp = JSON.parse(req.responseText);
  var docids = {};
  for(var i = 0; i < resp.results.length; i++) {
    T(docids[resp.results[i].id] === undefined, "Duplicates in _changes feed.");
    docids[resp.results[i].id] = true;
  }
};
