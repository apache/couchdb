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

  // Helper function to create a doc with multiple revisions
  // that are compacted away to ?REV_MISSING.

  var createDoc = function(docid) {
    var ret = [{_id: docid, count: 0}];
    T(db.save(ret[0]).ok);
    for(var i = 0; i < 2; i++) {
      ret[ret.length] = {
        _id: docid,
        _rev: ret[ret.length-1]._rev,
        count: ret[ret.length-1].count+1
      };
      T(db.save(ret[ret.length-1]).ok);
    }
    db.compact();
    while(db.info().compact_running) {}
    return ret;
  }

  // Helper function to check that there are no duplicates
  // in the changes feed and that it has proper update
  // sequence ordering.

  var checkChanges = function() {
    // Assert that there are no duplicates in _changes.
    var req = CouchDB.request("GET", "/test_suite_db/_changes");
    var resp = JSON.parse(req.responseText);
    var docids = {};
    var prev_seq = -1;
    for(var i = 0; i < resp.results.length; i++) {
      row = resp.results[i];
      T(row.seq > prev_seq, "Unordered _changes feed.");
      T(docids[row.id] === undefined, "Duplicates in _changes feed.");
      prev_seq = row.seq;
      docids[row.id] = true;
    }
  };

  // COUCHDB-1265 - Check that the changes feed remains proper
  // after we try and break the update_seq tree.

  // This first case is the one originally reported and "fixed"
  // in COUCHDB-1265. Reinserting an old revision into the
  // revision tree causes duplicates in the update_seq tree.

  var revs = createDoc("a");
  T(db.save(revs[1], {new_edits: false}).ok);
  T(db.save(revs[revs.length-1]).ok);
  checkChanges();

  // The original fix for COUCHDB-1265 is not entirely correct
  // as it didn't consider the possibility that a compaction
  // might run after the original tree screw up.

  revs = createDoc("b");
  T(db.save(revs[1], {new_edits: false}).ok);
  db.compact();
  while(db.info().compact_running) {}
  T(db.save(revs[revs.length-1]).ok);
  checkChanges();

};
