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

couchTests.all_docs = function(debug) {
  var db = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"false"});
  db.deleteDb();
  db.createDb();
  if (debug) debugger;

  // Create some more documents.
  // Notice the use of the ok member on the return result.
  T(db.save({_id:"0",a:1,b:1}).ok);
  T(db.save({_id:"3",a:4,b:16}).ok);
  T(db.save({_id:"1",a:2,b:4}).ok);
  T(db.save({_id:"2",a:3,b:9}).ok);

  // Check the all docs
  var results = db.allDocs();
  var rows = results.rows;

  T(results.total_rows == results.rows.length);

  for(var i=0; i < rows.length; i++) {
    T(rows[i].id >= "0" && rows[i].id <= "4");
  }

  // Check _all_docs with descending=true
  var desc = db.allDocs({descending:true});
  T(desc.total_rows == desc.rows.length);

  // Check _all_docs offset
  var all = db.allDocs({startkey:"2"});
  T(all.offset == 2);

  // Confirm that queries may assume raw collation.
  var raw = db.allDocs({ startkey: "org.couchdb.user:",
                         endkey  : "org.couchdb.user;"
                       });
  TEquals(0, raw.rows.length);

  // check that the docs show up in the seq view in the order they were created
  var changes = db.changes();
  var ids = ["0","3","1","2"];
  for (var i=0; i < changes.results.length; i++) {
    var row = changes.results[i];
    T(row.id == ids[i], "seq order");
  };

  // it should work in reverse as well
  changes = db.changes({descending:true});
  ids = ["2","1","3","0"];
  for (var i=0; i < changes.results.length; i++) {
    var row = changes.results[i];
    T(row.id == ids[i], "descending=true");
  };

  // check that deletions also show up right
  var doc1 = db.open("1");
  var deleted = db.deleteDoc(doc1);
  T(deleted.ok);
  changes = db.changes();
  // the deletion should make doc id 1 have the last seq num
  T(changes.results.length == 4);
  T(changes.results[3].id == "1");
  T(changes.results[3].deleted);

  // do an update
  var doc2 = db.open("3");
  doc2.updated = "totally";
  db.save(doc2);
  changes = db.changes();

  // the update should make doc id 3 have the last seq num
  T(changes.results.length == 4);
  T(changes.results[3].id == "3");

  // ok now lets see what happens with include docs
  changes = db.changes({include_docs: true});
  T(changes.results.length == 4);
  T(changes.results[3].id == "3");
  T(changes.results[3].doc.updated == "totally");

  T(changes.results[2].doc);
  T(changes.results[2].doc._deleted);

  rows = db.allDocs({include_docs: true}, ["1"]).rows;
  TEquals(1, rows.length);
  TEquals("1", rows[0].key);
  TEquals("1", rows[0].id);
  TEquals(true, rows[0].value.deleted);
  TEquals(null, rows[0].doc);

  // add conflicts
  var conflictDoc1 = {
    _id: "3", _rev: "2-aa01552213fafa022e6167113ed01087", value: "X"
  };
  var conflictDoc2 = {
    _id: "3", _rev: "2-ff01552213fafa022e6167113ed01087", value: "Z"
  };
  T(db.save(conflictDoc1, {new_edits: false}));
  T(db.save(conflictDoc2, {new_edits: false}));

  var winRev = db.open("3");

  changes = db.changes({include_docs: true, conflicts: true, style: "all_docs"});
  TEquals("3", changes.results[3].id);
  TEquals(3, changes.results[3].changes.length);
  TEquals(winRev._rev, changes.results[3].changes[0].rev);
  TEquals("3", changes.results[3].doc._id);
  TEquals(winRev._rev, changes.results[3].doc._rev);
  TEquals(true, changes.results[3].doc._conflicts instanceof Array);
  TEquals(2, changes.results[3].doc._conflicts.length);

  rows = db.allDocs({include_docs: true, conflicts: true}).rows;
  TEquals(3, rows.length);
  TEquals("3", rows[2].key);
  TEquals("3", rows[2].id);
  TEquals(winRev._rev, rows[2].value.rev);
  TEquals(winRev._rev, rows[2].doc._rev);
  TEquals("3", rows[2].doc._id);
  TEquals(true, rows[2].doc._conflicts instanceof Array);
  TEquals(2, rows[2].doc._conflicts.length);

  // test the all docs collates sanely
  db.save({_id: "Z", foo: "Z"});
  db.save({_id: "a", foo: "a"});

  var rows = db.allDocs({startkey: "Z", endkey: "Z"}).rows;
  T(rows.length == 1);

  // cleanup
  db.deleteDb();
};
