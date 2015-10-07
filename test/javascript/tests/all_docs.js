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
  var db_name = get_random_db_name();
  var db = new CouchDB(db_name, {"X-Couch-Full-Commit":"false"}, {w: 3});
  db.createDb();
  if (debug) debugger;

  // Create some more documents.
  // Notice the use of the ok member on the return result.

  var doc1 = db.save({_id:"0",a:1,b:1});
  var doc2 = db.save({_id:"3",a:4,b:16});
  var doc3 = db.save({_id:"1",a:2,b:4});
  var doc4 = db.save({_id:"2",a:3,b:9});

  T(doc1.ok);
  T(doc2.ok);
  T(doc3.ok);
  T(doc4.ok);

  var revs = [];
  revs.push(doc1.rev);
  revs.push(doc2.rev);
  revs.push(doc3.rev);
  revs.push(doc4.rev);

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


  // check that all docs show up in the changes feed
  // the order can vary
  var changes = db.changes();
  changes.results.forEach(function(row, idx) {
    var rev = row.changes[0].rev;
    TEquals(true, revs.indexOf(rev) !== -1, "doc " + i + " should be in changes");
  });

  // check that deletions also show up right
  var doc1 = db.open("1");
  var deleted = db.deleteDoc(doc1);
  T(deleted.ok);
  changes = db.changes();
  T(changes.results.length == 4);
  var deleted_doc = changes.results.filter(function(row) {
    return row.deleted == true;
  })[0];
  TEquals("1", deleted_doc.id, "deletes");

  // do an update
  var doc3 = db.open("3");
  doc3.updated = "totally";
  doc3 = db.save(doc3);
  changes = db.changes();

  // the update should make doc id 3 have the last seq num
  T(changes.results.length == 4);
  var updated_doc = changes.results.filter(function(row) {
    return row.id == doc3.id
  })[0];
  TEquals("3", updated_doc.id, "doc id should be 3");

  // ok now lets see what happens with include docs
  changes = db.changes({include_docs: true});
  T(changes.results.length == 4);

  var updated_doc = changes.results.filter(function(row) {
    return row.id == doc3.id
  })[0];
  T(updated_doc.doc.updated == "totally");

  var deleted_doc = changes.results.filter(function(row) {
    return row.deleted == true;
  })[0];
  TEquals(true, deleted_doc.doc._deleted, "deletes");

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

  var doc3 = changes.results.filter(function(row) {
    return row.id == "3";
  })[0];

  TEquals("3", doc3.id);
  TEquals(3, doc3.changes.length);
  TEquals(winRev._rev, doc3.changes[0].rev);
  TEquals("3", doc3.doc._id);
  TEquals(winRev._rev, doc3.doc._rev);
  TEquals(true, doc3.doc._conflicts instanceof Array);
  TEquals(2, doc3.doc._conflicts.length);

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
