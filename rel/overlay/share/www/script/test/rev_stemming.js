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

couchTests.rev_stemming = function(debug) {
  var db = new CouchDB("test_suite_db_a", {"X-Couch-Full-Commit":"false"});
  db.deleteDb();
  db.createDb();
  var dbB = new CouchDB("test_suite_db_b", {"X-Couch-Full-Commit":"false"});
  dbB.deleteDb();
  dbB.createDb();
  if (debug) debugger;

  var newLimit = 5;

  T(db.getDbProperty("_revs_limit") == 1000);

  var doc = {_id:"foo",foo:0}
  for( var i=0; i < newLimit + 1; i++) {
    doc.foo++;
    T(db.save(doc).ok);
  }
  var doc0 = db.open("foo", {revs:true});
  T(doc0._revisions.ids.length == newLimit + 1);

  var docBar = {_id:"bar",foo:0}
  for( var i=0; i < newLimit + 1; i++) {
    docBar.foo++;
    T(db.save(docBar).ok);
  }
  T(db.open("bar", {revs:true})._revisions.ids.length == newLimit + 1);

  T(db.setDbProperty("_revs_limit", newLimit).ok);

  for( var i=0; i < newLimit + 1; i++) {
    doc.foo++;
    T(db.save(doc).ok);
  }
  doc0 = db.open("foo", {revs:true});
  T(doc0._revisions.ids.length == newLimit);


  // If you replicate after you make more edits than the limit, you'll
  // cause a spurious edit conflict.
  CouchDB.replicate("test_suite_db_a", "test_suite_db_b");
  var docB1 = dbB.open("foo",{conflicts:true})
  T(docB1._conflicts == null);

  for( var i=0; i < newLimit - 1; i++) {
    doc.foo++;
    T(db.save(doc).ok);
  }

  // one less edit than limit, no conflict
  CouchDB.replicate("test_suite_db_a", "test_suite_db_b");
  var docB1 = dbB.open("foo",{conflicts:true})
  T(docB1._conflicts == null);

  //now we hit the limit
  for( var i=0; i < newLimit; i++) {
    doc.foo++;
    T(db.save(doc).ok);
  }

  CouchDB.replicate("test_suite_db_a", "test_suite_db_b");

  var docB2 = dbB.open("foo",{conflicts:true});

  // we have a conflict, but the previous replicated rev is always the losing
  // conflict
  T(docB2._conflicts[0] == docB1._rev)

  // We having already updated bar before setting the limit, so it's still got
  // a long rev history. compact to stem the revs.

  T(db.open("bar", {revs:true})._revisions.ids.length == newLimit + 1);

  T(db.compact().ok);

  // compaction isn't instantaneous, loop until done
  while (db.info().compact_running) {};

  // force reload because ETags don't honour compaction
  var req = db.request("GET", "/test_suite_db_a/bar?revs=true", {
    headers:{"if-none-match":"pommes"}
  });

  var finalDoc = JSON.parse(req.responseText);
  TEquals(newLimit, finalDoc._revisions.ids.length,
    "should return a truncated revision list");
};
