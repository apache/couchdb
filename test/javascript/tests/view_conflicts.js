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

couchTests.view_conflicts = function(debug) {
  var dbA = new CouchDB("test_suite_db_a", {"X-Couch-Full-Commit":"false"});
  dbA.deleteDb();
  dbA.createDb();
  var dbB = new CouchDB("test_suite_db_b", {"X-Couch-Full-Commit":"false"});
  dbB.deleteDb();
  dbB.createDb();
  if (debug) debugger;

  var docA = {_id: "foo", bar: 42};
  T(dbA.save(docA).ok);
  CouchDB.replicate(dbA.name, dbB.name);

  var docB = dbB.open("foo");
  docB.bar = 43;
  dbB.save(docB);
  docA.bar = 41;
  dbA.save(docA);
  CouchDB.replicate(dbA.name, dbB.name);

  var doc = dbB.open("foo", {conflicts: true});
  T(doc._conflicts.length == 1);
  var conflictRev = doc._conflicts[0];
  if (doc.bar == 41) { // A won
    T(conflictRev == docB._rev);
  } else { // B won
    T(doc.bar == 43);
    T(conflictRev == docA._rev);
  }

  var results = dbB.query(function(doc) {
    if (doc._conflicts) {
      emit(doc._id, doc._conflicts);
    }
  });
  T(results.rows[0].value[0] == conflictRev);
};
