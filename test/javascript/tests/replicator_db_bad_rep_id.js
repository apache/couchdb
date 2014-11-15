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

couchTests.replicator_db_bad_rep_id = function(debug) {

  if (debug) debugger;

  var populate_db = replicator_db.populate_db;
  var docs1 = replicator_db.docs1;
  var dbA = replicator_db.dbA;
  var dbB = replicator_db.dbB;
  var repDb = replicator_db.repDb;
  var wait = replicator_db.wait;
  var waitForRep = replicator_db.waitForRep;
  var waitForSeq = replicator_db.waitForSeq;

  function rep_doc_with_bad_rep_id() {
    populate_db(dbA, docs1);
    populate_db(dbB, []);

    var repDoc = {
      _id: "foo_rep",
      source: dbA.name,
      target: dbB.name,
      replication_id: "1234abc"
    };
    T(repDb.save(repDoc).ok);

    waitForRep(repDb, repDoc, "completed");
    for (var i = 0; i < docs1.length; i++) {
      var doc = docs1[i];
      var copy = dbB.open(doc._id);
      T(copy !== null);
      T(copy.value === doc.value);
    }

    var repDoc1 = repDb.open(repDoc._id);
    T(repDoc1 !== null);
    T(repDoc1.source === repDoc.source);
    T(repDoc1.target === repDoc.target);
    T(repDoc1._replication_state === "completed",
      "replication document with bad replication id failed");
    T(typeof repDoc1._replication_state_time === "string");
    T(typeof repDoc1._replication_id  === "string");
    T(repDoc1._replication_id !== "1234abc");
  }

  var server_config = [
    {
      section: "couch_httpd_auth",
      key: "iterations",
      value: "1"
    },
    {
      section: "replicator",
      key: "db",
      value: repDb.name
    }
  ];

  repDb.deleteDb();
  run_on_modified_server(server_config, rep_doc_with_bad_rep_id);

  // cleanup
  repDb.deleteDb();
  dbA.deleteDb();
  dbB.deleteDb();
}