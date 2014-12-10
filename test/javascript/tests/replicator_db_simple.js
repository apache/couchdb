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

couchTests.replicator_db_simple = function(debug) {

  if (debug) debugger;

  var populate_db = replicator_db.populate_db;
  var docs1 = replicator_db.docs1;
  var dbA = replicator_db.dbA;
  var dbB = replicator_db.dbB;
  var repDb = replicator_db.repDb;
  var waitForRep = replicator_db.waitForRep;

  function simple_replication() {
    populate_db(dbA, docs1);
    populate_db(dbB, []);

    var repDoc = {
      _id: "foo_simple_rep",
      source: dbA.name,
      target: dbB.name
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
    T(repDoc1._replication_state === "completed", "simple");
    T(typeof repDoc1._replication_state_time === "string");
    T(typeof repDoc1._replication_id  === "string");
    T(typeof repDoc1._replication_stats === "object", "doc has stats");
    var stats = repDoc1._replication_stats;
    TEquals(docs1.length, stats.revisions_checked,
       "right # of revisions_checked");
    TEquals(docs1.length, stats.missing_revisions_found,
      "right # of missing_revisions_found");
    TEquals(docs1.length, stats.docs_read, "right # of docs_read");
    TEquals(docs1.length, stats.docs_written, "right # of docs_written");
    TEquals(0, stats.doc_write_failures, "right # of doc_write_failures");
    TEquals(dbA.info().update_seq, stats.checkpointed_source_seq,
      "right checkpointed_source_seq");
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
  run_on_modified_server(server_config, simple_replication);

/*
 * Disabled, since error state would be set on the document only after
 * the exponential backoff retry done by the replicator database listener
 * terminates, which takes too much time for a unit test.
 */
 /*
   function error_state_replication() {
    populate_db(dbA, docs1);

    var repDoc = {
      _id: "foo_error_rep",
      source: dbA.name,
      target: "nonexistent_test_db"
    };
    T(repDb.save(repDoc).ok);

    waitForRep(repDb, repDoc, "error");
    var repDoc1 = repDb.open(repDoc._id);
    T(repDoc1 !== null);
    T(repDoc1._replication_state === "error");
    T(typeof repDoc1._replication_state_time === "string");
    T(typeof repDoc1._replication_id  === "string");
  }
 */
/*
 * repDb.deleteDb();
 * restartServer();
 * run_on_modified_server(server_config, error_state_replication);
 */


  // cleanup
  repDb.deleteDb();
  dbA.deleteDb();
  dbB.deleteDb();
}