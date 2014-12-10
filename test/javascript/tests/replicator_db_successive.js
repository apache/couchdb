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

couchTests.replicator_db_successive = function(debug) {

  if (debug) debugger;

  var populate_db = replicator_db.populate_db;
  var docs1 = replicator_db.docs1;
  var dbA = replicator_db.dbA;
  var dbB = replicator_db.dbB;
  var repDb = replicator_db.repDb;
  var wait = replicator_db.wait;
  var waitForRep = replicator_db.waitForRep;
  var waitForSeq = replicator_db.waitForSeq;

  function successive_identical_replications() {
    populate_db(dbA, docs1);
    populate_db(dbB, []);

    var repDoc1 = {
      _id: "foo_ident_rep_1",
      source: dbA.name,
      target: dbB.name
    };
    T(repDb.save(repDoc1).ok);

    waitForRep(repDb, repDoc1, "completed");
    for (var i = 0; i < docs1.length; i++) {
      var doc = docs1[i];
      var copy = dbB.open(doc._id);
      T(copy !== null);
      T(copy.value === doc.value);
    }

    var repDoc1_copy = repDb.open(repDoc1._id);
    T(repDoc1_copy !== null);
    T(repDoc1_copy.source === repDoc1.source);
    T(repDoc1_copy.target === repDoc1.target);
    T(repDoc1_copy._replication_state === "completed");
    T(typeof repDoc1_copy._replication_state_time === "string");
    T(typeof repDoc1_copy._replication_id  === "string");
    T(typeof repDoc1_copy._replication_stats === "object", "doc has stats");
    var stats = repDoc1_copy._replication_stats;
    TEquals(docs1.length, stats.revisions_checked,
      "right # of revisions_checked");
    TEquals(docs1.length, stats.missing_revisions_found,
      "right # of missing_revisions_found");
    TEquals(docs1.length, stats.docs_read, "right # of docs_read");
    TEquals(docs1.length, stats.docs_written, "right # of docs_written");
    TEquals(0, stats.doc_write_failures, "right # of doc_write_failures");
    TEquals(dbA.info().update_seq, stats.checkpointed_source_seq,
      "right checkpointed_source_seq");

    var newDoc = {
      _id: "doc666",
      value: 666
    };
    T(dbA.save(newDoc).ok);

    wait(200);
    var newDoc_copy = dbB.open(newDoc._id);
    // not replicated because first replication is complete (not continuous)
    T(newDoc_copy === null);

    var repDoc2 = {
      _id: "foo_ident_rep_2",
      source: dbA.name,
      target: dbB.name
    };
    T(repDb.save(repDoc2).ok);

    waitForRep(repDb, repDoc2, "completed");
    var newDoc_copy = dbB.open(newDoc._id);
    T(newDoc_copy !== null);
    T(newDoc_copy.value === newDoc.value);

    var repDoc2_copy = repDb.open(repDoc2._id);
    T(repDoc2_copy !== null);
    T(repDoc2_copy.source === repDoc1.source);
    T(repDoc2_copy.target === repDoc1.target);
    T(repDoc2_copy._replication_state === "completed");
    T(typeof repDoc2_copy._replication_state_time === "string");
    T(typeof repDoc2_copy._replication_id === "string");
    T(repDoc2_copy._replication_id === repDoc1_copy._replication_id);
    T(typeof repDoc2_copy._replication_stats === "object", "doc has stats");
    stats = repDoc2_copy._replication_stats;
    TEquals(1, stats.revisions_checked, "right # of revisions_checked");
    TEquals(1, stats.missing_revisions_found,
      "right # of missing_revisions_found");
    TEquals(1, stats.docs_read, "right # of docs_read");
    TEquals(1, stats.docs_written, "right # of docs_written");
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
  run_on_modified_server(server_config, successive_identical_replications);

  // cleanup
  repDb.deleteDb();
  dbA.deleteDb();
  dbB.deleteDb();
}