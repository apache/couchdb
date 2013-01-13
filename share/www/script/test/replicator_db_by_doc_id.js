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

couchTests.replicator_db_by_doc_id = function(debug) {

  if (debug) debugger;

  var populate_db = replicator_db.populate_db;
  var docs1 = replicator_db.docs1;
  var dbA = replicator_db.dbA;
  var dbB = replicator_db.dbB;
  var repDb = replicator_db.repDb;
  var wait = replicator_db.wait;
  var waitForRep = replicator_db.waitForRep;
  var waitForSeq = replicator_db.waitForSeq;

  function by_doc_ids_replication() {
    // to test that we can replicate docs with slashes in their IDs
    var docs2 = docs1.concat([
      {
        _id: "_design/mydesign",
        language : "javascript"
      }
    ]);

    populate_db(dbA, docs2);
    populate_db(dbB, []);

    var repDoc = {
      _id: "foo_cont_rep_doc",
      source: "http://" + CouchDB.host + "/" + dbA.name,
      target: dbB.name,
      doc_ids: ["foo666", "foo3", "_design/mydesign", "foo999", "foo1"]
    };
    T(repDb.save(repDoc).ok);

    waitForRep(repDb, repDoc, "completed");
    var copy = dbB.open("foo1");
    T(copy !== null);
    T(copy.value === 11);

    copy = dbB.open("foo2");
    T(copy === null);

    copy = dbB.open("foo3");
    T(copy !== null);
    T(copy.value === 33);

    copy = dbB.open("foo666");
    T(copy === null);

    copy = dbB.open("foo999");
    T(copy === null);

    copy = dbB.open("_design/mydesign");
    T(copy === null);

    repDoc = repDb.open(repDoc._id);
    T(typeof repDoc._replication_stats === "object", "doc has stats");
    var stats = repDoc._replication_stats;
    TEquals(3, stats.revisions_checked, "right # of revisions_checked");
    TEquals(3, stats.missing_revisions_found, "right # of missing_revisions_found");
    TEquals(3, stats.docs_read, "right # of docs_read");
    TEquals(2, stats.docs_written, "right # of docs_written");
    TEquals(1, stats.doc_write_failures, "right # of doc_write_failures");
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
  run_on_modified_server(server_config, by_doc_ids_replication);

  // cleanup
  repDb.deleteDb();
  dbA.deleteDb();
  dbB.deleteDb();
}