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

couchTests.replicator_db_filtered = function(debug) {

  if (debug) debugger;

  var populate_db = replicator_db.populate_db;
  var docs1 = replicator_db.docs1;
  var dbA = replicator_db.dbA;
  var dbB = replicator_db.dbB;
  var repDb = replicator_db.repDb;
  var waitForRep = replicator_db.waitForRep;

  function filtered_replication() {
    var docs2 = docs1.concat([
      {
        _id: "_design/mydesign",
        language : "javascript",
        filters : {
          myfilter : (function(doc, req) {
            return (doc.value % 2) !== Number(req.query.myparam);
          }).toString()
        }
      }
    ]);

    populate_db(dbA, docs2);
    populate_db(dbB, []);

    var repDoc = {
      _id: "foo_filt_rep_doc",
      source: "http://" + CouchDB.host + "/" + dbA.name,
      target: dbB.name,
      filter: "mydesign/myfilter",
      query_params: {
        myparam: 1
      }
    };
    T(repDb.save(repDoc).ok);

    waitForRep(repDb, repDoc, "completed");
    for (var i = 0; i < docs2.length; i++) {
      var doc = docs2[i];
      var copy = dbB.open(doc._id);

      if (typeof doc.value === "number") {
        if ((doc.value % 2) !== 1) {
          T(copy !== null);
          T(copy.value === doc.value);
        } else {
          T(copy === null);
        }
      }
    }

    var repDoc1 = repDb.open(repDoc._id);
    T(repDoc1 !== null);
    T(repDoc1.source === repDoc.source);
    T(repDoc1.target === repDoc.target);
    T(repDoc1._replication_state === "completed", "filtered");
    T(typeof repDoc1._replication_state_time === "string");
    T(typeof repDoc1._replication_id  === "string");
    T(typeof repDoc1._replication_stats === "object", "doc has stats");
    var stats = repDoc1._replication_stats;
    TEquals(2, stats.revisions_checked, "right # of revisions_checked");
    TEquals(2, stats.missing_revisions_found, "right # of missing_revisions_found");
    TEquals(2, stats.docs_read, "right # of docs_read");
    TEquals(1, stats.docs_written, "right # of docs_written");
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
  run_on_modified_server(server_config, filtered_replication);

  // cleanup
  repDb.deleteDb();
  dbA.deleteDb();
  dbB.deleteDb();
}