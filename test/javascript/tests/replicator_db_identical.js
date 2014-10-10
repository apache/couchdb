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

couchTests.replicator_db_identical = function(debug) {

  if (debug) debugger;

  var populate_db = replicator_db.populate_db;
  var docs1 = replicator_db.docs1;
  var dbA = replicator_db.dbA;
  var dbB = replicator_db.dbB;
  var repDb = replicator_db.repDb;
  var wait = replicator_db.wait;
  var waitForRep = replicator_db.waitForRep;
  var waitForSeq = replicator_db.waitForSeq;

  // test the case where multiple replication docs (different IDs)
  // describe in fact the same replication (source, target, etc)
  function identical_rep_docs() {
    populate_db(dbA, docs1);
    populate_db(dbB, []);

    var repDoc1 = {
      _id: "foo_dup_rep_doc_1",
      source: "http://" + CouchDB.host + "/" + dbA.name,
      target: dbB.name
    };
    var repDoc2 = {
      _id: "foo_dup_rep_doc_2",
      source: "http://" + CouchDB.host + "/" + dbA.name,
      target: dbB.name
    };

    T(repDb.save(repDoc1).ok);
    T(repDb.save(repDoc2).ok);

    waitForRep(repDb, repDoc1, "completed");
    for (var i = 0; i < docs1.length; i++) {
      var doc = docs1[i];
      var copy = dbB.open(doc._id);
      T(copy !== null);
      T(copy.value === doc.value);
    }

    repDoc1 = repDb.open("foo_dup_rep_doc_1");
    T(repDoc1 !== null);
    T(repDoc1._replication_state === "completed", "identical");
    T(typeof repDoc1._replication_state_time === "string");
    T(typeof repDoc1._replication_id  === "string");

    repDoc2 = repDb.open("foo_dup_rep_doc_2");
    T(repDoc2 !== null);
    T(typeof repDoc2._replication_state === "undefined");
    T(typeof repDoc2._replication_state_time === "undefined");
    T(repDoc2._replication_id === repDoc1._replication_id);
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
  run_on_modified_server(server_config, identical_rep_docs);

  // cleanup
  repDb.deleteDb();
  dbA.deleteDb();
  dbB.deleteDb();
}