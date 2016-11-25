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
  //return console.log('TODO');
  if (debug) debugger;

  var populate_db = replicator_db.populate_db;
  var docs1 = replicator_db.docs1;
  // TODO: dice DBs (at least target)
  var dbA = replicator_db.dbA;
  var dbB = replicator_db.dbB;
  //var repDb = replicator_db.repDb;
  var replDb = new CouchDB("_replicator");
  var wait = replicator_db.wait;
  var waitForRep = replicator_db.waitForRep;
  var waitForSeq = replicator_db.waitForSeq;

  function rep_doc_with_bad_rep_id() {
    populate_db(dbA, docs1);
    populate_db(dbB, []);

    var repDoc = {
      _id: "foo_rep",
// TODO: fix DB name issue and remove absolute URL again
      source: 'http://localhost:15984/'+dbA.name,
      target: 'http://localhost:15984/'+dbB.name,
      replication_id: "1234abc"
    };
    T(replDb.save(repDoc).ok);

    T(waitForRep(replDb, repDoc, "completed", "error") == "completed");
    for (var i = 0; i < docs1.length; i++) {
      var doc = docs1[i];
      var copy = dbB.open(doc._id);
      T(copy !== null);
      T(copy.value === doc.value);
    }

    var repDoc1 = replDb.open(repDoc._id);
    T(repDoc1 !== null);
    T(repDoc1.source === repDoc.source);
    T(repDoc1.target === repDoc.target);
    T(repDoc1._replication_state === "completed",
      "replication document with bad replication id failed");
    T(typeof repDoc1._replication_state_time === "string");
    T(typeof repDoc1._replication_id === "undefined");
  }

  /*var server_config = [
    {
      section: "couch_httpd_auth",
      key: "iterations",
      value: "1"
    },
    {
      section: "replicator",
      key: "db",
      value: null //repDb.name
    }
  ];*/

  //repDb.deleteDb();
  // don't run on modified server as it would be strange on cluster
  // but use "normal" replication DB, create a doc, reliably clear after run
  // on delete fail, the next tests would all fail
  function handleReplDoc(show) {
    var replDoc = replDb.open("foo_rep");
    if(replDoc!=null) {
      if(show) {
        //console.log(JSON.stringify(replDoc));
      }
      replDb.deleteDoc(replDoc);
    }
  }

  handleReplDoc();
  try {
    rep_doc_with_bad_rep_id();
  } finally {
    // cleanup or log
    try {
      handleReplDoc(true);
    } catch (e2) {
      console.log("Error during cleanup " + e2);
    }
  }
  //run_on_modified_server(server_config, rep_doc_with_bad_rep_id);

  // cleanup
  //repDb.deleteDb();
  dbA.deleteDb();
  dbB.deleteDb();
}
