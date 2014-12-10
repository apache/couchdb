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

couchTests.replicator_db_survives = function(debug) {

  if (debug) debugger;

  var populate_db = replicator_db.populate_db;
  var docs1 = replicator_db.docs1;
  var dbA = replicator_db.dbA;
  var dbB = replicator_db.dbB;
  var repDb = replicator_db.repDb;
  var usersDb = replicator_db.usersDb;
  var wait = replicator_db.wait;
  var waitForRep = replicator_db.waitForRep;
  var waitForSeq = replicator_db.waitForSeq;
  var waitForDocPos = replicator_db.waitForDocPos;
  var wait_rep_doc = replicator_db.wait_rep_doc;

  function continuous_replication_survives_restart() {
    var origRepDbName = CouchDB.request(
      "GET", "/_config/replicator/db").responseText;

    repDb.deleteDb();

    var xhr = CouchDB.request("PUT", "/_config/replicator/db", {
      body : JSON.stringify(repDb.name),
      headers: {"X-Couch-Persist": "false"}
    });
    T(xhr.status === 200);

    populate_db(dbA, docs1);
    populate_db(dbB, []);

    var repDoc = {
      _id: "foo_cont_rep_survives_doc",
      source: dbA.name,
      target: dbB.name,
      continuous: true
    };

    T(repDb.save(repDoc).ok);

    waitForSeq(dbA, dbB);
    for (var i = 0; i < docs1.length; i++) {
      var doc = docs1[i];
      var copy = dbB.open(doc._id);
      T(copy !== null);
      T(copy.value === doc.value);
    }

    repDb.ensureFullCommit();
    dbA.ensureFullCommit();

    restartServer();

    xhr = CouchDB.request("PUT", "/_config/replicator/db", {
      body : JSON.stringify(repDb.name),
      headers: {"X-Couch-Persist": "false"}
    });

    T(xhr.status === 200);

    // add another doc to source, it will be replicated to target
    var docX = {
      _id: "foo1000",
      value: 1001
    };

    T(dbA.save(docX).ok);

    waitForSeq(dbA, dbB);
    var copy = dbB.open("foo1000");
    T(copy !== null);
    T(copy.value === 1001);

    repDoc = waitForDocPos(repDb, "foo_cont_rep_survives_doc", 3);
    T(repDoc !== null);
    T(repDoc.continuous === true);

    // stop replication
    T(repDb.deleteDoc(repDoc).ok);

    xhr = CouchDB.request("PUT", "/_config/replicator/db", {
      body : origRepDbName,
      headers: {"X-Couch-Persist": "false"}
    });
    T(xhr.status === 200);
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
    },
    {
      section: "couch_httpd_auth",
      key: "authentication_db",
      value: usersDb.name
    }
  ];

  repDb.deleteDb();
  run_on_modified_server(server_config, continuous_replication_survives_restart);

  // cleanup
  repDb.deleteDb();
  dbA.deleteDb();
  dbB.deleteDb();
  usersDb.deleteDb();
}
