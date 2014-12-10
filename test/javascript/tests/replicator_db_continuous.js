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

couchTests.replicator_db_continuous = function(debug) {

  if (debug) debugger;

  var populate_db = replicator_db.populate_db;
  var docs1 = replicator_db.docs1;
  var dbA = replicator_db.dbA;
  var dbB = replicator_db.dbB;
  var repDb = replicator_db.repDb;
  var wait = replicator_db.wait;
  var waitForRep = replicator_db.waitForRep;
  var waitForSeq = replicator_db.waitForSeq;

  function continuous_replication() {
    populate_db(dbA, docs1);
    populate_db(dbB, []);

    var repDoc = {
      _id: "foo_cont_rep_doc",
      source: "http://" + CouchDB.host + "/" + dbA.name,
      target: dbB.name,
      continuous: true,
      user_ctx: {
        roles: ["_admin"]
      }
    };

    T(repDb.save(repDoc).ok);

    waitForSeq(dbA, dbB);
    for (var i = 0; i < docs1.length; i++) {
      var doc = docs1[i];
      var copy = dbB.open(doc._id);
      T(copy !== null);
      T(copy.value === doc.value);
    }

    var tasks = JSON.parse(CouchDB.request("GET", "/_active_tasks").responseText);
    TEquals(1, tasks.length, "1 active task");
    TEquals(repDoc._id, tasks[0].doc_id, "replication doc id in active tasks");

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

    var repDoc1 = repDb.open(repDoc._id);
    T(repDoc1 !== null);
    T(repDoc1.source === repDoc.source);
    T(repDoc1.target === repDoc.target);
    T(repDoc1._replication_state === "triggered");
    T(typeof repDoc1._replication_state_time === "string");
    T(typeof repDoc1._replication_id  === "string");

    // Design documents are only replicated to local targets if the respective
    // replication document has a user_ctx filed with the "_admin" role in it.
    var ddoc = {
      _id: "_design/foobar",
      language: "javascript"
    };

    T(dbA.save(ddoc).ok);

    waitForSeq(dbA, dbB);
    var ddoc_copy = dbB.open("_design/foobar");
    T(ddoc_copy !== null);
    T(ddoc.language === "javascript");

    // update the design doc on source, test that the new revision is replicated
    ddoc.language = "erlang";
    T(dbA.save(ddoc).ok);
    T(ddoc._rev.indexOf("2-") === 0);

    waitForSeq(dbA, dbB);
    ddoc_copy = dbB.open("_design/foobar");
    T(ddoc_copy !== null);
    T(ddoc_copy._rev === ddoc._rev);
    T(ddoc.language === "erlang");

    // stop replication by deleting the replication document
    T(repDb.deleteDoc(repDoc1).ok);

    // add another doc to source, it will NOT be replicated to target
    var docY = {
      _id: "foo666",
      value: 999
    };

    T(dbA.save(docY).ok);

    wait(200); // is there a way to avoid wait here?
    var copy = dbB.open("foo666");
    T(copy === null);
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
  run_on_modified_server(server_config, continuous_replication);

  // cleanup
  repDb.deleteDb();
  dbA.deleteDb();
  dbB.deleteDb();
}