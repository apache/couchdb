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

  function rep_db_write_authorization() {
    populate_db(dbA, docs1);
    populate_db(dbB, []);

    var server_admins_config = [
      {
        section: "admins",
        key: "fdmanana",
        value: "qwerty"
      }
    ];

    run_on_modified_server(server_admins_config, function() {
      var repDoc = {
        _id: "foo_rep_doc",
        source: dbA.name,
        target: dbB.name,
        continuous: true
      };

      T(CouchDB.login("fdmanana", "qwerty").ok);
      T(CouchDB.session().userCtx.name === "fdmanana");
      T(CouchDB.session().userCtx.roles.indexOf("_admin") !== -1);

      T(repDb.save(repDoc).ok);

      waitForRep(repDb, repDoc, "completed");

      for (var i = 0; i < docs1.length; i++) {
        var doc = docs1[i];
        var copy = dbB.open(doc._id);

        T(copy !== null);
        T(copy.value === doc.value);
      }

      repDoc = repDb.open("foo_rep_doc");
      T(repDoc !== null);
      repDoc.target = "test_suite_foo_db";
      repDoc.create_target = true;

      // Only the replicator can update replication documents.
      // Admins can only add and delete replication documents.
      try {
        repDb.save(repDoc);
        T(false && "Should have thrown an exception");
      } catch (x) {
        T(x["error"] === "forbidden");
      }
    });
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
  run_on_modified_server(server_config, rep_db_write_authorization);

  // cleanup
  repDb.deleteDb();
  dbA.deleteDb();
  dbB.deleteDb();
  usersDb.deleteDb();
}