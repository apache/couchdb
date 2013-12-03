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

couchTests.replicator_db_invalid_filter = function(debug) {

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
  var wait_rep_doc = replicator_db.wait_rep_doc;

  function test_invalid_filter() {
    // COUCHDB-1199 - replication document with a filter field that was invalid
    // crashed the CouchDB server.
    var repDoc1 = {
       _id: "rep1",
       source: "couch_foo_test_db",
       target: "couch_bar_test_db",
       filter: "test/foofilter"
    };

    TEquals(true, repDb.save(repDoc1).ok);

    waitForRep(repDb, repDoc1, "error");
    repDoc1 = repDb.open(repDoc1._id);
    TEquals("undefined", typeof repDoc1._replication_id);
    TEquals("error", repDoc1._replication_state);
    TEquals("Could not open source database `couch_foo_test_db`: {db_not_found,<<\"couch_foo_test_db\">>}",
            repDoc1._replication_state_reason);

    populate_db(dbA, docs1);
    populate_db(dbB, []);

    var repDoc2 = {
       _id: "rep2",
       source: dbA.name,
       target: dbB.name,
       filter: "test/foofilter"
    };

    TEquals(true, repDb.save(repDoc2).ok);

    waitForRep(repDb, repDoc2, "error");
    repDoc2 = repDb.open(repDoc2._id);
    TEquals("undefined", typeof repDoc2._replication_id);
    TEquals("error", repDoc2._replication_state);
    TEquals("Couldn't open document `_design/test` from source database `test_suite_rep_db_a`: {error,<<\"not_found\">>}",
            repDoc2._replication_state_reason);

    var ddoc = {
      _id: "_design/mydesign",
      language : "javascript",
      filters : {
        myfilter : (function(doc, req) {
          return true;
        }).toString()
      }
    };

    TEquals(true, dbA.save(ddoc).ok);

    var repDoc3 = {
       _id: "rep3",
       source: dbA.name,
       target: dbB.name,
       filter: "mydesign/myfilter"
    };

    TEquals(true, repDb.save(repDoc3).ok);

    waitForRep(repDb, repDoc3, "completed");
    repDoc3 = repDb.open(repDoc3._id);
    TEquals("string", typeof repDoc3._replication_id);
    TEquals("completed", repDoc3._replication_state);
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
  run_on_modified_server(server_config, test_invalid_filter);

  // cleanup
  repDb.deleteDb();
  dbA.deleteDb();
  dbB.deleteDb();
  usersDb.deleteDb();
}