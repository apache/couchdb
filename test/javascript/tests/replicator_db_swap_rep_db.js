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

couchTests.replicator_db_swap_rep_db = function(debug) {

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

  function swap_rep_db() {
    var repDb2 = new CouchDB("test_suite_rep_db_2");
    var dbA = new CouchDB("test_suite_rep_db_a");
    var dbA_copy = new CouchDB("test_suite_rep_db_a_copy");
    var dbB = new CouchDB("test_suite_rep_db_b");
    var dbB_copy = new CouchDB("test_suite_rep_db_b_copy");
    var dbC = new CouchDB("test_suite_rep_db_c");
    var dbC_copy = new CouchDB("test_suite_rep_db_c_copy");
    var repDoc1, repDoc2, repDoc3;
    var xhr, i, doc, copy, new_doc;

    populate_db(dbA, docs1);
    populate_db(dbB, docs1);
    populate_db(dbC, docs1);
    populate_db(dbA_copy, []);
    populate_db(dbB_copy, []);
    populate_db(dbC_copy, []);
    populate_db(repDb2, []);

    repDoc1 = {
      _id: "rep1",
      source: CouchDB.protocol + CouchDB.host + "/" + dbA.name,
      target: dbA_copy.name,
      continuous: true
    };
    repDoc2 = {
      _id: "rep2",
      source: CouchDB.protocol + CouchDB.host + "/" + dbB.name,
      target: dbB_copy.name,
      continuous: true
    };
    repDoc3 = {
      _id: "rep3",
      source: CouchDB.protocol + CouchDB.host + "/" + dbC.name,
      target: dbC_copy.name,
      continuous: true
    };

    TEquals(true, repDb.save(repDoc1).ok);
    TEquals(true, repDb.save(repDoc2).ok);

    waitForSeq(dbA, dbA_copy);
    waitForSeq(dbB, dbB_copy);

    xhr = CouchDB.request("PUT", "/_config/replicator/db",{
      body : JSON.stringify(repDb2.name),
      headers: {"X-Couch-Persist": "false"}
    });
    TEquals(200, xhr.status);

    // Temporary band-aid, give the replicator db some
    // time to make the switch
    wait(500);

    new_doc = {
      _id: "foo666",
      value: 666
    };

    TEquals(true, dbA.save(new_doc).ok);
    TEquals(true, dbB.save(new_doc).ok);
    waitForSeq(dbA, dbA_copy);
    waitForSeq(dbB, dbB_copy);

    TEquals(true, repDb2.save(repDoc3).ok);
    waitForSeq(dbC, dbC_copy);

    for (i = 0; i < docs1.length; i++) {
      doc = docs1[i];
      copy = dbA_copy.open(doc._id);
      T(copy !== null);
      TEquals(doc.value, copy.value);
      copy = dbB_copy.open(doc._id);
      T(copy !== null);
      TEquals(doc.value, copy.value);
      copy = dbC_copy.open(doc._id);
      T(copy !== null);
      TEquals(doc.value, copy.value);
    }

    // replications rep1 and rep2 should have been stopped when the replicator
    // database was swapped
    copy = dbA_copy.open(new_doc._id);
    TEquals(null, copy);
    copy = dbB_copy.open(new_doc._id);
    TEquals(null, copy);

    xhr = CouchDB.request("PUT", "/_config/replicator/db",{
      body : JSON.stringify(repDb.name),
      headers: {"X-Couch-Persist": "false"}
    });
    TEquals(200, xhr.status);

    // after setting the replicator database to the former, replications rep1
    // and rep2 should have been resumed, while rep3 was stopped
    TEquals(true, dbC.save(new_doc).ok);
    wait(1000);

    waitForSeq(dbA, dbA_copy);
    waitForSeq(dbB, dbB_copy);

    copy = dbA_copy.open(new_doc._id);
    T(copy !== null);
    TEquals(new_doc.value, copy.value);
    copy = dbB_copy.open(new_doc._id);
    T(copy !== null);
    TEquals(new_doc.value, copy.value);
    copy = dbC_copy.open(new_doc._id);
    TEquals(null, copy);
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
  run_on_modified_server(server_config, swap_rep_db);

  // cleanup
  repDb.deleteDb();
  dbA.deleteDb();
  dbB.deleteDb();
  usersDb.deleteDb();
  (new CouchDB("test_suite_rep_db_2")).deleteDb();
  (new CouchDB("test_suite_rep_db_c")).deleteDb();
  (new CouchDB("test_suite_rep_db_a_copy")).deleteDb();
  (new CouchDB("test_suite_rep_db_b_copy")).deleteDb();
  (new CouchDB("test_suite_rep_db_c_copy")).deleteDb();

}