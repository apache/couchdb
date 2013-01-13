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

couchTests.replicator_db_compact_rep_db = function(debug) {

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

  function compact_rep_db() {
    var dbA_copy = new CouchDB("test_suite_rep_db_a_copy");
    var dbB_copy = new CouchDB("test_suite_rep_db_b_copy");
    var repDoc1, repDoc2;
    var xhr, i, doc, copy, new_doc;
    var docs = makeDocs(1, 50);

    populate_db(dbA, docs);
    populate_db(dbB, docs);
    populate_db(dbA_copy, []);
    populate_db(dbB_copy, []);

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

    TEquals(true, repDb.save(repDoc1).ok);
    TEquals(true, repDb.save(repDoc2).ok);

    TEquals(true, repDb.compact().ok);
    TEquals(202, repDb.last_req.status);

    waitForSeq(dbA, dbA_copy);
    waitForSeq(dbB, dbB_copy);

    while (repDb.info().compact_running) {};

    for (i = 0; i < docs.length; i++) {
      copy = dbA_copy.open(docs[i]._id);
      T(copy !== null);
      copy = dbB_copy.open(docs[i]._id);
      T(copy !== null);
    }

    new_doc = {
      _id: "foo666",
      value: 666
    };

    TEquals(true, dbA.save(new_doc).ok);
    TEquals(true, dbB.save(new_doc).ok);

    waitForSeq(dbA, dbA_copy);
    waitForSeq(dbB, dbB_copy);

    copy = dbA.open(new_doc._id);
    T(copy !== null);
    TEquals(666, copy.value);
    copy = dbB.open(new_doc._id);
    T(copy !== null);
    TEquals(666, copy.value);
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
  run_on_modified_server(server_config, compact_rep_db);

  // cleanup
  repDb.deleteDb();
  dbA.deleteDb();
  dbB.deleteDb();
  usersDb.deleteDb();
  (new CouchDB("test_suite_rep_db_a_copy")).deleteDb();
  (new CouchDB("test_suite_rep_db_b_copy")).deleteDb();

}