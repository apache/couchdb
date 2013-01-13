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

couchTests.replicator_db_field_validation = function(debug) {

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

  function rep_doc_field_validation() {
    var docs = makeDocs(1, 5);

    populate_db(dbA, docs);
    populate_db(dbB, []);

    var repDoc = {
       _id: "rep1",
       target: dbB.name
    };

    try {
      repDb.save(repDoc);
      T(false, "should have failed because source field is missing");
    } catch (x) {
      TEquals("forbidden", x.error);
    }

    repDoc = {
       _id: "rep1",
       source: 123,
       target: dbB.name
    };

    try {
      repDb.save(repDoc);
      T(false, "should have failed because source field is a number");
    } catch (x) {
      TEquals("forbidden", x.error);
    }

    repDoc = {
       _id: "rep1",
       source: dbA.name
    };

    try {
      repDb.save(repDoc);
      T(false, "should have failed because target field is missing");
    } catch (x) {
      TEquals("forbidden", x.error);
    }

    repDoc = {
       _id: "rep1",
       source: dbA.name,
       target: null
    };

    try {
      repDb.save(repDoc);
      T(false, "should have failed because target field is null");
    } catch (x) {
      TEquals("forbidden", x.error);
    }

    repDoc = {
       _id: "rep1",
       source: dbA.name,
       target: { url: 123 }
    };

    try {
      repDb.save(repDoc);
      T(false, "should have failed because target.url field is not a string");
    } catch (x) {
      TEquals("forbidden", x.error);
    }

    repDoc = {
       _id: "rep1",
       source: dbA.name,
       target: { url: dbB.name, auth: null }
    };

    try {
      repDb.save(repDoc);
      T(false, "should have failed because target.auth field is null");
    } catch (x) {
      TEquals("forbidden", x.error);
    }

    repDoc = {
       _id: "rep1",
       source: dbA.name,
       target: { url: dbB.name, auth: "foo:bar" }
    };

    try {
      repDb.save(repDoc);
      T(false, "should have failed because target.auth field is not an object");
    } catch (x) {
      TEquals("forbidden", x.error);
    }

    repDoc = {
       _id: "rep1",
       source: dbA.name,
       target: dbB.name,
       continuous: "true"
    };

    try {
      repDb.save(repDoc);
      T(false, "should have failed because continuous is not a boolean");
    } catch (x) {
      TEquals("forbidden", x.error);
    }

    repDoc = {
       _id: "rep1",
       source: dbA.name,
       target: dbB.name,
       filter: 123
    };

    try {
      repDb.save(repDoc);
      T(false, "should have failed because filter is not a string");
    } catch (x) {
      TEquals("forbidden", x.error);
    }
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
  run_on_modified_server(server_config, rep_doc_field_validation);

  // cleanup
  repDb.deleteDb();
  dbA.deleteDb();
  dbB.deleteDb();
  usersDb.deleteDb();
}