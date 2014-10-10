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

couchTests.replicator_db_credential_delegation = function(debug) {

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

  function test_replication_credentials_delegation() {
    populate_db(usersDb, []);

    var joeUserDoc = CouchDB.prepareUserDoc({
      name: "joe",
      roles: ["god", "erlanger"]
    }, "erly");
    T(usersDb.save(joeUserDoc).ok);

    var ddoc = {
      _id: "_design/beer",
      language: "javascript"
    };
    populate_db(dbA, docs1.concat([ddoc]));
    populate_db(dbB, []);

    T(dbB.setSecObj({
      admins: {
        names: [],
        roles: ["god"]
      }
    }).ok);

    var server_admins_config = [
      {
        section: "couch_httpd_auth",
        key: "iterations",
        value: "1"
      },
      {
        section: "admins",
        key: "fdmanana",
        value: "qwerty"
      }
    ];

    run_on_modified_server(server_admins_config, function() {

      T(CouchDB.login("fdmanana", "qwerty").ok);
      T(CouchDB.session().userCtx.name === "fdmanana");
      T(CouchDB.session().userCtx.roles.indexOf("_admin") !== -1);

      var repDoc = {
        _id: "foo_rep_del_doc_1",
        source: dbA.name,
        target: dbB.name,
        user_ctx: {
          name: "joe",
          roles: ["erlanger"]
        }
      };

      T(repDb.save(repDoc).ok);

      waitForRep(repDb, repDoc, "completed");
      for (var i = 0; i < docs1.length; i++) {
        var doc = docs1[i];
        var copy = dbB.open(doc._id);
        T(copy !== null);
        T(copy.value === doc.value);
      }

      // design doc was not replicated, because joe is not an admin of db B
      var doc = dbB.open(ddoc._id);
      T(doc === null);

      // now test the same replication but putting the role "god" in the
      // delegation user context property
      var repDoc2 = {
        _id: "foo_rep_del_doc_2",
        source: dbA.name,
        target: dbB.name,
        user_ctx: {
          name: "joe",
          roles: ["erlanger", "god"]
        }
      };
      T(repDb.save(repDoc2).ok);

      waitForRep(repDb, repDoc2, "completed");
      for (var i = 0; i < docs1.length; i++) {
        var doc = docs1[i];
        var copy = dbB.open(doc._id);
        T(copy !== null);
        T(copy.value === doc.value);
      }

      // because anyone with a 'god' role is an admin of db B, a replication
      // that is delegated to a 'god' role can write design docs to db B
      doc = dbB.open(ddoc._id);
      T(doc !== null);
      T(doc.language === ddoc.language);
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
    },
    {
      section: "couch_httpd_auth",
      key: "authentication_db",
      value: usersDb.name
    }
  ];

  repDb.deleteDb();
  run_on_modified_server(server_config, test_replication_credentials_delegation);

  // cleanup
  repDb.deleteDb();
  dbA.deleteDb();
  dbB.deleteDb();
  usersDb.deleteDb();
}