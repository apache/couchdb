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

couchTests.replicator_db_user_ctx = function(debug) {

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

 function test_user_ctx_validation() {
    populate_db(dbA, docs1);
    populate_db(dbB, []);
    populate_db(usersDb, []);

    var joeUserDoc = CouchDB.prepareUserDoc({
      name: "joe",
      roles: ["erlanger", "bar"]
    }, "erly");
    var fdmananaUserDoc = CouchDB.prepareUserDoc({
      name: "fdmanana",
      roles: ["a", "b", "c"]
    }, "qwerty");

    TEquals(true, usersDb.save(joeUserDoc).ok);
    TEquals(true, usersDb.save(fdmananaUserDoc).ok);

    T(dbB.setSecObj({
      admins: {
        names: [],
        roles: ["god"]
      },
      readers: {
        names: [],
        roles: ["foo"]
      }
    }).ok);

    TEquals(true, CouchDB.login("joe", "erly").ok);
    TEquals("joe", CouchDB.session().userCtx.name);
    TEquals(-1, CouchDB.session().userCtx.roles.indexOf("_admin"));

    var repDoc = {
      _id: "foo_rep",
      source: CouchDB.protocol + CouchDB.host + "/" + dbA.name,
      target: dbB.name
    };

    try {
      repDb.save(repDoc);
      T(false, "Should have failed, user_ctx missing.");
    } catch (x) {
      TEquals("forbidden", x.error);
    }

    repDoc.user_ctx = {
      name: "john",
      roles: ["erlanger"]
    };

    try {
      repDb.save(repDoc);
      T(false, "Should have failed, wrong user_ctx.name.");
    } catch (x) {
      TEquals("forbidden", x.error);
    }

    repDoc.user_ctx = {
      name: "joe",
      roles: ["bar", "god", "erlanger"]
    };

    try {
      repDb.save(repDoc);
      T(false, "Should have failed, a bad role in user_ctx.roles.");
    } catch (x) {
      TEquals("forbidden", x.error);
    }

    // user_ctx.roles might contain only a subset of the user's roles
    repDoc.user_ctx = {
      name: "joe",
      roles: ["erlanger"]
    };

    TEquals(true, repDb.save(repDoc).ok);
    CouchDB.logout();

    waitForRep(repDb, repDoc, "error");
    var repDoc1 = repDb.open(repDoc._id);
    T(repDoc1 !== null);
    TEquals(repDoc.source, repDoc1.source);
    TEquals(repDoc.target, repDoc1.target);
    TEquals("error", repDoc1._replication_state);
    TEquals("string", typeof repDoc1._replication_id);
    TEquals("string", typeof repDoc1._replication_state_time);

    TEquals(true, CouchDB.login("fdmanana", "qwerty").ok);
    TEquals("fdmanana", CouchDB.session().userCtx.name);
    TEquals(-1, CouchDB.session().userCtx.roles.indexOf("_admin"));

    try {
      T(repDb.deleteDoc(repDoc1).ok);
      T(false, "Shouldn't be able to delete replication document.");
    } catch (x) {
      TEquals("forbidden", x.error);
    }

    CouchDB.logout();
    TEquals(true, CouchDB.login("joe", "erly").ok);
    TEquals("joe", CouchDB.session().userCtx.name);
    TEquals(-1, CouchDB.session().userCtx.roles.indexOf("_admin"));

    T(repDb.deleteDoc(repDoc1).ok);
    CouchDB.logout();

    for (var i = 0; i < docs1.length; i++) {
      var doc = docs1[i];
      var copy = dbB.open(doc._id);

      TEquals(null, copy);
    }

    T(dbB.setSecObj({
      admins: {
        names: [],
        roles: ["god", "erlanger"]
      },
      readers: {
        names: [],
        roles: ["foo"]
      }
    }).ok);

    TEquals(true, CouchDB.login("joe", "erly").ok);
    TEquals("joe", CouchDB.session().userCtx.name);
    TEquals(-1, CouchDB.session().userCtx.roles.indexOf("_admin"));

    repDoc = {
      _id: "foo_rep_2",
      source: CouchDB.protocol + CouchDB.host + "/" + dbA.name,
      target: dbB.name,
      user_ctx: {
        name: "joe",
        roles: ["erlanger"]
      }
    };

    TEquals(true, repDb.save(repDoc).ok);
    CouchDB.logout();

    waitForRep(repDb, repDoc, "complete");
    repDoc1 = repDb.open(repDoc._id);
    T(repDoc1 !== null);
    TEquals(repDoc.source, repDoc1.source);
    TEquals(repDoc.target, repDoc1.target);
    TEquals("completed", repDoc1._replication_state);
    TEquals("string", typeof repDoc1._replication_id);
    TEquals("string", typeof repDoc1._replication_state_time);

    for (var i = 0; i < docs1.length; i++) {
      var doc = docs1[i];
      var copy = dbB.open(doc._id);

      T(copy !== null);
      TEquals(doc.value, copy.value);
    }

    // Admins don't need to supply a user_ctx property in replication docs.
    // If they do not, the implicit user_ctx "user_ctx": {name: null, roles: []}
    // is used, meaning that design documents will not be replicated into
    // local targets
    T(dbB.setSecObj({
      admins: {
        names: [],
        roles: []
      },
      readers: {
        names: [],
        roles: []
      }
    }).ok);

    var ddoc = { _id: "_design/foo" };
    TEquals(true, dbA.save(ddoc).ok);

    repDoc = {
      _id: "foo_rep_3",
      source: CouchDB.protocol + CouchDB.host + "/" + dbA.name,
      target: dbB.name
    };

    TEquals(true, repDb.save(repDoc).ok);
    waitForRep(repDb, repDoc, "complete");
    repDoc1 = repDb.open(repDoc._id);
    T(repDoc1 !== null);
    TEquals(repDoc.source, repDoc1.source);
    TEquals(repDoc.target, repDoc1.target);
    TEquals("completed", repDoc1._replication_state);
    TEquals("string", typeof repDoc1._replication_id);
    TEquals("string", typeof repDoc1._replication_state_time);

    var ddoc_copy = dbB.open(ddoc._id);
    T(ddoc_copy === null);

    repDoc = {
      _id: "foo_rep_4",
      source: CouchDB.protocol + CouchDB.host + "/" + dbA.name,
      target: dbB.name,
      user_ctx: {
        roles: ["_admin"]
      }
    };

    TEquals(true, repDb.save(repDoc).ok);
    waitForRep(repDb, repDoc, "complete");
    repDoc1 = repDb.open(repDoc._id);
    T(repDoc1 !== null);
    TEquals(repDoc.source, repDoc1.source);
    TEquals(repDoc.target, repDoc1.target);
    TEquals("completed", repDoc1._replication_state);
    TEquals("string", typeof repDoc1._replication_id);
    TEquals("string", typeof repDoc1._replication_state_time);

    ddoc_copy = dbB.open(ddoc._id);
    T(ddoc_copy !== null);
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
  run_on_modified_server(server_config, test_user_ctx_validation);

  // cleanup
  repDb.deleteDb();
  dbA.deleteDb();
  dbB.deleteDb();
  usersDb.deleteDb();
}