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

couchTests.replicator_db_security = function(debug) {
  var dbs = ["couch_test_rep_db", "couch_test_users_db",
    "test_suite_db_a", "test_suite_db_b", "test_suite_db_c"]
    .map(function(db_name) {
      var db = new CouchDB(db_name, {"X-Couch-Full-Commit":"false"});
      db.deleteDb();
      db.createDb();
      return db;
  });

  var repDb = dbs[0];
  var usersDb = dbs[1];
  var dbA = dbs[2];
  var dbB = dbs[3];
  var dbC = dbs[4];

  if (debug) debugger;

  var loginUser = function(username) {
    var pws = {
      jan: "apple",
      jchris: "mp3",
      fdmanana: "foobar",
      benoitc: "test"
    };
    T(CouchDB.login(username, pws[username]).ok);
  };

  var repChanges = function(username) {
    var pws = {
      jan: "apple",
      jchris: "mp3",
      fdmanana: "foobar",
      benoitc: "test"
    };
    T(CouchDB.login(username, pws[username]).ok);
    var changes = CouchDB.request(
      "GET",
       "/" + repDb.name + "/_changes?include_docs=true" +
         "&anti-cache=" + String(Math.round(Math.random() * 100000)));
    return changes = JSON.parse(changes.responseText);
  };

  var save_as = function(db, doc, username)
  {
    loginUser(username);
    try {
      return db.save(doc);
    } catch (ex) {
      return ex;
    } finally {
      CouchDB.logout();
    }
  };

  var open_as = function(db, docId, username) {
    loginUser(username);
    try {
      return db.open(docId);
    } finally {
      CouchDB.logout();
    }
  };

  // from test replicator_db.js
  function waitForDocPos(db, docId, pos) {
    var doc, curPos, t0, t1,
        maxWait = 3000;

    doc = db.open(docId);
    curPos = Number(doc._rev.split("-", 1));
    t0 = t1 = new Date();

    while ((curPos < pos) && ((t1 - t0) <= maxWait)) {
       doc = db.open(docId);
       curPos = Number(doc._rev.split("-", 1));
       t1 = new Date();
    }

    return doc;
  }

  var testFun = function()
  {
    // _replicator db
    // in admin party mode, anonymous should be able to create a replication
    var repDoc = {
      _id: "null-owner-rep",
      source: dbA.name,
      target: dbB.name
    };
    var result = repDb.save(repDoc);
    TEquals(true, result.ok, "should allow anonymous replication docs in admin party");
    // new docs should get an owner field enforced. In admin party mode owner is null
    repDoc = repDb.open(repDoc._id);
    TIsnull(repDoc.owner, "owner should be null in admin party");

// Uncomment when _users database security changes are implemented.
//
//     var jchrisDoc = {
//       _id: "org.couchdb.user:jchris",
//       type: "user",
//       name: "jchris",
//       password: "mp3",
//       roles: []
//     };
    var jchrisDoc = CouchDB.prepareUserDoc({
      name: "jchris",
      roles: []
    }, "mp3");
    usersDb.save(jchrisDoc); // set up a non-admin user

// Uncomment when _users database security changes are implemented.
//
//     var jchrisDoc = {
//       _id: "org.couchdb.user:fdmanana",
//       type: "user",
//       name: "fdmanana",
//       password: "foobar",
//       roles: []
//     };
    var fdmananaDoc = CouchDB.prepareUserDoc({
      name: "fdmanana",
      roles: []
    }, "foobar");
    usersDb.save(fdmananaDoc); // set up a non-admin user

// Uncomment when _users database security changes are implemented.
//
//     var benoitcDoc = {
//       _id: "org.couchdb.user:fdmanana",
//       type: "user",
//       name: "fdmanana",
//       password: "foobar",
//       roles: []
//     };
    var benoitcDoc = CouchDB.prepareUserDoc({
      name: "benoitc",
      roles: []
    }, "test");
    usersDb.save(benoitcDoc); // set up a non-admin user

    T(repDb.setSecObj({
      "admins" : {
        roles : [],
        names : ["benoitc"]
      }
    }).ok);
    
    run_on_modified_server([
        {
          section: "admins",
          key: "jan",
          value: "apple"
        }
      ], function() {
        // replication docs from admin-party mode in non-admin party mode can not
        //   be edited by non-admins (non-server admins)
        repDoc = repDb.open(repDoc._id);
        repDoc.target = dbC.name;
        var result = save_as(repDb, repDoc, "jchris");
        TEquals("forbidden", result.error, "should forbid editing null-owner docs");

        // replication docs from admin-party mode in non-admin party mode can only
        //   be edited by admins (server admins)
        repDoc = waitForDocPos(repDb, repDoc._id, 3);
        repDoc.target = dbC.name;
        var result = save_as(repDb, repDoc, "jan");
        repDoc = open_as(repDb, repDoc._id, "jchris");
        TEquals(true, result.ok, "should allow editing null-owner docs to admins");
        TEquals("jan", repDoc.owner, "owner should be the admin now");

        // user can update their own replication docs (repDoc.owner)
        var jchrisRepDoc = {
          _id: "jchris-rep-doc",
          source: dbC.name,
          target: dbA.name,
          user_ctx: { name: "jchris", roles: [] }
        };

        var result = save_as(repDb, jchrisRepDoc, "jchris");
        TEquals(true, result.ok, "should create rep doc");
        jchrisRepDoc = repDb.open(jchrisRepDoc._id);
        TEquals("jchris", jchrisRepDoc.owner, "should assign correct owner");
        jchrisRepDoc = waitForDocPos(repDb, jchrisRepDoc._id, 3);
        jchrisRepDoc = open_as(repDb, jchrisRepDoc._id, "jchris");
        jchrisRepDoc.target = dbB.name;
        var result = save_as(repDb, jchrisRepDoc, "jchris");
        TEquals(true, result.ok, "should allow update of rep doc");

        // user should not be able to read from any view
        var ddoc = {
          _id: "_design/reps",
          views: {
            test: {
            map: "function(doc) {" +
              "if (doc._replication_state) { " +
                "emit(doc._id, doc._replication_state);" +
              "}" +
            "}"
            }
          }
        };

        save_as(repDb, ddoc, "jan");

        try {
          repDb.view("reps/test");
          T(false, "non-admin had view read access");
        } catch (ex) {
          TEquals("forbidden", ex.error,
            "non-admins should not be able to read a view");
        }

        // admin should be able to read from any view
        TEquals(true, CouchDB.login("jan", "apple").ok);
        var result = repDb.view("reps/test");
        CouchDB.logout();
        TEquals(2, result.total_rows, "should allow access and list two users");

        // test _all_docs, only available for _admins
        try {
          repDb.allDocs({include_docs: true});
          T(false, "non-admin had _all_docs access");
        } catch (ex) {
          TEquals("forbidden", ex.error,
            "non-admins should not be able to access _all_docs");
        }

        TEquals(true, CouchDB.login("jan", "apple").ok);
        try {
          repDb.allDocs({include_docs: true});
        } catch (ex) {
          T(false, "admin couldn't access _all_docs");
        }
        CouchDB.logout();

        try {
          repDb.view("reps/test");
          T(false, "non-admin had view read access");
        } catch (ex) {
          TEquals("forbidden", ex.error,
            "non-admins should not be able to read a view");
        }

        // admin should be able to read from any view
        TEquals(true, CouchDB.login("benoitc", "test").ok);
        var result = repDb.view("reps/test");
        CouchDB.logout();
        TEquals(2, result.total_rows, "should allow access and list two users");

        // test _all_docs, only available for _admins
        try {
          repDb.allDocs({include_docs: true});
          T(false, "non-admin had _all_docs access");
        } catch (ex) {
          TEquals("forbidden", ex.error,
            "non-admins should not be able to access _all_docs");
        }

        TEquals(true, CouchDB.login("benoitc", "test").ok);
        try {
          repDb.allDocs({include_docs: true});
        } catch (ex) {
          T(false, "admin couldn't access _all_docs");
        }
        CouchDB.logout();

        // Verify that users can't access credentials in the "source" and
        // "target" fields of replication documents owned by other users.
        var fdmananaRepDoc = {
          _id: "fdmanana-rep-doc",
          source: "http://fdmanana:foobar@" + CouchDB.host + "/" + dbC.name,
          target: dbA.name,
          user_ctx: { name: "fdmanana", roles: [] }
        };

        var result = save_as(repDb, fdmananaRepDoc, "fdmanana");
        TEquals(true, result.ok, "should create rep doc");
        waitForDocPos(repDb, fdmananaRepDoc._id, 3);
        fdmananaRepDoc = open_as(repDb, fdmananaRepDoc._id, "fdmanana");
        TEquals("fdmanana", fdmananaRepDoc.owner, "should assign correct owner");
        TEquals("http://fdmanana:foobar@" + CouchDB.host + "/" + dbC.name,
           fdmananaRepDoc.source, "source field has credentials");

        fdmananaRepDoc = open_as(repDb, fdmananaRepDoc._id, "jchris");
        TEquals("fdmanana", fdmananaRepDoc.owner, "should assign correct owner");
        TEquals("http://" + CouchDB.host + "/" + dbC.name,
           fdmananaRepDoc.source, "source field doesn't contain credentials");

        // _changes?include_docs=true, users shouldn't be able to see credentials
        // in documents owned by other users.
        var changes = repChanges("jchris");
        var doc = changes.results[changes.results.length - 1].doc;
        TEquals(fdmananaRepDoc._id, doc._id, "Got the right doc from _changes");
        TEquals("http://" + CouchDB.host + "/" + dbC.name,
           doc.source, "source field doesn't contain credentials (doc from _changes)");
        CouchDB.logout();

        // _changes?include_docs=true, user should be able to see credentials
        // in documents they own.
        var changes = repChanges("fdmanana");
        var doc = changes.results[changes.results.length - 1].doc;
        TEquals(fdmananaRepDoc._id, doc._id, "Got the right doc from _changes");
        TEquals("http://fdmanana:foobar@" + CouchDB.host + "/" + dbC.name,
           doc.source, "source field contains credentials (doc from _changes)");
        CouchDB.logout();

        // _changes?include_docs=true, admins should be able to see credentials
        // from all documents.
        var changes = repChanges("jan");
        var doc = changes.results[changes.results.length - 1].doc;
        TEquals(fdmananaRepDoc._id, doc._id, "Got the right doc from _changes");
        TEquals("http://fdmanana:foobar@" + CouchDB.host + "/" + dbC.name,
           doc.source, "source field contains credentials (doc from _changes)");
        CouchDB.logout();

        // _changes?include_docs=true, db admins should be able to see credentials
        // from all documents.
        var changes = repChanges("benoitc");
        var doc = changes.results[changes.results.length - 1].doc;
        TEquals(fdmananaRepDoc._id, doc._id, "Got the right doc from _changes");
        TEquals("http://fdmanana:foobar@" + CouchDB.host + "/" + dbC.name,
           doc.source, "source field contains credentials (doc from _changes)");
        CouchDB.logout();

        var fdmananaRepDocOAuth = {
          _id: "fdmanana-rep-doc-oauth",
          source: dbC.name,
          target: {
            url: "http://" + CouchDB.host + "/" + dbA.name,
            oauth: {
              token: "abc",
              token_secret: "foo",
              consumer_key: "123",
              consumer_secret: "321"
            }
          },
          user_ctx: { name: "fdmanana", roles: [] }
        };

        var result = save_as(repDb, fdmananaRepDocOAuth, "fdmanana");
        TEquals(true, result.ok, "should create rep doc");
        waitForDocPos(repDb, fdmananaRepDocOAuth._id, 3);
        fdmananaRepDocOAuth = open_as(repDb, fdmananaRepDocOAuth._id, "fdmanana");
        TEquals("fdmanana", fdmananaRepDocOAuth.owner, "should assign correct owner");
        TEquals("object", typeof fdmananaRepDocOAuth.target.oauth,
          "target field has oauth credentials");

        fdmananaRepDocOAuth = open_as(repDb, fdmananaRepDocOAuth._id, "jchris");
        TEquals("fdmanana", fdmananaRepDocOAuth.owner, "should assign correct owner");
        TEquals("undefined", typeof fdmananaRepDocOAuth.target.oauth,
          "target field doesn't have oauth credentials");

        // ensure "old" replicator docs still work
        // done in replicator_db.js?

        // Login as admin so run_on_modified_server can do its cleanup.
        TEquals(true, CouchDB.login("jan", "apple").ok);
      });
  };

  usersDb.deleteDb();
  repDb.deleteDb();

  run_on_modified_server([
    {
      section: "couch_httpd_auth",
      key: "authentication_db",
      value: usersDb.name
    },
    {
      section: "replicator",
      key: "db",
      value: repDb.name
    }],
    testFun
  );

  // cleanup
  usersDb.deleteDb();
  repDb.deleteDb();
};
