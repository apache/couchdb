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

couchTests.users_db_security = function(debug) {
  var usersDb = new CouchDB("test_suite_users", {"X-Couch-Full-Commit":"false"});
  if (debug) debugger;


  var loginUser = function(username) {
    var pws = {
      jan: "apple",
      jchris: "mp3",
      jchris1: "couch",
      fdmanana: "foobar",
      benoitc: "test"
    };
    var username1 = username.replace(/[0-9]$/, "");
    var password = pws[username];
    //console.log("Logging in '" + username1 + "' with password '" + password + "'");
    T(CouchDB.login(username1, pws[username]).ok);
  };

  var open_as = function(db, docId, username) {
    loginUser(username);
    try {
      return db.open(docId, {"anti-cache": Math.round(Math.random() * 100000)});
    } finally {
      CouchDB.logout();
    }
  };

  var view_as = function(db, viewname, username) {
    loginUser(username);
    try {
      return db.view(viewname);
    } finally {
      CouchDB.logout();
    }
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

  var changes_as = function(db, username)
  {
    loginUser(username);
    try {
      return db.changes();
    } catch(ex) {
      return ex;
    } finally {
      CouchDB.logout();
    }
  };

  var testFun = function()
  {
    usersDb.deleteDb();

    // _users db
    // a doc with a field 'password' should be hashed to 'password_sha'
    //  with salt and salt stored in 'salt', 'password' is set to null.
    //  Exising 'password_sha' and 'salt' fields are overwritten with new values
    //  when a non-null 'password' field exists.
    // anonymous should be able to create a user document
    var userDoc = {
      _id: "org.couchdb.user:jchris",
      type: "user",
      name: "jchris",
      password: "mp3",
      roles: []
    };

    // jan's gonna be admin as he's the first user
    TEquals(true, usersDb.save(userDoc).ok, "should save document");
    userDoc = usersDb.open("org.couchdb.user:jchris");
    console.log(userDoc);
    TEquals(undefined, userDoc.password, "password field should be null 1");
    TEquals(40, userDoc.password_sha.length, "password_sha should exist");
    TEquals(32, userDoc.salt.length, "salt should exist");

    // create server admin
    run_on_modified_server([
        {
          section: "admins",
          key: "jan",
          value: "apple"
        }
      ], function() {

      // anonymous should not be able to read an existing user's user document
      var res = usersDb.open("org.couchdb.user:jchris");
      TEquals(null, res, "anonymous user doc read should be not found");

      // anonymous should not be able to read /_users/_changes
      try {
        var ch = usersDb.changes();
        T(false, "anonymous can read _changes");
      } catch(e) {
        TEquals("unauthorized", e.error, "anoymous can't read _changes");
      }

      // user should be able to read their own document
      var jchrisDoc = open_as(usersDb, "org.couchdb.user:jchris", "jchris");
      TEquals("org.couchdb.user:jchris", jchrisDoc._id);

      // user should not be able to read /_users/_changes
      var changes = changes_as(usersDb, "jchris");
      TEquals("unauthorized", changes.error, "user can't read _changes");

      // new 'password' fields should trigger new hashing routine
      jchrisDoc.password = "couch";

      TEquals(true, save_as(usersDb, jchrisDoc, "jchris").ok);
      var jchrisDoc = open_as(usersDb, "org.couchdb.user:jchris", "jchris1");

      TEquals(undefined, jchrisDoc.password, "password field should be null 2");
      TEquals(40, jchrisDoc.password_sha.length, "password_sha should exist");
      TEquals(32, jchrisDoc.salt.length, "salt should exist");

      TEquals(true, userDoc.salt != jchrisDoc.salt, "should have new salt");
      TEquals(true, userDoc.password_sha != jchrisDoc.password_sha,
        "should have new password_sha");

      // user should not be able to read another user's user document
      var fdmananaDoc = {
        _id: "org.couchdb.user:fdmanana",
        type: "user",
        name: "fdmanana",
        password: "foobar",
        roles: []
      };

      usersDb.save(fdmananaDoc);

      var fdmananaDocAsReadByjchris =
        open_as(usersDb, "org.couchdb.user:fdmanana", "jchris1");
      TEquals(null, fdmananaDocAsReadByjchris,
        "should not_found opening another user's user doc");


      // save a db admin
      var benoitcDoc = {
        _id: "org.couchdb.user:benoitc",
        type: "user",
        name: "benoitc",
        password: "test",
        roles: ["user_admin"]
      };
      save_as(usersDb, benoitcDoc, "jan");

      TEquals(true, CouchDB.login("jan", "apple").ok);
      T(usersDb.setSecObj({
        "admins" : {
          roles : [],
          names : ["benoitc"]
        }
      }).ok);
      CouchDB.logout();

      // user should not be able to read from any view
      var ddoc = {
        _id: "_design/user_db_auth",
        views: {
          test: {
            map: "function(doc) { emit(doc._id, null); }"
          }
        }
      };

      save_as(usersDb, ddoc, "jan");

      try {
        usersDb.view("user_db_auth/test");
        T(false, "user had access to view in admin db");
      } catch(e) {
        TEquals("forbidden", e.error,
        "non-admins should not be able to read a view");
      }

      // admin should be able to read from any view
      var result = view_as(usersDb, "user_db_auth/test", "jan");
      TEquals(3, result.total_rows, "should allow access and list two users to admin");

      // db admin should be able to read from any view
      var result = view_as(usersDb, "user_db_auth/test", "benoitc");
      TEquals(3, result.total_rows, "should allow access and list two users to db admin");


      // non-admins can't read design docs
      try {
        open_as(usersDb, "_design/user_db_auth", "jchris1");
        T(false, "non-admin read design doc, should not happen");
      } catch(e) {
        TEquals("forbidden", e.error, "non-admins can't read design docs");
      }

      // admin should be able to read and edit any user doc
      fdmananaDoc.password = "mobile";
      var result = save_as(usersDb, fdmananaDoc, "jan");
      TEquals(true, result.ok, "admin should be able to update any user doc");

      // admin should be able to read and edit any user doc
      fdmananaDoc.password = "mobile1";
      var result = save_as(usersDb, fdmananaDoc, "benoitc");
      TEquals(true, result.ok, "db admin by role should be able to update any user doc");

      TEquals(true, CouchDB.login("jan", "apple").ok);
      T(usersDb.setSecObj({
        "admins" : {
          roles : ["user_admin"],
          names : []
        }
      }).ok);
      CouchDB.logout();

      // db admin should be able to read and edit any user doc
      fdmananaDoc.password = "mobile2";
      var result = save_as(usersDb, fdmananaDoc, "benoitc");
      TEquals(true, result.ok, "db admin should be able to update any user doc");

      // ensure creation of old-style docs still works
      var robertDoc = CouchDB.prepareUserDoc({ name: "robert" }, "anchovy");
      var result = usersDb.save(robertDoc);
      TEquals(true, result.ok, "old-style user docs should still be accepted");

      // log in one last time so run_on_modified_server can clean up the admin account
      TEquals(true, CouchDB.login("jan", "apple").ok);
    });
  };

  usersDb.deleteDb();
  run_on_modified_server(
    [{section: "couch_httpd_auth",
      key: "authentication_db", value: usersDb.name}],
    testFun
  );
  usersDb.deleteDb(); // cleanup

};
