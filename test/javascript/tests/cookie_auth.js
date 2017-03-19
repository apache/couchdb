// Licensed under the Apache License, Version 2.0 (the "License"); you may not
// use this file except in compliance with the License.  You may obtain a copy
// of the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
// WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
// License for the specific language governing permissions and limitations under
// the License.

couchTests.cookie_auth = function(debug) {
  // This tests cookie-based authentication.

  var db_name = get_random_db_name();
  var db = new CouchDB(db_name, {"X-Couch-Full-Commit":"false"});
  db.createDb();

  // used later, needs to be global here
  var users_db_name = '_users';
  var usersDb = new CouchDB(users_db_name, {"X-Couch-Full-Commit":"false"});
  try { usersDb.createDb(); } catch (e) { /* ignore if exists*/ }

  if (debug) debugger;

  var password = "3.141592653589";

  var loginUser = function(username) {
    var pws = {
      jan: "apple",
      "Jason Davies": password,
      jchris: "funnybone"
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

  // Simple secret key generator
  function generateSecret(length) {
    var tab = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
    var secret = '';
    for (var i=0; i<length; i++) {
      secret += tab.charAt(Math.floor(Math.random() * 64));
    }
    return secret;
  }

  // this function will be called on the modified server
  var testFun = function () {
    try {

      // test that the users db is born with the auth ddoc
      var ddoc = open_as(usersDb, "_design/_auth", "jan");
      T(ddoc && ddoc.validate_doc_update);

      // TODO test that changing the config so an existing db becomes the users db installs the ddoc also

      // Create a user
      var jasonUserDoc = CouchDB.prepareUserDoc({
        name: "Jason Davies"
      }, password);
      T(usersDb.save(jasonUserDoc).ok);

      var checkDoc = open_as(usersDb, jasonUserDoc._id, "jan");
      TEquals("Jason Davies", checkDoc.name);

      var jchrisUserDoc = CouchDB.prepareUserDoc({
        name: "jchris"
      }, "funnybone");
      T(usersDb.save(jchrisUserDoc).ok);

      // make sure we cant create duplicate users
      var duplicateJchrisDoc = CouchDB.prepareUserDoc({
        name: "jchris"
      }, "eh, Boo-Boo?");

      try {
        usersDb.save(duplicateJchrisDoc);
        T(false && "Can't create duplicate user names. Should have thrown an error.");
      } catch (e) {
        TEquals("conflict", e.error);
        TEquals(409, usersDb.last_req.status);
      }

      // we can't create _names
      var underscoreUserDoc = CouchDB.prepareUserDoc({
        name: "_why"
      }, "copperfield");

      try {
        usersDb.save(underscoreUserDoc);
        T(false && "Can't create underscore user names. Should have thrown an error.");
      } catch (e) {
        TEquals("forbidden", e.error);
        TEquals(403, usersDb.last_req.status);
      }

      // we can't create docs with malformed ids
      var badIdDoc = CouchDB.prepareUserDoc({
        name: "w00x"
      }, "bar");

      badIdDoc._id = "org.apache.couchdb:w00x";

      try {
        usersDb.save(badIdDoc);
        T(false && "Can't create malformed docids. Should have thrown an error.");
      } catch (e) {
        TEquals("forbidden", e.error);
        TEquals(403, usersDb.last_req.status);
      }

      // login works
      T(CouchDB.login('Jason Davies', password).ok);
      TEquals('Jason Davies', CouchDB.session().userCtx.name);

      // JSON login works
      var xhr = CouchDB.request("POST", "/_session", {
        headers: {"Content-Type": "application/json"},
        body: JSON.stringify({
          name: 'Jason Davies',
          password: password
        })
      });

      T(JSON.parse(xhr.responseText).ok);
      TEquals('Jason Davies', CouchDB.session().userCtx.name);

      // update one's own credentials document
      jasonUserDoc.foo=2;
      T(usersDb.save(jasonUserDoc).ok);
      T(CouchDB.session().userCtx.roles.indexOf("_admin") == -1);
      // can't delete another users doc unless you are admin
      try {
        usersDb.deleteDoc(jchrisUserDoc);
        T(false && "Can't delete other users docs. Should have thrown an error.");
      } catch (e) {
        TEquals("not_found", e.error);
        TEquals(404, usersDb.last_req.status);
      }

      // TODO should login() throw an exception here?
       T(!CouchDB.login('Jason Davies', "2.71828").ok);
       T(!CouchDB.login('Robert Allen Zimmerman', 'd00d').ok);

       // a failed login attempt should log you out
       T(CouchDB.session().userCtx.name != 'Jason Davies');

       // test redirect on success
       xhr = CouchDB.request("POST", "/_session?next=/", {
         headers: {"Content-Type": "application/x-www-form-urlencoded"},
         body: "name=Jason%20Davies&password="+encodeURIComponent(password)
       });
       // the browser should transparently follow the redirect and GET the server root (/)
       // see http://dev.w3.org/2006/webapi/XMLHttpRequest/#infrastructure-for-the-send-method
       if (xhr.status == 200) {
         T(/Welcome/.test(xhr.responseText))
       }

       // test redirect on fail
       xhr = CouchDB.request("POST", "/_session?fail=/", {
         headers: {"Content-Type": "application/x-www-form-urlencoded"},
         body: "name=Jason%20Davies&password=foobar"
       });
       if (xhr.status == 200) {
         T(/Welcome/.test(xhr.responseText));
       }

      // test users db validations
      //
      // test that you can't update docs unless you are logged in as the user (or are admin)
      T(CouchDB.login("jchris", "funnybone").ok);
      T(CouchDB.session().userCtx.name == "jchris");
      T(CouchDB.session().userCtx.roles.length == 0);

      jasonUserDoc.foo=3;

      try {
        usersDb.save(jasonUserDoc);
        T(false && "Can't update someone else's user doc. Should have thrown an error.");
      } catch (e) {
        T(e.error == "not_found");
        T(usersDb.last_req.status == 404);
      }

      // test that you can't edit roles unless you are admin
      jchrisUserDoc.roles = ["foo"];

      try {
        usersDb.save(jchrisUserDoc);
        T(false && "Can't set roles unless you are admin. Should have thrown an error.");
      } catch (e) {
        T(e.error == "forbidden");
        T(usersDb.last_req.status == 403);
      }

      T(CouchDB.logout().ok);

      jchrisUserDoc.foo = ["foo"];
      T(save_as(usersDb, jchrisUserDoc, "jan"));
      wait(5000) // wait for auth cache invalidation

      // test that you can't save system (underscore) roles even if you are admin
      jchrisUserDoc.roles = ["_bar"];

      var res = save_as(usersDb, jchrisUserDoc, "jan");
      T(res.error == "forbidden");
      T(usersDb.last_req.status == 403);

      // make sure the foo role has been applied
      T(CouchDB.login("jchris", "funnybone").ok);
      T(CouchDB.session().userCtx.name == "jchris");
      T(CouchDB.session().userCtx.roles.indexOf("_admin") == -1);
      T(CouchDB.session().userCtx.roles.indexOf("foo") != -1);

      // now let's make jchris a server admin
      T(CouchDB.logout().ok);

      // set the -hashed- password so the salt matches
      // todo ask on the ML about this

      TEquals(true, CouchDB.login("jan", "apple").ok);
      run_on_modified_server([{section: "admins",
        key: "jchris", value: "funnybone"}], function() {
          T(CouchDB.login("jchris", "funnybone").ok);
          T(CouchDB.session().userCtx.name == "jchris");
          T(CouchDB.session().userCtx.roles.indexOf("_admin") != -1);
          // test that jchris still has the foo role
          T(CouchDB.session().userCtx.roles.indexOf("foo") != -1);

          // should work even when user doc has no password
          jchrisUserDoc = usersDb.open(jchrisUserDoc._id);
          delete jchrisUserDoc.salt;
          delete jchrisUserDoc.password_sha;
          T(usersDb.save(jchrisUserDoc).ok);
          T(CouchDB.logout().ok);
          T(CouchDB.login("jchris", "funnybone").ok);
          var s = CouchDB.session();
          T(s.userCtx.name == "jchris");
          T(s.userCtx.roles.indexOf("_admin") != -1);
          // test session info
          T(s.info.authenticated == "cookie");
          T(s.info.authentication_db == users_db_name);
          // test that jchris still has the foo role
          T(CouchDB.session().userCtx.roles.indexOf("foo") != -1);
        });

    } finally {
      // Make sure we erase any auth cookies so we don't affect other tests
      T(CouchDB.logout().ok);
    }
    // log in one last time so run_on_modified_server can clean up the admin account
    TEquals(true, CouchDB.login("jan", "apple").ok);
  };

  // per se, _users is born with a ddoc
  // problem is: the birth seems async and it takes some time till it is there. We do know, however, that it WILL. So: use _changes 2 our advantage
  var users_db_chg = CouchDB.request("GET", users_db_name + "/_changes?feed=longpoll&timeout=5000&filter=_design");
  T(users_db_chg.responseText);
  // now we should be safe
  run_on_modified_server(
    [
     {section: "couch_httpd_auth",
      key: "authentication_db", value: users_db_name},
     {section: "chttpd_auth",
      key: "authentication_db", value: users_db_name},
     {section: "couch_httpd_auth",
      key: "iterations", value: "1"},
     {section: "admins",
       key: "jan", value: "apple"}
    ],
    testFun
  );

  // cleanup
  db.deleteDb();
  usersDb.deleteDb();
};
