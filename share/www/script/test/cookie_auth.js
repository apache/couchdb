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
  
  var db = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"false"});
  db.deleteDb();
  db.createDb();
  if (debug) debugger;

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
      // try using an invalid cookie
      var usersDb = new CouchDB("test_suite_users", {"X-Couch-Full-Commit":"false"});
      usersDb.deleteDb();
      usersDb.createDb();
      
      // test that the users db is born with the auth ddoc
      var ddoc = usersDb.open("_design/_auth");
      T(ddoc.validate_doc_update);
      
      // TODO test that changing the config so an existing db becomes the users db installs the ddoc also
      
      var password = "3.141592653589";

      // Create a user
      var jasonUserDoc = CouchDB.prepareUserDoc({
        name: "Jason Davies",
        roles: ["dev"]
      }, password);
      T(usersDb.save(jasonUserDoc).ok);      
      
      var checkDoc = usersDb.open(jasonUserDoc._id);
      T(checkDoc.name == "Jason Davies");
      
      var jchrisUserDoc = CouchDB.prepareUserDoc({
        name: "jchris@apache.org"
      }, "funnybone");
      T(usersDb.save(jchrisUserDoc).ok);

      // make sure we cant create duplicate users
      var duplicateJchrisDoc = CouchDB.prepareUserDoc({
        name: "jchris@apache.org"
      }, "eh, Boo-Boo?");

      try {
        usersDb.save(duplicateJchrisDoc);
        T(false && "Can't create duplicate user names. Should have thrown an error.");
      } catch (e) {
        T(e.error == "conflict");
        T(usersDb.last_req.status == 409);
      }
      
      // we can't create _names
      var underscoreUserDoc = CouchDB.prepareUserDoc({
        name: "_why"
      }, "copperfield");

      try {
        usersDb.save(underscoreUserDoc);
        T(false && "Can't create underscore user names. Should have thrown an error.");
      } catch (e) {
        T(e.error == "forbidden");
        T(usersDb.last_req.status == 403);
      }
      
      // we can't create docs with malformed ids
      var badIdDoc = CouchDB.prepareUserDoc({
        name: "foo"
      }, "bar");
      
      badIdDoc._id = "org.apache.couchdb:w00x";

      try {
        usersDb.save(badIdDoc);
        T(false && "Can't create malformed docids. Should have thrown an error.");
      } catch (e) {
        T(e.error == "forbidden");
        T(usersDb.last_req.status == 403);
      }
      
      // login works
      T(CouchDB.login('Jason Davies', password).ok);
      T(CouchDB.session().userCtx.name == 'Jason Davies');
      
      // JSON login works
      var xhr = CouchDB.request("POST", "/_session", {
        headers: {"Content-Type": "application/json"},
        body: JSON.stringify({
          name: 'Jason Davies',
          password: password
        })
      });

      T(JSON.parse(xhr.responseText).ok);
      T(CouchDB.session().userCtx.name == 'Jason Davies');

      // update one's own credentials document
      jasonUserDoc.foo=2;
      T(usersDb.save(jasonUserDoc).ok);
      T(CouchDB.session().userCtx.roles.indexOf("_admin") == -1);
      // can't delete another users doc unless you are admin
      try {
        usersDb.deleteDoc(jchrisUserDoc);
        T(false && "Can't delete other users docs. Should have thrown an error.");
      } catch (e) {
        T(e.error == "forbidden");
        T(usersDb.last_req.status == 403);
      }

      // TODO should login() throw an exception here?
       T(!CouchDB.login('Jason Davies', "2.71828").ok);
       T(!CouchDB.login('Robert Allen Zimmerman', 'd00d').ok);

       // a failed login attempt should log you out
       T(CouchDB.session().userCtx.name != 'Jason Davies');

       // test redirect
       xhr = CouchDB.request("POST", "/_session?next=/", {
         headers: {"Content-Type": "application/x-www-form-urlencoded"},
         body: "name=Jason%20Davies&password="+encodeURIComponent(password)
       });
       // should this be a redirect code instead of 200?
       // The cURL adapter is returning the expected 302 here.
       // I imagine this has to do with whether the client is willing
       // to follow the redirect, ie, the browser follows and does a
       // GET on the returned Location
       if (xhr.status == 200) {
         T(/Welcome/.test(xhr.responseText));
       } else {
         T(xhr.status == 302);
         T(xhr.getResponseHeader("Location"));
       }

      // test users db validations
      // 
      // test that you can't update docs unless you are logged in as the user (or are admin)
      T(CouchDB.login("jchris@apache.org", "funnybone").ok);
      T(CouchDB.session().userCtx.name == "jchris@apache.org");
      T(CouchDB.session().userCtx.roles.length == 0);
      
      jasonUserDoc.foo=3;

      try {
        usersDb.save(jasonUserDoc);
        T(false && "Can't update someone else's user doc. Should have thrown an error.");
      } catch (e) {
        T(e.error == "forbidden");
        T(usersDb.last_req.status == 403);
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
      T(CouchDB.session().userCtx.roles[0] == "_admin");      

      jchrisUserDoc.foo = ["foo"];
      T(usersDb.save(jchrisUserDoc).ok);

      // test that you can't save system (underscore) roles even if you are admin
      jchrisUserDoc.roles = ["_bar"];
      
      try {
        usersDb.save(jchrisUserDoc);
        T(false && "Can't add system roles to user's db. Should have thrown an error.");
      } catch (e) {
        T(e.error == "forbidden");
        T(usersDb.last_req.status == 403);
      }
      
      // make sure the foo role has been applied
      T(CouchDB.login("jchris@apache.org", "funnybone").ok);
      T(CouchDB.session().userCtx.name == "jchris@apache.org");
      T(CouchDB.session().userCtx.roles.indexOf("_admin") == -1);
      T(CouchDB.session().userCtx.roles.indexOf("foo") != -1);
      
      // now let's make jchris a server admin
      T(CouchDB.logout().ok);
      T(CouchDB.session().userCtx.roles[0] == "_admin");
      T(CouchDB.session().userCtx.name == null);
      
      // set the -hashed- password so the salt matches
      // todo ask on the ML about this
      run_on_modified_server([{section: "admins",
        key: "jchris@apache.org", value: "funnybone"}], function() {
          T(CouchDB.login("jchris@apache.org", "funnybone").ok);
          T(CouchDB.session().userCtx.name == "jchris@apache.org");
          T(CouchDB.session().userCtx.roles.indexOf("_admin") != -1);
          // test that jchris still has the foo role
          T(CouchDB.session().userCtx.roles.indexOf("foo") != -1);

          // should work even when user doc has no password
          jchrisUserDoc = usersDb.open(jchrisUserDoc._id);
          delete jchrisUserDoc.salt;
          delete jchrisUserDoc.password_sha;
          T(usersDb.save(jchrisUserDoc).ok);
          T(CouchDB.logout().ok);
          T(CouchDB.login("jchris@apache.org", "funnybone").ok);
          var s = CouchDB.session();
          T(s.userCtx.name == "jchris@apache.org");
          T(s.userCtx.roles.indexOf("_admin") != -1);
          // test session info
          T(s.info.authenticated == "cookie");
          T(s.info.authentication_db == "test_suite_users");
          // test that jchris still has the foo role
          T(CouchDB.session().userCtx.roles.indexOf("foo") != -1);
        });      

    } finally {
      // Make sure we erase any auth cookies so we don't affect other tests
      T(CouchDB.logout().ok);
    }
  };

  run_on_modified_server(
    [{section: "httpd",
      key: "authentication_handlers",
      value: "{couch_httpd_auth, cookie_authentication_handler}, {couch_httpd_auth, default_authentication_handler}"},
     {section: "couch_httpd_auth",
      key: "secret", value: generateSecret(64)},
     {section: "couch_httpd_auth",
      key: "authentication_db", value: "test_suite_users"}],
    testFun
  );

};
