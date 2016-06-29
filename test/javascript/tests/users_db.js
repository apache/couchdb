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

couchTests.users_db = function(debug) {

  // This tests the users db, especially validations
  // this should also test that you can log into the couch
  
  var users_db_name = '_users';
  var usersDb = new CouchDB(users_db_name, {"X-Couch-Full-Commit":"false"});
  try { usersDb.createDb(); } catch (e) { /* ignore if exists*/ }
  // have a 2nd "normal" DB 2 provoke conflicts
  var usersDbAlt = new CouchDB(get_random_db_name(), {"X-Couch-Full-Commit":"false"});
  usersDbAlt.createDb();

  // test that you can treat "_user" as a db-name
  // this can complicate people who try to secure the users db with 
  // an http proxy and fail to get both the actual db and the _user path
  // maybe it's not the right approach...
  // hard to know what else to do, as we don't let non-admins inspect the config
  // to determine the actual users db name.

  function testFun() {

    // test that the validation function is installed
    // this will fail When the test is run in isolation,
    // since it doesn’t wait for the ddoc to be created.
    // in a full test suite run, this is fine.
    // dev trick: run `test/javascript/run basics users_db`
    // var ddoc = usersDb.open("_design/_auth");
    // T(ddoc.validate_doc_update);
    
    // test that you can login as a user using basic auth
    var jchrisUserDoc = CouchDB.prepareUserDoc({
      name: "jchris@apache.org"
    }, "funnybone");
    T(usersDb.save(jchrisUserDoc).ok);
    
    T(CouchDB.session().userCtx.name == null);

    // test that you can use basic auth aginst the users db
    var s = CouchDB.session({
      headers : {
        //                 base64_encode("jchris@apache.org:funnybone")
        "Authorization" : "Basic amNocmlzQGFwYWNoZS5vcmc6ZnVubnlib25l"
      }
    });
    T(s.userCtx.name == "jchris@apache.org");
    T(s.info.authenticated == "default");
    T(s.info.authentication_db == "" + users_db_name + "");
    TEquals(["cookie", "default", "local"], s.info.authentication_handlers);
    var s = CouchDB.session({
      headers : {
        "Authorization" : "Basic Xzpf" // name and pass of _:_
      }
    });
    T(s.name == null);
    T(s.info.authenticated == "local");
    CouchDB.logout();
    
    // ok, now create a conflicting edit on the jchris doc, and make sure there's no login.
    // (use replication to create the conflict) - need 2 be admin
    CouchDB.login("jan", "apple");
    CouchDB.replicate(usersDb.name, usersDbAlt.name);
    // save in one DB
    var jchrisUser2 = JSON.parse(JSON.stringify(jchrisUserDoc));
    jchrisUser2.foo = "bar";

    T(usersDb.save(jchrisUser2).ok);
    try {
      usersDb.save(jchrisUserDoc);
      T(false && "should be an update conflict");
    } catch(e) {
      T(true);
    }

    // then in the other
    var jchrisUser3 = JSON.parse(JSON.stringify(jchrisUserDoc));
    jchrisUser3.foo = "barrrr";
    T(usersDbAlt.save(jchrisUser3).ok);
    CouchDB.replicate(usersDbAlt.name, usersDb.name); // now we should have a conflict

    var jchrisWithConflict = usersDb.open(jchrisUserDoc._id, {conflicts : true});
    T(jchrisWithConflict._conflicts.length == 1);
    CouchDB.logout();

    wait(5000) // wait for auth_cache invalidation

    // no login with conflicted user doc
    try {
      var s = CouchDB.session({
        headers : {
          "Authorization" : "Basic amNocmlzQGFwYWNoZS5vcmc6ZnVubnlib25l"
        }
      });
      T(false && "this will throw");
    } catch(e) {
      T(e.error == "unauthorized");
      T(/conflict/.test(e.reason));
    }

    // you can delete a user doc
    // there is NO admin party here - so we have to login again
    CouchDB.login("jan", "apple");
    s = CouchDB.session().userCtx;
    //T(s.name == null);
    //console.log(JSON.stringify(usersDb.allDocs()));
    T(s.roles.indexOf("_admin") !== -1);
    T(usersDb.deleteDoc(jchrisWithConflict).ok);

    // you can't change doc from type "user"
    jchrisUserDoc = usersDb.open(jchrisUserDoc._id);
    jchrisUserDoc.type = "not user";
    try {
      usersDb.save(jchrisUserDoc);
      T(false && "should only allow us to save doc when type == 'user'");
    } catch(e) {
      T(e.reason == "doc.type must be user");
    }
    jchrisUserDoc.type = "user";

    // "roles" must be an array
    jchrisUserDoc.roles = "not an array";
    try {
      usersDb.save(jchrisUserDoc);
      T(false && "should only allow us to save doc when roles is an array");
    } catch(e) {
      T(e.reason == "doc.roles must be an array");
    }
    jchrisUserDoc.roles = [];

    // "roles" must be an array of strings
    jchrisUserDoc.roles = [12];
    try {
      usersDb.save(jchrisUserDoc);
      T(false && "should only allow us to save doc when roles is an array of strings");
    } catch(e) {
      TEquals(e.reason, "doc.roles can only contain strings");
    }
    jchrisUserDoc.roles = [];

    // "roles" must exist
    delete jchrisUserDoc.roles;
    try {
      usersDb.save(jchrisUserDoc);
      T(false && "should only allow us to save doc when roles exists");
    } catch(e) {
      T(e.reason == "doc.roles must exist");
    }
    jchrisUserDoc.roles = [];

    // character : is not allowed in usernames
    var joeUserDoc = CouchDB.prepareUserDoc({
      name: "joe:erlang"
    }, "qwerty");
    try {
      usersDb.save(joeUserDoc);
      T(false, "shouldn't allow : in usernames");
    } catch(e) {
      TEquals("Character `:` is not allowed in usernames.", e.reason);
    }

    // test that you can login as a user with a password starting with :
    var doc = CouchDB.prepareUserDoc({
      name: "foo@example.org"
    }, ":bar");
    T(usersDb.save(doc).ok);
    CouchDB.logout();

    T(CouchDB.session().userCtx.name == null);

    // test that you can use basic auth aginst the users db
    var s = CouchDB.session({
      headers : {
        //                 base64_encode("foo@example.org::bar")
        "Authorization" : "Basic Zm9vQGV4YW1wbGUub3JnOjpiYXI="
      }
    });
    T(s.userCtx.name == "foo@example.org");
    CouchDB.logout();

    // log in one last time so run_on_modified_server can clean up the admin account
    TEquals(true, CouchDB.login("jan", "apple").ok);
  };

  run_on_modified_server(
    [{section: "couch_httpd_auth",
      key: "iterations", value: "1"},
     {section: "admins",
      key: "jan", value: "apple"}],
    testFun
  );

  usersDbAlt.deleteDb(); // cleanup
  usersDb.deleteDb();
}
