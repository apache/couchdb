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
  
  var usersDb = new CouchDB("test_suite_users", {"X-Couch-Full-Commit":"false"});

  // test that you can treat "_user" as a db-name
  // this can complicate people who try to secure the users db with 
  // an http proxy and fail to get both the actual db and the _user path
  // maybe it's not the right approach...
  // hard to know what else to do, as we don't let non-admins inspect the config
  // to determine the actual users db name.

  function testFun() {
    // test that the validation function is installed
    var ddoc = usersDb.open("_design/_auth");
    T(ddoc.validate_doc_update);
    
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
    T(s.info.authentication_db == "test_suite_users");
    TEquals(["oauth", "cookie", "default"], s.info.authentication_handlers);
    var s = CouchDB.session({
      headers : {
        "Authorization" : "Basic Xzpf" // name and pass of _:_
      }
    });
    T(s.name == null);
    T(s.info.authenticated == "default");
    
    
    // ok, now create a conflicting edit on the jchris doc, and make sure there's no login.
    var jchrisUser2 = JSON.parse(JSON.stringify(jchrisUserDoc));
    jchrisUser2.foo = "bar";
    T(usersDb.save(jchrisUser2).ok);
    try {
      usersDb.save(jchrisUserDoc);
      T(false && "should be an update conflict");
    } catch(e) {
      T(true);
    }
    // save as bulk with new_edits=false to force conflict save
    var resp = usersDb.bulkSave([jchrisUserDoc],{all_or_nothing : true});
    
    var jchrisWithConflict = usersDb.open(jchrisUserDoc._id, {conflicts : true});
    T(jchrisWithConflict._conflicts.length == 1);
    
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
    s = CouchDB.session().userCtx;
    T(s.name == null);
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
  };

  usersDb.deleteDb();
  run_on_modified_server(
    [{section: "couch_httpd_auth",
      key: "authentication_db", value: usersDb.name}],
    testFun
  );
  usersDb.deleteDb(); // cleanup
  
}
