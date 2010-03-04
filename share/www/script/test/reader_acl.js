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

couchTests.reader_acl = function(debug) {
  // this tests read access control

  var usersDb = new CouchDB("test_suite_users", {"X-Couch-Full-Commit":"false"});
  var secretDb = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"false"});
  function testFun() {
    try {
      usersDb.deleteDb();
      usersDb.createDb();
      secretDb.deleteDb();
      secretDb.createDb();

      // create a user with top-secret-clearance
      var jchrisUserDoc = CouchDB.prepareUserDoc({
        name: "jchris@apache.org",
        roles : ["top-secret"]
      }, "funnybone");
      T(usersDb.save(jchrisUserDoc).ok);

      T(CouchDB.session().userCtx.name == null);

      // set secret db to be read controlled
      T(secretDb.save({_id:"baz",foo:"bar"}).ok);
      T(secretDb.open("baz").foo == "bar");

      T(secretDb.setSecObj({
        "readers" : {
          roles : ["super-secret-club"],
          names : ["joe","barb"]
        }
      }).ok);
      
      usersDb.ensureFullCommit();
      // security changes will always commit synchronously
      restartServer();
      
      // can't read it as jchris
      T(CouchDB.login("jchris@apache.org", "funnybone").ok);
      T(CouchDB.session().userCtx.name == "jchris@apache.org");

      try {
        secretDb.open("baz");
        T(false && "can't open a doc from a secret db") ;
      } catch(e) {
        T(true)
      }

      CouchDB.logout();
      
      // make anyone with the top-secret role an admin
      // db admins are automatically readers
      T(secretDb.setSecObj({
        "admins" : {
          roles : ["top-secret"],
          names : []
        },
        "readers" : {
          roles : ["super-secret-club"],
          names : ["joe","barb"]
        }
      }).ok);

      T(CouchDB.login("jchris@apache.org", "funnybone").ok);

      T(secretDb.open("baz").foo == "bar");

      CouchDB.logout();
      T(CouchDB.session().userCtx.roles.indexOf("_admin") != -1);

      // admin now adds the top-secret role to the db's readers
      // and removes db-admins
      T(secretDb.setSecObj({
        "admins" : {
          roles : [],
          names : []
        },
        "readers" : {
          roles : ["super-secret-club", "top-secret"],
          names : ["joe","barb"]
        }
      }).ok);

      // server _admin can always read
      T(secretDb.open("baz").foo == "bar");

      // and run temp views
      TEquals(secretDb.query(function(doc) {
        emit(null, null)
      }).total_rows, 1);

      T(secretDb.save({
        "_id" : "_design/foo",
        views : {
          bar : {
            map : "function(doc){emit(null, null)}"
          }
        }
      }).ok)

      // now top-secret users can read too
      T(CouchDB.login("jchris@apache.org", "funnybone").ok);
      T(CouchDB.session().userCtx.roles.indexOf("_admin") == -1);
      T(secretDb.open("baz").foo == "bar");
      // readers can query stored views
      T(secretDb.view("foo/bar").total_rows == 1);
      
      CouchDB.logout();

      // can't set non string reader names or roles
      try {
        secretDb.setSecObj({
          "readers" : {
            roles : ["super-secret-club", {"top-secret":"awesome"}],
            names : ["joe","barb"]
          }
        })
        T(false && "only string roles");
      } catch (e) {}

      try {
        secretDb.setSecObj({
          "readers" : {
            roles : ["super-secret-club", {"top-secret":"awesome"}],
            names : ["joe",22]
          }
        });
        T(false && "only string names");
      } catch (e) {}
      
      try {
        secretDb.setSecObj({
          "readers" : {
            roles : ["super-secret-club", {"top-secret":"awesome"}],
            names : "joe"
          }
        });
        T(false && "only lists of names");
      } catch (e) {}
    } finally {
      CouchDB.logout();
    }
  }

  run_on_modified_server(
    [{section: "httpd",
      key: "authentication_handlers",
      value: "{couch_httpd_auth, cookie_authentication_handler}, {couch_httpd_auth, default_authentication_handler}"},
     {section: "couch_httpd_auth",
      key: "authentication_db", value: "test_suite_users"}],
    testFun
  );
}
