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
    usersDb.deleteDb();
    
    // test that the validation function is installed
    var ddoc = usersDb.open("_design/_auth");
    T(ddoc.validate_doc_update);
    
    // test that you can login as a user using basic auth
    var jchrisUserDoc = CouchDB.prepareUserDoc({
      username: "jchris@apache.org"
    }, "funnybone");
    T(usersDb.save(jchrisUserDoc).ok);
    
    T(CouchDB.session().name == null);
    var s = CouchDB.session({
      headers : {
        "Authorization" : "Basic amNocmlzQGFwYWNoZS5vcmc6ZnVubnlib25l"
      }
    });
    T(s.name == "jchris@apache.org");
    T(s.user_doc._id == "org.couchdb.user:jchris@apache.org")
    T(s.info.authenticated == "{couch_httpd_auth, default_authentication_handler}");
    T(s.info.user_db == "test_suite_users");
    TEquals(["{couch_httpd_oauth, oauth_authentication_handler}", 
      "{couch_httpd_auth, cookie_authentication_handler}", 
      "{couch_httpd_auth, default_authentication_handler}"], s.info.handlers);
    var s = CouchDB.session({
      headers : {
        "Authorization" : "Basic Xzpf" // username and pass of _:_
      }
    });
    T(s.name == null);
    T(s.info.authenticated == "{couch_httpd_auth, default_authentication_handler}");
  };
  
  run_on_modified_server(
    [{section: "couch_httpd_auth",
      key: "authentication_db", value: "test_suite_users"}],
    testFun
  );
  
}