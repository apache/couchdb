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
      
      var password = "3.141592653589";

      // Create a user
      T(usersDb.save({
        _id: "a1",
        salt: "123",
        password_sha: hex_sha1(password + "123"),
        username: "Jason Davies",
        author: "Jason Davies",
        type: "user",
        roles: ["_admin"]
      }).ok);

      var validationDoc = {
        _id : "_design/validate",
        validate_doc_update: "(" + (function (newDoc, oldDoc, userCtx) {
          // docs should have an author field.
          if (!newDoc._deleted && !newDoc.author) {
            throw {forbidden:
                "Documents must have an author field"};
          }
          if (oldDoc && oldDoc.author != userCtx.name) {
              throw {unauthorized:
                  "You are not the author of this document. You jerk."+userCtx.name};
          }
        }).toString() + ")"
      };

      T(db.save(validationDoc).ok);

      

      T(CouchDB.login('Jason Davies', password).ok);
      // update the credentials document
      var doc = usersDb.open("a1");
      doc.foo=2;
      T(usersDb.save(doc).ok);

      // Save a document that's missing an author field.
      try {
        // db has a validation function
        db.save({foo:1});
        T(false && "Can't get here. Should have thrown an error 2");
      } catch (e) {
        T(e.error == "forbidden");
        T(db.last_req.status == 403);
      }

      // TODO should login() throw an exception here?
      T(!CouchDB.login('Jason Davies', "2.71828").ok);
      T(!CouchDB.login('Robert Allen Zimmerman', 'd00d').ok);

      // test redirect
      xhr = CouchDB.request("POST", "/_session?next=/", {
        headers: {"Content-Type": "application/x-www-form-urlencoded"},
        body: "username=Jason%20Davies&password="+encodeURIComponent(password)
      });
      // should this be a redirect code instead of 200?
      T(xhr.status == 200);
      
      usersDb.deleteDb();
      // test user creation
      T(CouchDB.createUser("test", "testpassword", "test@somemail.com", ['read', 'write']).ok);
      
      // make sure we create a unique user
      T(!CouchDB.createUser("test", "testpassword2", "test2@somemail.com", ['read', 'write']).ok);
      
      // test login
      T(CouchDB.login("test", "testpassword").ok);
      T(!CouchDB.login('test', "testpassword2").ok);
      
      // test update user without changing password
      T(CouchDB.updateUser("test", "test2@somemail.com").ok);
      result = usersDb.view("_auth/users", {key: "test"});
      T(result.rows[0].value['email'] == "test2@somemail.com");
       
       
      // test changing password
      result = usersDb.view("_auth/users", {key: "test"});
      T(CouchDB.updateUser("test", "test2@somemail.com", [], "testpassword2", "testpassword").ok);
      result1 = usersDb.view("_auth/users", {key: "test"});
      T(result.rows[0].value['password_sha'] != result1.rows[0].value['password_sha']);
      
      
      // test changing password with passing old password
      T(!CouchDB.updateUser("test", "test2@somemail.com", [], "testpassword2").ok);
      
      // test changing password whith bad old password
      T(!CouchDB.updateUser("test", "test2@somemail.com", [], "testpassword2", "badpasswword").ok);
      
      // Only admins can change roles
      T(!CouchDB.updateUser("test", "test2@somemail.com", ['read', 'write']).ok);
      
      T(CouchDB.logout().ok);
      
      T(CouchDB.updateUser("test", "test2@somemail.com").ok);
      result = usersDb.view("_auth/users", {key: "test"});
      T(result.rows[0].value['email'] == "test2@somemail.com");

      // test changing password, we don't need to set old password when we are admin
      result = usersDb.view("_auth/users", {key: "test"});
      T(CouchDB.updateUser("test", "test2@somemail.com", [], "testpassword3").ok);
      result1 = usersDb.view("_auth/users", {key: "test"});
      T(result.rows[0].value['password_sha'] != result1.rows[0].value['password_sha']);

      // Only admins can change roles
      T(CouchDB.updateUser("test", "test2@somemail.com", ['read']).ok);

    } finally {
      // Make sure we erase any auth cookies so we don't affect other tests
      T(CouchDB.logout().ok);
    }
  };

  run_on_modified_server(
    [{section: "httpd",
      key: "authentication_handlers",
      value: "{couch_httpd_auth, cookie_authentication_handler}"},
     {section: "couch_httpd_auth",
      key: "secret", value: generateSecret(64)},
     {section: "couch_httpd_auth",
      key: "authentication_db", value: "test_suite_users"}],
    testFun
  );

};
