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

couchTests.security_validation = function(debug) {
  // This tests couchdb's security and validation features. This does
  // not test authentication, except to use test authentication code made
  // specifically for this testing. It is a WWWW-Authenticate scheme named
  // X-Couch-Test-Auth, and the user names and passwords are hard coded
  // on the server-side.
  // 
  // We could have used Basic authentication, however the XMLHttpRequest
  // implementation for Firefox and Safari, and probably other browsers are
  // broken (Firefox always prompts the user on 401 failures, Safari gives
  // odd security errors when using different name/passwords, perhaps due
  // to cross site scripting prevention).  These problems essentially make Basic
  // authentication testing in the browser impossible. But while hard to
  // test automated in the browser, Basic auth may still useful for real
  // world use where these bugs/behaviors don't matter.
  //
  // So for testing purposes we are using this custom X-Couch-Test-Auth.
  // It's identical to Basic auth, except it doesn't even base64 encode
  // the "username:password" string, it's sent completely plain text.
  // Firefox and Safari both deal with this correctly (which is to say
  // they correctly do nothing special).
  
  
  var db = new CouchDB("test_suite_db");
  db.deleteDb();
  db.createDb();
  if (debug) debugger;
  
  run_on_modified_server(
    [{section: "httpd",
      key: "authentication_handler",
      value: "{couch_httpd, special_test_authentication_handler}"},
    {section:"httpd",
      key: "WWW-Authenticate",
      value:  "X-Couch-Test-Auth"}],
      
    function () {
  
      // try saving document usin the wrong credentials
      var wrongPasswordDb = new CouchDB("test_suite_db",
        {"WWW-Authenticate": "X-Couch-Test-Auth Damien Katz:foo"}
      );
  
      try {
        wrongPasswordDb.save({foo:1,author:"Damien Katz"});
        T(false && "Can't get here. Should have thrown an error 1");
      } catch (e) {
        T(e.error == "unauthorized");
        T(wrongPasswordDb.last_req.status == 401);
      }
      
      
      // Create the design doc that will run custom validation code
      var designDoc = {
        _id:"_design/test",
        language: "javascript",
        validate_doc_update: "(" + (function (newDoc, oldDoc, userCtx) {
          // docs should have an author field.
          if (!newDoc._deleted && !newDoc.author) {
            throw {forbidden:
                "Documents must have an author field"};
          }
          if (oldDoc && oldDoc.author != userCtx.name) {
              throw {unauthorized:
                  "You are not the author of this document. You jerk."};
          }
        }).toString() + ")"
      }

      // Save a document normally
      var userDb = new CouchDB("test_suite_db",
        {"WWW-Authenticate": "X-Couch-Test-Auth Damien Katz:pecan pie"}
      );
      
      T(userDb.save({_id:"testdoc", foo:1, author:"Damien Katz"}).ok);
      
      // Attempt to save the design as a non-admin
      try {
        userDb.save(designDoc);
        T(false && "Can't get here. Should have thrown an error on design doc");
      } catch (e) {
        T(e.error == "unauthorized");
        T(userDb.last_req.status == 401);
      }
      
      // add user as admin
      db.setAdmins(["Damien Katz"]);
      
      T(userDb.save(designDoc).ok);
  
      // update the document
      var doc = userDb.open("testdoc");
      doc.foo=2;
      T(userDb.save(doc).ok);
      
      // Save a document that's missing an author field.
      try {
        userDb.save({foo:1});
        T(false && "Can't get here. Should have thrown an error 2");
      } catch (e) {
        T(e.error == "forbidden");
        T(userDb.last_req.status == 403);
      }
  
      // Now attempt to update the document as a different user, Jan 
      var user2Db = new CouchDB("test_suite_db",
        {"WWW-Authenticate": "X-Couch-Test-Auth Jan Lehnardt:apple"}
      );
  
      var doc = user2Db.open("testdoc");
      doc.foo=3;
      try {
        user2Db.save(doc);
        T(false && "Can't get here. Should have thrown an error 3");
      } catch (e) {
        T(e.error == "unauthorized");
        T(user2Db.last_req.status == 401);
      }
      
      // Now have Damien change the author to Jan
      doc = userDb.open("testdoc");
      doc.author="Jan Lehnardt";
      T(userDb.save(doc).ok);
      
      // Now update the document as Jan
      doc = user2Db.open("testdoc");
      doc.foo = 3;
      T(user2Db.save(doc).ok);
      
      // Damien can't delete it
      try {
        userDb.deleteDoc(doc);
        T(false && "Can't get here. Should have thrown an error 4");
      } catch (e) {
        T(e.error == "unauthorized");
        T(userDb.last_req.status == 401);
      }
      
      // Now delete document
      T(user2Db.deleteDoc(doc).ok);
    });
};
