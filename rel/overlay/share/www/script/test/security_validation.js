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

couchTests.security_validation = function(debug) {
  // This tests couchdb's security and validation features. This does
  // not test authentication, except to use test authentication code made
  // specifically for this testing. It is a WWW-Authenticate scheme named
  // X-Couch-Test-Auth, and the user names and passwords are hard coded
  // on the server-side.
  //
  // We could have used Basic authentication, however the XMLHttpRequest
  // implementation for Firefox and Safari, and probably other browsers are
  // broken (Firefox always prompts the user on 401 failures, Safari gives
  // odd security errors when using different name/passwords, perhaps due
  // to cross site scripting prevention). These problems essentially make Basic
  // authentication testing in the browser impossible. But while hard to
  // test automated in the browser, Basic auth may still useful for real
  // world use where these bugs/behaviors don't matter.
  //
  // So for testing purposes we are using this custom X-Couch-Test-Auth.
  // It's identical to Basic auth, except it doesn't even base64 encode
  // the "username:password" string, it's sent completely plain text.
  // Firefox and Safari both deal with this correctly (which is to say
  // they correctly do nothing special).

  var db = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"false"});
  db.deleteDb();
  db.createDb();
  if (debug) debugger;

  run_on_modified_server(
    [{section: "httpd",
      key: "authentication_handlers",
      value: "{couch_httpd_auth, special_test_authentication_handler}"},
     {section:"httpd",
      key: "WWW-Authenticate",
      value:  "X-Couch-Test-Auth"}],

    function () {
      // try saving document using the wrong credentials
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

      // test force basic login
      var resp = wrongPasswordDb.request("GET", "/_session?basic=true");
      var err = JSON.parse(resp.responseText);
      T(err.error == "unauthorized");
      T(resp.status == 401);

      // Create the design doc that will run custom validation code
      var designDoc = {
        _id:"_design/test",
        language: "javascript",
        validate_doc_update: "(" + (function (newDoc, oldDoc, userCtx, secObj) {
          if (secObj.admin_override) {
            if (userCtx.roles.indexOf('_admin') != -1) {
              // user is admin, they can do anything
              return true;
            }
          }
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

      // set user as the admin
      T(db.setSecObj({
        admins : {names : ["Damien Katz"]}
      }).ok);

      T(userDb.save(designDoc).ok);

      var user2Db = new CouchDB("test_suite_db",
        {"WWW-Authenticate": "X-Couch-Test-Auth Jan Lehnardt:apple"}
      );
      // Attempt to save the design as a non-admin (in replication scenario)
      try {
        user2Db.save(designDoc, {new_edits : false});
        T(false && "Can't get here. Should have thrown an error on design doc");
      } catch (e) {
        T(e.error == "unauthorized");
        T(user2Db.last_req.status == 401);
      }

      // test the _session API
      var resp = userDb.request("GET", "/_session");
      var user = JSON.parse(resp.responseText).userCtx;
      T(user.name == "Damien Katz");
      // test that the roles are listed properly
      TEquals(user.roles, []);


      // update the document
      var doc = userDb.open("testdoc");
      doc.foo=2;
      T(userDb.save(doc).ok);

      // Save a document that's missing an author field (before and after compaction)
      for (var i=0; i<2; i++) {
          try {
              userDb.save({foo:1});
              T(false && "Can't get here. Should have thrown an error 2");
          } catch (e) {
              T(e.error == "forbidden");
              T(userDb.last_req.status == 403);
          }
          // compact.
          T(db.compact().ok);
          T(db.last_req.status == 202);
          // compaction isn't instantaneous, loop until done
          while (db.info().compact_running) {};
      }

      // Now attempt to update the document as a different user, Jan
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
      
      // admin must save with author field unless admin override
      var resp = db.request("GET", "/_session");
      var user = JSON.parse(resp.responseText).userCtx;
      T(user.name == null);
      // test that we are admin
      TEquals(user.roles, ["_admin"]);
      
      // can't save the doc even though we are admin
      var doc = db.open("testdoc");
      doc.foo=3;
      try {
        db.save(doc);
        T(false && "Can't get here. Should have thrown an error 3");
      } catch (e) {
        T(e.error == "unauthorized");
        T(db.last_req.status == 401);
      }

      // now turn on admin override
      T(db.setDbProperty("_security", {admin_override : true}).ok);
      T(db.save(doc).ok);

      // try to do something lame
      try {
        db.setDbProperty("_security", ["foo"]);
        T(false && "can't do this");
      } catch(e) {}

      // go back to normal
      T(db.setDbProperty("_security", {admin_override : false}).ok);

      // Now delete document
      T(user2Db.deleteDoc(doc).ok);

      // now test bulk docs
      var docs = [{_id:"bahbah",author:"Damien Katz",foo:"bar"},{_id:"fahfah",foo:"baz"}];

      // Create the docs
      var results = db.bulkSave(docs);

      T(results[0].rev)
      T(results[0].error == undefined)
      T(results[1].rev === undefined)
      T(results[1].error == "forbidden")

      T(db.open("bahbah"));
      T(db.open("fahfah") == null);


      // now all or nothing with a failure
      var docs = [{_id:"booboo",author:"Damien Katz",foo:"bar"},{_id:"foofoo",foo:"baz"}];

      // Create the docs
      var results = db.bulkSave(docs, {all_or_nothing:true});

      T(results.errors.length == 1);
      T(results.errors[0].error == "forbidden");
      T(db.open("booboo") == null);
      T(db.open("foofoo") == null);

      // Now test replication
      var AuthHeaders = {"WWW-Authenticate": "X-Couch-Test-Auth Christopher Lenz:dog food"};
      var host = CouchDB.host;
      var dbPairs = [
        {source:"test_suite_db_a",
          target:"test_suite_db_b"},

        {source:"test_suite_db_a",
          target:{url: CouchDB.protocol + host + "/test_suite_db_b",
                  headers: AuthHeaders}},

        {source:{url:CouchDB.protocol + host + "/test_suite_db_a",
                 headers: AuthHeaders},
          target:"test_suite_db_b"},

        {source:{url:CouchDB.protocol + host + "/test_suite_db_a",
                 headers: AuthHeaders},
         target:{url:CouchDB.protocol + host + "/test_suite_db_b",
                 headers: AuthHeaders}},
      ]
      var adminDbA = new CouchDB("test_suite_db_a", {"X-Couch-Full-Commit":"false"});
      var adminDbB = new CouchDB("test_suite_db_b", {"X-Couch-Full-Commit":"false"});
      var dbA = new CouchDB("test_suite_db_a",
          {"WWW-Authenticate": "X-Couch-Test-Auth Christopher Lenz:dog food"});
      var dbB = new CouchDB("test_suite_db_b",
          {"WWW-Authenticate": "X-Couch-Test-Auth Christopher Lenz:dog food"});
      var xhr;
      for (var testPair = 0; testPair < dbPairs.length; testPair++) {
        var A = dbPairs[testPair].source
        var B = dbPairs[testPair].target

        adminDbA.deleteDb();
        adminDbA.createDb();
        adminDbB.deleteDb();
        adminDbB.createDb();

        // save and replicate a documents that will and will not pass our design
        // doc validation function.
        dbA.save({_id:"foo1",value:"a",author:"Noah Slater"});
        dbA.save({_id:"foo2",value:"a",author:"Christopher Lenz"});
        dbA.save({_id:"bad1",value:"a"});

        T(CouchDB.replicate(A, B, {headers:AuthHeaders}).ok);
        T(CouchDB.replicate(B, A, {headers:AuthHeaders}).ok);

        T(dbA.open("foo1"));
        T(dbB.open("foo1"));
        T(dbA.open("foo2"));
        T(dbB.open("foo2"));

        // save the design doc to dbA
        delete designDoc._rev; // clear rev from previous saves
        adminDbA.save(designDoc);

        // no affect on already saved docs
        T(dbA.open("bad1"));

        // Update some docs on dbB. Since the design hasn't replicated, anything
        // is allowed.

        // this edit will fail validation on replication to dbA (no author)
        T(dbB.save({_id:"bad2",value:"a"}).ok);

        // this edit will fail security on replication to dbA (wrong author
        //  replicating the change)
        var foo1 = dbB.open("foo1");
        foo1.value = "b";
        dbB.save(foo1);

        // this is a legal edit
        var foo2 = dbB.open("foo2");
        foo2.value = "b";
        dbB.save(foo2);

        var results = CouchDB.replicate(B, A, {headers:AuthHeaders});

        T(results.ok);

        T(results.history[0].docs_written == 1);
        T(results.history[0].doc_write_failures == 2);

        // bad2 should not be on dbA
        T(dbA.open("bad2") == null);

        // The edit to foo1 should not have replicated.
        T(dbA.open("foo1").value == "a");

        // The edit to foo2 should have replicated.
        T(dbA.open("foo2").value == "b");
      }
    });
};
