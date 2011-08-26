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

// Do some basic tests.
couchTests.basics = function(debug) {
  var result = JSON.parse(CouchDB.request("GET", "/").responseText);
  T(result.couchdb == "Welcome");

  var db = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"false"});
  db.deleteDb();

  // bug COUCHDB-100: DELETE on non-existent DB returns 500 instead of 404
  db.deleteDb();

  db.createDb();

  // PUT on existing DB should return 412 instead of 500
  xhr = CouchDB.request("PUT", "/test_suite_db/");
  T(xhr.status == 412);
  if (debug) debugger;

  // creating a new DB should return Location header
  // and it should work for dbs with slashes (COUCHDB-411)
  var dbnames = ["test_suite_db", "test_suite_db%2Fwith_slashes"];
  dbnames.forEach(function(dbname) {
    xhr = CouchDB.request("DELETE", "/" + dbname);
    xhr = CouchDB.request("PUT", "/" + dbname);
    TEquals(dbname,
      xhr.getResponseHeader("Location").substr(-dbname.length),
      "should return Location header to newly created document");
    TEquals(CouchDB.protocol,
      xhr.getResponseHeader("Location").substr(0, CouchDB.protocol.length),
      "should return absolute Location header to newly created document");
  });

  // Get the database info, check the db_name
  T(db.info().db_name == "test_suite_db");
  T(CouchDB.allDbs().indexOf("test_suite_db") != -1);

  // Get the database info, check the doc_count
  T(db.info().doc_count == 0);

  // create a document and save it to the database
  var doc = {_id:"0",a:1,b:1};
  var result = db.save(doc);

  T(result.ok==true); // return object has an ok member with a value true
  T(result.id); // the _id of the document is set.
  T(result.rev); // the revision id of the document is set.

  // Verify the input doc is now set with the doc id and rev
  // (for caller convenience).
  T(doc._id == result.id && doc._rev == result.rev);

  var id = result.id; // save off the id for later

  // make sure the revs_info status is good
  var doc = db.open(id, {revs_info:true});
  T(doc._revs_info[0].status == "available");

  // make sure you can do a seq=true option
  var doc = db.open(id, {local_seq:true});
  T(doc._local_seq == 1);


  // Create some more documents.
  // Notice the use of the ok member on the return result.
  T(db.save({_id:"1",a:2,b:4}).ok);
  T(db.save({_id:"2",a:3,b:9}).ok);
  T(db.save({_id:"3",a:4,b:16}).ok);

  // Check the database doc count
  T(db.info().doc_count == 4);

  // COUCHDB-954
  var oldRev = db.save({_id:"COUCHDB-954", a:1}).rev;
  var newRev = db.save({_id:"COUCHDB-954", _rev:oldRev}).rev;

  // test behavior of open_revs with explicit revision list
  var result = db.open("COUCHDB-954", {open_revs:[oldRev,newRev]});
  T(result.length == 2, "should get two revisions back");
  T(result[0].ok);
  T(result[1].ok);

  // latest=true suppresses non-leaf revisions
  var result = db.open("COUCHDB-954", {open_revs:[oldRev,newRev], latest:true});
  T(result.length == 1, "should only get the child revision with latest=true");
  T(result[0].ok._rev == newRev, "should get the child and not the parent");

  // latest=true returns a child when you ask for a parent
  var result = db.open("COUCHDB-954", {open_revs:[oldRev], latest:true});
  T(result[0].ok._rev == newRev, "should get child when we requested parent");

  // clean up after ourselves
  db.save({_id:"COUCHDB-954", _rev:newRev, _deleted:true});

  // Test a simple map functions

  // create a map function that selects all documents whose "a" member
  // has a value of 4, and then returns the document's b value.
  var mapFunction = function(doc){
    if (doc.a==4)
      emit(null, doc.b);
  };

  var results = db.query(mapFunction);

  // verify only one document found and the result value (doc.b).
  T(results.total_rows == 1 && results.rows[0].value == 16);

  // reopen document we saved earlier
  var existingDoc = db.open(id);

  T(existingDoc.a==1);

  //modify and save
  existingDoc.a=4;
  db.save(existingDoc);

  // redo the map query
  results = db.query(mapFunction);

  // the modified document should now be in the results.
  T(results.total_rows == 2);

  // write 2 more documents
  T(db.save({a:3,b:9}).ok);
  T(db.save({a:4,b:16}).ok);

  results = db.query(mapFunction);

  // 1 more document should now be in the result.
  T(results.total_rows == 3);
  T(db.info().doc_count == 6);

  var reduceFunction = function(keys, values){
    return sum(values);
  };

  results = db.query(mapFunction, reduceFunction);

  T(results.rows[0].value == 33);

  // delete a document
  T(db.deleteDoc(existingDoc).ok);

  // make sure we can't open the doc
  T(db.open(existingDoc._id) == null);

  results = db.query(mapFunction);

  // 1 less document should now be in the results.
  T(results.total_rows == 2);
  T(db.info().doc_count == 5);

  // make sure we can still open the old rev of the deleted doc
  T(db.open(existingDoc._id, {rev: existingDoc._rev}) != null);
  // make sure restart works
  T(db.ensureFullCommit().ok);
  restartServer();

  // make sure we can still open
  T(db.open(existingDoc._id, {rev: existingDoc._rev}) != null);

  // test that the POST response has a Location header
  var xhr = CouchDB.request("POST", "/test_suite_db", {
    body: JSON.stringify({"foo":"bar"}),
    headers: {"Content-Type": "application/json"}
  });
  var resp = JSON.parse(xhr.responseText);
  T(resp.ok);
  var loc = xhr.getResponseHeader("Location");
  T(loc, "should have a Location header");
  var locs = loc.split('/');
  T(locs[locs.length-1] == resp.id);
  T(locs[locs.length-2] == "test_suite_db");

  // test that that POST's with an _id aren't overriden with a UUID.
  var xhr = CouchDB.request("POST", "/test_suite_db", {
    headers: {"Content-Type": "application/json"},
    body: JSON.stringify({"_id": "oppossum", "yar": "matey"})
  });
  var resp = JSON.parse(xhr.responseText);
  T(resp.ok);
  T(resp.id == "oppossum");
  var doc = db.open("oppossum");
  T(doc.yar == "matey");

  // document put's should return a Location header
  var xhr = CouchDB.request("PUT", "/test_suite_db/newdoc", {
    body: JSON.stringify({"a":1})
  });
  TEquals("/test_suite_db/newdoc",
    xhr.getResponseHeader("Location").substr(-21),
    "should return Location header to newly created document");
  TEquals(CouchDB.protocol,
    xhr.getResponseHeader("Location").substr(0, CouchDB.protocol.length),
    "should return absolute Location header to newly created document");

  // deleting a non-existent doc should be 404
  xhr = CouchDB.request("DELETE", "/test_suite_db/doc-does-not-exist");
  T(xhr.status == 404);

  // Check for invalid document members
  var bad_docs = [
    ["goldfish", {"_zing": 4}],
    ["zebrafish", {"_zoom": "hello"}],
    ["mudfish", {"zane": "goldfish", "_fan": "something smells delicious"}],
    ["tastyfish", {"_bing": {"wha?": "soda can"}}]
  ];
  var test_doc = function(info) {
  var data = JSON.stringify(info[1]);
    xhr = CouchDB.request("PUT", "/test_suite_db/" + info[0], {body: data});
    T(xhr.status == 500);
    result = JSON.parse(xhr.responseText);
    T(result.error == "doc_validation");

    xhr = CouchDB.request("POST", "/test_suite_db/", {
      headers: {"Content-Type": "application/json"},
      body: data
    });
    T(xhr.status == 500);
    result = JSON.parse(xhr.responseText);
    T(result.error == "doc_validation");
  };
  bad_docs.forEach(test_doc);

  // Check some common error responses.
  // PUT body not an object
  xhr = CouchDB.request("PUT", "/test_suite_db/bar", {body: "[]"});
  T(xhr.status == 400);
  result = JSON.parse(xhr.responseText);
  T(result.error == "bad_request");
  T(result.reason == "Document must be a JSON object");

  // Body of a _bulk_docs is not an object
  xhr = CouchDB.request("POST", "/test_suite_db/_bulk_docs", {body: "[]"});
  T(xhr.status == 400);
  result = JSON.parse(xhr.responseText);
  T(result.error == "bad_request");
  T(result.reason == "Request body must be a JSON object");

  // Body of an _all_docs  multi-get is not a {"key": [...]} structure.
  xhr = CouchDB.request("POST", "/test_suite_db/_all_docs", {body: "[]"});
  T(xhr.status == 400);
  result = JSON.parse(xhr.responseText);
  T(result.error == "bad_request");
  T(result.reason == "Request body must be a JSON object");
  var data = "{\"keys\": 1}";
  xhr = CouchDB.request("POST", "/test_suite_db/_all_docs", {body:data});
  T(xhr.status == 400);
  result = JSON.parse(xhr.responseText);
  T(result.error == "bad_request");
  T(result.reason == "`keys` member must be a array.");

  // oops, the doc id got lost in code nirwana
  xhr = CouchDB.request("DELETE", "/test_suite_db/?rev=foobarbaz");
  TEquals(400, xhr.status, "should return a bad request");
  result = JSON.parse(xhr.responseText);
  TEquals("bad_request", result.error);
  TEquals("You tried to DELETE a database with a ?=rev parameter. Did you mean to DELETE a document instead?", result.reason);

  // On restart, a request for creating a database that already exists can
  // not override the existing database file
  db = new CouchDB("test_suite_foobar");
  db.deleteDb();
  xhr = CouchDB.request("PUT", "/" + db.name);
  TEquals(201, xhr.status);

  TEquals(true, db.save({"_id": "doc1"}).ok);
  TEquals(true, db.ensureFullCommit().ok);

  TEquals(1, db.info().doc_count);

  restartServer();

  xhr = CouchDB.request("PUT", "/" + db.name);
  TEquals(412, xhr.status);

  TEquals(1, db.info().doc_count);
};
