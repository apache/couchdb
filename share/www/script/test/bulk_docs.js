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

couchTests.bulk_docs = function(debug) {
  var db = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"false"});
  db.deleteDb();
  db.createDb();
  if (debug) debugger;

  var docs = makeDocs(5);

  // Create the docs
  var results = db.bulkSave(docs);

  T(results.length == 5);
  for (var i = 0; i < 5; i++) {
    T(results[i].id == docs[i]._id);
    T(results[i].rev);
    // Update the doc
    docs[i].string = docs[i].string + ".00";
  }

  // Save the docs
  results = db.bulkSave(docs);
  T(results.length == 5);
  for (i = 0; i < 5; i++) {
    T(results[i].id == i.toString());

    // set the delete flag to delete the docs in the next step
    docs[i]._deleted = true;
  }

  // now test a bulk update with a conflict
  // open and save
  var doc = db.open("0");
  db.save(doc);

  // Now bulk delete the docs
  results = db.bulkSave(docs);

  // doc "0" should be a conflict
  T(results.length == 5);
  T(results[0].id == "0");
  T(results[0].error == "conflict");
  T(typeof results[0].rev === "undefined"); // no rev member when a conflict

  // but the rest are not
  for (i = 1; i < 5; i++) {
    T(results[i].id == i.toString());
    T(results[i].rev);
    T(db.open(docs[i]._id) == null);
  }

  // now force a conflict to to save

  // save doc 0, this will cause a conflict when we save docs[0]
  var doc = db.open("0");
  docs[0] = db.open("0");
  db.save(doc);

  docs[0].shooby = "dooby";

  // Now save the bulk docs, When we use all_or_nothing, we don't get conflict
  // checking, all docs are saved regardless of conflict status, or none are
  // saved.
  results = db.bulkSave(docs,{all_or_nothing:true});
  T(results.error === undefined);

  var doc = db.open("0", {conflicts:true});
  var docConflict = db.open("0", {rev:doc._conflicts[0]});

  T(doc.shooby == "dooby" || docConflict.shooby == "dooby");

  // verify creating a document with no id returns a new id
  var req = CouchDB.request("POST", "/test_suite_db/_bulk_docs", {
    body: JSON.stringify({"docs": [{"foo":"bar"}]})
  });
  results = JSON.parse(req.responseText);

  T(results[0].id != "");
  T(results[0].rev != "");


  // Regression test for failure on update/delete
  var newdoc = {"_id": "foobar", "body": "baz"};
  T(db.save(newdoc).ok);
  var update = {"_id": newdoc._id, "_rev": newdoc._rev, "body": "blam"};
  var torem = {"_id": newdoc._id, "_rev": newdoc._rev, "_deleted": true};
  results = db.bulkSave([update, torem]);
  T(results[0].error == "conflict" || results[1].error == "conflict");


  // verify that sending a request with no docs causes error thrown
  var req = CouchDB.request("POST", "/test_suite_db/_bulk_docs", {
    body: JSON.stringify({"doc": [{"foo":"bar"}]})
  });

  T(req.status == 400 );
  result = JSON.parse(req.responseText);
  T(result.error == "bad_request");
  T(result.reason == "Missing JSON list of 'docs'");
};
