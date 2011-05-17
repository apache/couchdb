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

// test saving a semi-large quanitity of documents and do some view queries.
couchTests.lots_of_docs = function(debug) {
  var db = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"false"});
  db.deleteDb();
  db.createDb();
  if (debug) debugger;

  // keep number lowish for now to keep tests fasts. Crank up manually to
  // to really test.
  var numDocsToCreate = 500;

  for(var i=0; i < numDocsToCreate; i += 100) {
      var createNow = Math.min(numDocsToCreate - i, 100);
      var docs = makeDocs(i, i + createNow);
      db.bulkSave(docs);
  }

  // query all documents, and return the doc.integer member as a key.
  results = db.query(function(doc){ emit(doc.integer, null) });

  T(results.total_rows == numDocsToCreate);

  // validate the keys are ordered ascending
  for(var i=0; i<numDocsToCreate; i++) {
    T(results.rows[i].key==i);
  }

  // do the query again, but with descending output
  results = db.query(function(doc){ emit(doc.integer, null) }, null, {
    descending: true
  });

  T(results.total_rows == numDocsToCreate);

  // validate the keys are ordered descending
  for(var i=0; i<numDocsToCreate; i++) {
    T(results.rows[numDocsToCreate-1-i].key==i);
  }

  // Check _all_docs with descending=true again (now that there are many docs)
  var desc = db.allDocs({descending:true});
  T(desc.total_rows == desc.rows.length);
};
