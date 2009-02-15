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

couchTests.bulk_docs = function(debug) {
  var db = new CouchDB("test_suite_db");
  db.deleteDb();
  db.createDb();
  if (debug) debugger;

  var docs = makeDocs(5);

  // Create the docs
  var result = db.bulkSave(docs);
  T(result.ok);
  T(result.new_revs.length == 5);
  for (var i = 0; i < 5; i++) {
    T(result.new_revs[i].id == docs[i]._id);
    T(result.new_revs[i].rev);
    docs[i].string = docs[i].string + ".00";
  }

  // Update the docs
  result = db.bulkSave(docs);
  T(result.ok);
  T(result.new_revs.length == 5);
  for (i = 0; i < 5; i++) {
    T(result.new_revs[i].id == i.toString());
    docs[i]._deleted = true;
  }

  // Delete the docs
  result = db.bulkSave(docs);
  T(result.ok);
  T(result.new_revs.length == 5);
  for (i = 0; i < 5; i++) {
    T(db.open(docs[i]._id) == null);
  }
  
  // verify creating a document with no id returns a new id
  var req = CouchDB.request("POST", "/test_suite_db/_bulk_docs", {
    body: JSON.stringify({"docs": [{"foo":"bar"}]})
  });
  result = JSON.parse(req.responseText);
  
  T(result.new_revs[0].id != "");
  T(result.new_revs[0].rev != "");
};
