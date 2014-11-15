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

couchTests.design_options = function(debug) {
  var db = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"false"});
  db.deleteDb();
  db.createDb();
  if (debug) debugger;

  //// test the includes_design option
  var map = "function (doc) {emit(null, doc._id);}";
  var withseq = "function(doc) {emit(doc._local_seq, null)}"

  // we need a design doc even to test temp views with it
  var designDoc = {
    _id:"_design/fu",
    language: "javascript",
    options: {
      include_design: true,
      local_seq: true
    },
    views: {
      data: {"map": map},
      with_seq : {"map" : withseq}
    }
  };
  T(db.save(designDoc).ok);

  // should work for temp views
  var rows = db.query(map, null, {options:{include_design: true}}).rows;
  T(rows.length == 1);
  T(rows[0].value == "_design/fu");

  rows = db.query(map).rows;
  T(rows.length == 0);

  // when true, should include design docs in views
  rows = db.view("fu/data").rows;
  T(rows.length == 1);
  T(rows[0].value == "_design/fu");

  // when false, should not
  designDoc.options.include_design = false;
  delete designDoc._rev;
  designDoc._id = "_design/bingo";
  T(db.save(designDoc).ok);
  rows = db.view("bingo/data").rows;
  T(rows.length == 0);

  // should default to false
  delete designDoc.options;
  delete designDoc._rev;
  designDoc._id = "_design/bango";
  T(db.save(designDoc).ok);
  rows = db.view("bango/data").rows;
  T(rows.length == 0);

  // should also have local_seq in the view
  var resp = db.save({});
  rows = db.view("fu/with_seq").rows;
  T(rows[0].key == 1)
  T(rows[1].key == 2)
  var doc = db.open(resp.id);
  db.deleteDoc(doc);
};
