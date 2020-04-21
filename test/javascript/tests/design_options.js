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
couchTests.elixir = true;
couchTests.design_options = function(debug) {
  return console.log('done in test/elixir/test/design_options.exs');
  var db_name = get_random_db_name();
  var db = new CouchDB(db_name, {"X-Couch-Full-Commit":"false"});
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
  // no more there on cluster - pointless test
  //var rows = db.query(map, null, {options:{include_design: true}}).rows;
  //T(rows.length == 1);
  //T(rows[0].value == "_design/fu");
  //
  //rows = db.query(map).rows;
  //T(rows.length == 0);

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
  // format is more complex on cluster now
  T(!!rows[0].key)
  T(!!rows[1].key)
  var doc = db.open(resp.id);
  db.deleteDoc(doc);
  db.deleteDb();
};
