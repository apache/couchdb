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

couchTests.view_compaction = function(debug) {

  if (debug) debugger;

  var db = new CouchDB("test_suite_db", {"X-Couch-Full-Commit": "true"});

  db.deleteDb();
  db.createDb();

  var ddoc = {
    _id: "_design/foo",
    language: "javascript",
    views: {
      view1: {
        map: "function(doc) { emit(doc._id, doc.value) }"
      },
      view2: {
        map: "function(doc) { emit(doc._id, doc.value); }",
        reduce: "function(keys, values, rereduce) { return sum(values); }"
      }
    }
  };
  T(db.save(ddoc).ok);

  var docs = makeDocs(0, 10000);
  db.bulkSave(docs);

  var resp = db.view('foo/view1', {});
  T(resp.rows.length === 10000);

  resp = db.view('foo/view2', {});
  T(resp.rows.length === 1);

  resp = db.designInfo("_design/foo");
  T(resp.view_index.update_seq === 10001);


  // update docs
  for (var i = 0; i < docs.length; i++) {
    docs[i].integer = docs[i].integer + 1;
  }
  db.bulkSave(docs);


  resp = db.view('foo/view1', {});
  T(resp.rows.length === 10000);

  resp = db.view('foo/view2', {});
  T(resp.rows.length === 1);

  resp = db.designInfo("_design/foo");
  T(resp.view_index.update_seq === 20001);


  // update docs again...
  for (var i = 0; i < docs.length; i++) {
    docs[i].integer = docs[i].integer + 2;
  }
  db.bulkSave(docs);


  resp = db.view('foo/view1', {});
  T(resp.rows.length === 10000);

  resp = db.view('foo/view2', {});
  T(resp.rows.length === 1);

  resp = db.designInfo("_design/foo");
  T(resp.view_index.update_seq === 30001);

  var disk_size_before_compact = resp.view_index.disk_size;
  var data_size_before_compact = resp.view_index.data_size;

  TEquals("number", typeof data_size_before_compact, "data size is a number");
  T(data_size_before_compact < disk_size_before_compact, "data size < file size");

  // compact view group
  var xhr = CouchDB.request("POST", "/" + db.name + "/_compact" + "/foo");
  T(JSON.parse(xhr.responseText).ok === true);

  resp = db.designInfo("_design/foo");
  while (resp.view_index.compact_running === true) {
    resp = db.designInfo("_design/foo");
  }


  resp = db.view('foo/view1', {});
  T(resp.rows.length === 10000);

  resp = db.view('foo/view2', {});
  T(resp.rows.length === 1);

  resp = db.designInfo("_design/foo");
  T(resp.view_index.update_seq === 30001);
  T(resp.view_index.disk_size < disk_size_before_compact);
  TEquals("number", typeof resp.view_index.data_size, "data size is a number");
  T(resp.view_index.data_size < resp.view_index.disk_size, "data size < file size");
};