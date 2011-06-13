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

couchTests.view_include_docs = function(debug) {
  var db = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"false"});
  db.deleteDb();
  db.createDb();
  if (debug) debugger;

  var docs = makeDocs(0, 100);
  db.bulkSave(docs);

  var designDoc = {
    _id:"_design/test",
    language: "javascript",
    views: {
      all_docs: {
        map: "function(doc) { emit(doc.integer, doc.string) }"
      },
      with_prev: {
        map: "function(doc){if(doc.prev) emit(doc._id,{'_rev':doc.prev}); else emit(doc._id,{'_rev':doc._rev});}"
      },
      with_id: {
        map: "function(doc) {if(doc.link_id) { var value = {'_id':doc.link_id}; if (doc.link_rev) {value._rev = doc.link_rev}; emit(doc._id, value);}};"
      },
      summate: {
        map:"function (doc) {emit(doc.integer, doc.integer)};",
        reduce:"function (keys, values) { return sum(values); };"
      }
    }
  }
  T(db.save(designDoc).ok);

  var resp = db.view('test/all_docs', {include_docs: true, limit: 2});
  T(resp.rows.length == 2);
  T(resp.rows[0].id == "0");
  T(resp.rows[0].doc._id == "0");
  T(resp.rows[1].id == "1");
  T(resp.rows[1].doc._id == "1");

  resp = db.view('test/all_docs', {include_docs: true}, [29, 74]);
  T(resp.rows.length == 2);
  T(resp.rows[0].doc._id == "29");
  T(resp.rows[1].doc.integer == 74);

  resp = db.allDocs({limit: 2, skip: 1, include_docs: true});
  T(resp.rows.length == 2);
  T(resp.rows[0].doc.integer == 1);
  T(resp.rows[1].doc.integer == 10);

  resp = db.allDocs({include_docs: true}, ['not_a_doc']);
  T(resp.rows.length == 1);
  T(!resp.rows[0].doc);

  resp = db.allDocs({include_docs: true}, ["1", "foo"]);
  T(resp.rows.length == 2);
  T(resp.rows[0].doc.integer == 1);
  T(!resp.rows[1].doc);

  resp = db.allDocs({include_docs: true, limit: 0});
  T(resp.rows.length == 0);

  // No reduce support
  try {
      resp = db.view('test/summate', {include_docs: true});
      alert(JSON.stringify(resp));
      T(0==1);
  } catch (e) {
      T(e.error == 'query_parse_error');
  }

  // Reduce support when reduce=false
  resp = db.view('test/summate', {reduce: false, include_docs: true});
  T(resp.rows.length == 100);

  // Not an error with include_docs=false&reduce=true
  resp = db.view('test/summate', {reduce: true, include_docs: false});
  T(resp.rows.length == 1);
  T(resp.rows[0].value == 4950);

  T(db.save({
    "_id": "link-to-10",
    "link_id" : "10"
  }).ok);
  
  // you can link to another doc from a value.
  resp = db.view("test/with_id", {key:"link-to-10"});
  T(resp.rows[0].key == "link-to-10");
  T(resp.rows[0].value["_id"] == "10");
  
  resp = db.view("test/with_id", {key:"link-to-10",include_docs: true});
  T(resp.rows[0].key == "link-to-10");
  T(resp.rows[0].value["_id"] == "10");
  T(resp.rows[0].doc._id == "10");

  // Check emitted _rev controls things
  resp = db.allDocs({include_docs: true}, ["0"]);
  var before = resp.rows[0].doc;

  var after = db.open("0");
  after.integer = 100;
  after.prev = after._rev;
  resp = db.save(after)
  T(resp.ok);
  
  var after = db.open("0");
  TEquals(resp.rev, after._rev, "fails with firebug running");
  T(after._rev != after.prev, "passes");
  TEquals(100, after.integer, "fails with firebug running");

  // should emit the previous revision
  resp = db.view("test/with_prev", {include_docs: true}, ["0"]);
  T(resp.rows[0].doc._id == "0");
  T(resp.rows[0].doc._rev == before._rev);
  T(!resp.rows[0].doc.prev);
  T(resp.rows[0].doc.integer == 0);

  var xhr = CouchDB.request("POST", "/test_suite_db/_compact");
  T(xhr.status == 202)
  while (db.info().compact_running) {}

  resp = db.view("test/with_prev", {include_docs: true}, ["0", "23"]);
  T(resp.rows.length == 2);
  T(resp.rows[0].key == "0");
  T(resp.rows[0].id == "0");
  T(!resp.rows[0].doc);
  T(resp.rows[0].doc == null);
  T(resp.rows[1].doc.integer == 23);

  // COUCHDB-549 - include_docs=true with conflicts=true

  var dbA = new CouchDB("test_suite_db_a", {"X-Couch-Full-Commit":"false"});
  var dbB = new CouchDB("test_suite_db_b", {"X-Couch-Full-Commit":"false"});

  dbA.deleteDb();
  dbA.createDb();
  dbB.deleteDb();
  dbB.createDb();

  var ddoc = {
    _id: "_design/mydesign",
    language : "javascript",
    views : {
      myview : {
        map: (function(doc) {
          emit(doc.value, 1);
        }).toString()
      }
    }
  };
  TEquals(true, dbA.save(ddoc).ok);

  var doc1a = {_id: "foo", value: 1, str: "1"};
  TEquals(true, dbA.save(doc1a).ok);

  var doc1b = {_id: "foo", value: 1, str: "666"};
  TEquals(true, dbB.save(doc1b).ok);

  var doc2 = {_id: "bar", value: 2, str: "2"};
  TEquals(true, dbA.save(doc2).ok);

  TEquals(true, CouchDB.replicate(dbA.name, dbB.name).ok);

  doc1b = dbB.open("foo", {conflicts: true});
  TEquals(true, doc1b._conflicts instanceof Array);
  TEquals(1, doc1b._conflicts.length);
  var conflictRev = doc1b._conflicts[0];

  doc2 = dbB.open("bar", {conflicts: true});
  TEquals("undefined", typeof doc2._conflicts);

  resp = dbB.view("mydesign/myview", {include_docs: true, conflicts: true});

  TEquals(2, resp.rows.length);
  TEquals(true, resp.rows[0].doc._conflicts instanceof Array);
  TEquals(1, resp.rows[0].doc._conflicts.length);
  TEquals(conflictRev, resp.rows[0].doc._conflicts[0]);
  TEquals("undefined", typeof resp.rows[1].doc._conflicts);

  // cleanup
  dbA.deleteDb();
  dbB.deleteDb();
};
