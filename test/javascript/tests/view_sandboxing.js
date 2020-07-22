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

couchTests.elixir = true
couchTests.view_sandboxing = function(debug) {
  var db_name = get_random_db_name();
  var db = new CouchDB(db_name, {"X-Couch-Full-Commit":"false"});
  db.createDb();
  if (debug) debugger;

  var doc = {integer: 1, string: "1", array: [1, 2, 3]};
  T(db.save(doc).ok);

  // make sure that attempting to change the document throws an error
  var results = db.query(function(doc) {
    doc.integer = 2;
    emit(null, doc);
  }, null, {"include_docs": true});
  // either we have an error or our doc is unchanged
  T(results.total_rows == 0 || results.rows[0].doc.integer == 1);

  var results = db.query(function(doc) {
    doc.array[0] = 0;
    emit(null, doc);
  }, null, {"include_docs": true});
  // either we have an error or our doc is unchanged
  T(results.total_rows == 0 || results.rows[0].doc.array[0] == 1);

  // make sure that a view cannot invoke interpreter internals such as the
  // garbage collector
  var results = db.query(function(doc) {
    gc();
    emit(null, doc);
  });
  T(results.total_rows == 0);

  // make sure that a view cannot access the map_funs array defined used by
  // the view server
  var results = db.query(function(doc) { map_funs.push(1); emit(null, doc); });
  T(results.total_rows == 0);

  // make sure that a view cannot access the map_results array defined used by
  // the view server
  var results = db.query(function(doc) { map_results.push(1); emit(null, doc); });
  T(results.total_rows == 0);

  // test for COUCHDB-925
  // altering 'doc' variable in map function affects other map functions
  var ddoc = {
    _id: "_design/foobar",
    language: "javascript",
    views: {
      view1: {
        map:
          (function(doc) {
            if (doc.values) {
              doc.values = [666];
            }
            if (doc.tags) {
              doc.tags.push("qwerty");
            }
            if (doc.tokens) {
              doc.tokens["c"] = 3;
            }
          }).toString()
      },
      view2: {
        map:
          (function(doc) {
            if (doc.values) {
              emit(doc._id, doc.values);
            }
            if (doc.tags) {
              emit(doc._id, doc.tags);
            }
            if (doc.tokens) {
              emit(doc._id, doc.tokens);
            }
          }).toString()
      }
    }
  };
  var doc1 = {
    _id: "doc1",
    values: [1, 2, 3]
  };
  var doc2 = {
    _id: "doc2",
    tags: ["foo", "bar"],
    tokens: {a: 1, b: 2}
  };

  db.deleteDb();
  // avoid Heisenbugs when files are not cleared entirely
  db_name = get_random_db_name();
  db = new CouchDB(db_name, {"X-Couch-Full-Commit":"false"});
  db.createDb();
  T(db.save(ddoc).ok);
  T(db.save(doc1).ok);
  T(db.save(doc2).ok);

  var view1Results = db.view(
    "foobar/view1", {bypass_cache: Math.round(Math.random() * 1000)});
  var view2Results = db.view(
    "foobar/view2", {bypass_cache: Math.round(Math.random() * 1000)});

  TEquals(0, view1Results.rows.length, "view1 has 0 rows");
  TEquals(3, view2Results.rows.length, "view2 has 3 rows");

  TEquals(doc1._id, view2Results.rows[0].key);
  TEquals(doc2._id, view2Results.rows[1].key);
  TEquals(doc2._id, view2Results.rows[2].key);

  // https://bugzilla.mozilla.org/show_bug.cgi?id=449657
  TEquals(3, view2Results.rows[0].value.length,
    "Warning: installed SpiderMonkey version doesn't allow sealing of arrays");
  if (view2Results.rows[0].value.length === 3) {
    TEquals(1, view2Results.rows[0].value[0]);
    TEquals(2, view2Results.rows[0].value[1]);
    TEquals(3, view2Results.rows[0].value[2]);
  }

  // we can't be 100% sure about the order for the same key
  T(view2Results.rows[1].value["a"] == 1 || view2Results.rows[1].value[0] == "foo");
  T(view2Results.rows[1].value["b"] == 2 || view2Results.rows[1].value[1] == "bar");
  T(view2Results.rows[2].value["a"] == 1 || view2Results.rows[2].value[0] == "foo");
  T(view2Results.rows[2].value["b"] == 2 || view2Results.rows[2].value[1] == "bar");
  TEquals('undefined', typeof view2Results.rows[1].value["c"], "doc2.tokens object was not sealed");
  TEquals('undefined', typeof view2Results.rows[2].value["c"], "doc2.tokens object was not sealed");

/* (see above)
  TEquals(2, view2Results.rows[2].value.length,
    "Warning: installed SpiderMonkey version doesn't allow sealing of arrays");
  if (view2Results.rows[2].value.length === 2) {
    TEquals("foo", view2Results.rows[2].value[0]);
    TEquals("bar", view2Results.rows[2].value[1]);
  }
*/

  // cleanup
  db.deleteDb();

  // test that runtime code evaluation can be prevented
  db_name = get_random_db_name();
  db = new CouchDB(db_name, {"X-Couch-Full-Commit":"false"});
  db.createDb();

  var doc = {integer: 1, string: "1", array: [1, 2, 3]};
  T(db.save(doc).ok);

  var results = db.query(function(doc) {
      var glob = emit.constructor('return this')();
      emit(doc._id, null);
  });

  TEquals(0, results.rows.length);

  // cleanup
  db.deleteDb();
};
