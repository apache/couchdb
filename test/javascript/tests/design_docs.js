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

couchTests.design_docs = function(debug) {
  var db = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"false"});
  var db2 = new CouchDB("test_suite_db_a", {"X-Couch-Full-Commit":"false"});

  if (debug) debugger;

  db.deleteDb();
  db.createDb();
  db2.deleteDb();
  db2.createDb();

  var server_config = [
    {
      section: "query_server_config",
      key: "reduce_limit",
      value: "false"
    }
  ];

  var testFun = function() {
    var numDocs = 500;

    function makebigstring(power) {
      var str = "a";
      while(power-- > 0) {
        str = str + str;
      }
      return str;
    }

    var designDoc = {
      _id: "_design/test",
      language: "javascript",
      whatever : {
        stringzone : "exports.string = 'plankton';",
        commonjs : {
          whynot : "exports.test = require('../stringzone'); " +
            "exports.foo = require('whatever/stringzone');",
          upper : "exports.testing = require('./whynot').test.string.toUpperCase()+" +
            "module.id+require('./whynot').foo.string",
          circular_one: "require('./circular_two'); exports.name = 'One';",
          circular_two: "require('./circular_one'); exports.name = 'Two';"
        },
        // paths relative to parent
        idtest1: {
          a: {
            b: {d: "module.exports = require('../c/e').id;"},
            c: {e: "exports.id = module.id;"}
          }
        },
        // multiple paths relative to parent
        idtest2: {
          a: {
            b: {d: "module.exports = require('../../a/c/e').id;"},
            c: {e: "exports.id = module.id;"}
          }
        },
        // paths relative to module
        idtest3: {
          a: {
            b: "module.exports = require('./c/d').id;",
            c: {
              d: "module.exports = require('./e');",
              e: "exports.id = module.id;"
            }
          }
        },
        // paths relative to module and parent
        idtest4: {
          a: {
            b: "module.exports = require('../a/./c/d').id;",
            c: {
              d: "module.exports = require('./e');",
              e: "exports.id = module.id;"
            }
          }
        },
        // paths relative to root
        idtest5: {
          a: "module.exports = require('whatever/idtest5/b').id;",
          b: "exports.id = module.id;"
        }
      },
      views: {
        all_docs_twice: {
          map:
            (function(doc) {
              emit(doc.integer, null);
              emit(doc.integer, null);
            }).toString()
        },
        no_docs: {
          map:
            (function(doc) {
            }).toString()
        },
        single_doc: {
          map:
            (function(doc) {
              if (doc._id === "1") {
                emit(1, null);
              }
            }).toString()
        },
        summate: {
          map:
            (function(doc) {
              emit(doc.integer, doc.integer);
            }).toString(),
          reduce:
            (function(keys, values) {
              return sum(values);
            }).toString()
        },
        summate2: {
          map:
            (function(doc) {
              emit(doc.integer, doc.integer);
            }).toString(),
          reduce:
            (function(keys, values) {
              return sum(values);
            }).toString()
        },
        huge_src_and_results: {
          map:
            (function(doc) {
              if (doc._id === "1") {
                emit(makebigstring(16), null);
              }
            }).toString(),
          reduce:
            (function(keys, values) {
              return makebigstring(16);
            }).toString()
        },
        lib : {
          baz : "exports.baz = 'bam';",
          foo : {
            foo : "exports.foo = 'bar';",
            boom : "exports.boom = 'ok';",
            zoom : "exports.zoom = 'yeah';"
          }
        },
        commonjs : {
          map :
            (function(doc) {
              emit(null, require('views/lib/foo/boom').boom);
            }).toString()
        }
      },
      shows: {
        simple:
          (function() {
            return 'ok';
          }).toString(),
        requirey:
          (function() {
            var lib = require('whatever/commonjs/upper');
            return lib.testing;
          }).toString(),
        circular:
          (function() {
            var lib = require('whatever/commonjs/upper');
            return JSON.stringify(this);
          }).toString(),
        circular_require:
          (function() {
            return require('whatever/commonjs/circular_one').name;
          }).toString(),
        idtest1: (function() {
            return require('whatever/idtest1/a/b/d');
          }).toString(),
        idtest2: (function() {
            return require('whatever/idtest2/a/b/d');
          }).toString(),
        idtest3: (function() {
            return require('whatever/idtest3/a/b');
          }).toString(),
        idtest4: (function() {
            return require('whatever/idtest4/a/b');
          }).toString(),
        idtest5: (function() {
            return require('whatever/idtest5/a');
          }).toString()
      }
    }; // designDoc

    var xhr = CouchDB.request(
      "PUT", "/test_suite_db_a/_design/test", {body: JSON.stringify(designDoc)}
    );
    var resp = JSON.parse(xhr.responseText);

    TEquals(resp.rev, db.save(designDoc).rev);

    // test that editing a show fun on the ddoc results in a change in output
    xhr = CouchDB.request("GET", "/test_suite_db/_design/test/_show/simple");
    T(xhr.status == 200);
    TEquals(xhr.responseText, "ok");

    designDoc.shows.simple = (function() {
      return 'ko';
    }).toString();
    T(db.save(designDoc).ok);

    xhr = CouchDB.request("GET", "/test_suite_db/_design/test/_show/simple");
    T(xhr.status == 200);
    TEquals(xhr.responseText, "ko");

    xhr = CouchDB.request(
      "GET", "/test_suite_db_a/_design/test/_show/simple?cache=buster"
    );
    T(xhr.status == 200);
    TEquals("ok", xhr.responseText, 'query server used wrong ddoc');

    // test commonjs require
    xhr = CouchDB.request("GET", "/test_suite_db/_design/test/_show/requirey");
    T(xhr.status == 200);
    TEquals("PLANKTONwhatever/commonjs/upperplankton", xhr.responseText);

    xhr = CouchDB.request("GET", "/test_suite_db/_design/test/_show/circular");
    T(xhr.status == 200);
    TEquals("javascript", JSON.parse(xhr.responseText).language);

    // test circular commonjs dependencies
    xhr = CouchDB.request(
      "GET",
      "/test_suite_db/_design/test/_show/circular_require"
    );
    TEquals(200, xhr.status);
    TEquals("One", xhr.responseText);

    // Test that changes to the design doc properly invalidate cached modules:

    // update the designDoc and replace
    designDoc.whatever.commonjs.circular_one = "exports.name = 'Updated';"
    T(db.save(designDoc).ok);

    // request circular_require show function again and check the response has
    // changed
    xhr = CouchDB.request(
      "GET",
      "/test_suite_db/_design/test/_show/circular_require"
    );
    TEquals(200, xhr.status);
    TEquals("Updated", xhr.responseText);


    // test module id values are as expected:
    xhr = CouchDB.request("GET", "/test_suite_db/_design/test/_show/idtest1");
    TEquals(200, xhr.status);
    TEquals("whatever/idtest1/a/c/e", xhr.responseText);

    xhr = CouchDB.request("GET", "/test_suite_db/_design/test/_show/idtest2");
    TEquals(200, xhr.status);
    TEquals("whatever/idtest2/a/c/e", xhr.responseText);

    xhr = CouchDB.request("GET", "/test_suite_db/_design/test/_show/idtest3");
    TEquals(200, xhr.status);
    TEquals("whatever/idtest3/a/c/e", xhr.responseText);

    xhr = CouchDB.request("GET", "/test_suite_db/_design/test/_show/idtest4");
    TEquals(200, xhr.status);
    TEquals("whatever/idtest4/a/c/e", xhr.responseText);

    xhr = CouchDB.request("GET", "/test_suite_db/_design/test/_show/idtest5");
    TEquals(200, xhr.status);
    TEquals("whatever/idtest5/b", xhr.responseText);


    var prev_view_sig = db.designInfo("_design/test").view_index.signature;
    var prev_view_size = db.designInfo("_design/test").view_index.disk_size;

    db.bulkSave(makeDocs(1, numDocs + 1));
    T(db.ensureFullCommit().ok);

    // test that we get correct design doc info back,
    // and also that GET /db/_design/test/_info
    // hasn't triggered an update of the views
    db.view("test/summate", {stale: "ok"}); // make sure view group's open
    for (var i = 0; i < 2; i++) {
      var dinfo = db.designInfo("_design/test");
      TEquals("test", dinfo.name);
      var vinfo = dinfo.view_index;
      TEquals(prev_view_size, vinfo.disk_size, "view group disk size didn't change");
      TEquals(false, vinfo.compact_running);
      TEquals(prev_view_sig, vinfo.signature, 'ddoc sig');
      // wait some time (there were issues where an update
      // of the views had been triggered in the background)
      var start = new Date().getTime();
      while (new Date().getTime() < start + 2000);
      TEquals(0, db.view("test/all_docs_twice", {stale: "ok"}).total_rows, 'view info');
      TEquals(0, db.view("test/single_doc", {stale: "ok"}).total_rows, 'view info');
      TEquals(0, db.view("test/summate", {stale: "ok"}).rows.length, 'view info');
      T(db.ensureFullCommit().ok);
      restartServer();
    };

    db.bulkSave(makeDocs(numDocs + 1, numDocs * 2 + 1));
    T(db.ensureFullCommit().ok);

    // open view group
    db.view("test/summate", {stale: "ok"});
    // wait so the views can get initialized
    var start = new Date().getTime();
    while (new Date().getTime() < start + 2000);

    // test that POST /db/_view_cleanup
    // doesn't trigger an update of the views
    var len1 = db.view("test/all_docs_twice", {stale: "ok"}).total_rows;
    var len2 = db.view("test/single_doc", {stale: "ok"}).total_rows;
    var len3 = db.view("test/summate", {stale: "ok"}).rows.length;
    for (i = 0; i < 2; i++) {
      T(db.viewCleanup().ok);
      // wait some time (there were issues where an update
      // of the views had been triggered in the background)
      start = new Date().getTime();
      while (new Date().getTime() < start + 2000);
      TEquals(len1, db.view("test/all_docs_twice", {stale: "ok"}).total_rows, 'view cleanup');
      TEquals(len2, db.view("test/single_doc", {stale: "ok"}).total_rows, 'view cleanup');
      TEquals(len3, db.view("test/summate", {stale: "ok"}).rows.length, 'view cleanup');
      T(db.ensureFullCommit().ok);
      restartServer();
      // we'll test whether the view group stays closed
      // and the views stay uninitialized (they should!)
      len1 = len2 = len3 = 0;
    };

    // test commonjs in map functions
    resp = db.view("test/commonjs", {limit:1});
    T(resp.rows[0].value == 'ok');

    // test that the _all_docs view returns correctly with keys
    var results = db.allDocs({startkey:"_design", endkey:"_design0"});
    T(results.rows.length == 1);

    for (i = 0; i < 2; i++) {
      var rows = db.view("test/all_docs_twice").rows;
      for (var j = 0; j < numDocs; j++) {
        T(rows[2 * j].key == (j + 1));
        T(rows[(2 * j) + 1].key == (j + 1));
      };
      T(db.view("test/no_docs").total_rows == 0);
      T(db.view("test/single_doc").total_rows == 1);
      T(db.ensureFullCommit().ok);
      restartServer();
    };

    // test when language not specified, Javascript is implied
    var designDoc2 = {
      _id: "_design/test2",
      // language: "javascript",
      views: {
        single_doc: {
          map:
            (function(doc) {
              if (doc._id === "1") {
                emit(1, null);
              }
            }).toString()
        }
      }
    };

    T(db.save(designDoc2).ok);
    T(db.view("test2/single_doc").total_rows == 1);

    var summate = function(N) {
      return (N + 1) * (N / 2);
    };
    var result = db.view("test/summate");
    T(result.rows[0].value == summate(numDocs * 2));

    result = db.view("test/summate", {startkey: 4, endkey: 4});
    T(result.rows[0].value == 4);

    result = db.view("test/summate", {startkey: 4, endkey: 5});
    T(result.rows[0].value == 9);

    result = db.view("test/summate", {startkey: 4, endkey: 6});
    T(result.rows[0].value == 15);

    // test start_key and end_key aliases
    result = db.view("test/summate", {start_key: 4, end_key: 6});
    T(result.rows[0].value == 15);

    // Verify that a shared index (view def is an exact copy of "summate")
    // does not confuse the reduce stage
    result = db.view("test/summate2", {startkey: 4, endkey: 6});
    T(result.rows[0].value == 15);

    for(i = 1; i < (numDocs / 2); i += 30) {
      result = db.view("test/summate", {startkey: i, endkey: (numDocs - i)});
      T(result.rows[0].value == summate(numDocs - i) - summate(i - 1));
    }

    T(db.deleteDoc(designDoc).ok);
    T(db.open(designDoc._id) == null);
    T(db.view("test/no_docs") == null);

    T(db.ensureFullCommit().ok);
    restartServer();
    T(db.open(designDoc._id) == null);
    T(db.view("test/no_docs") == null);

    // trigger ddoc cleanup
    T(db.viewCleanup().ok);
  }; // enf of testFun

  run_on_modified_server(server_config, testFun);

  // COUCHDB-1227 - if a design document is deleted, by adding a "_deleted"
  // field with the boolean value true, its validate_doc_update functions
  // should no longer have effect.
  db.deleteDb();
  db.createDb();
  var ddoc = {
    _id: "_design/test",
    language: "javascript",
    validate_doc_update: (function(newDoc, oldDoc, userCtx, secObj) {
       if (newDoc.value % 2 == 0) {
          throw({forbidden: "dont like even numbers"});
       }
       return true;
    }).toString()
  };

  TEquals(true, db.save(ddoc).ok);
  try {
    db.save({_id: "doc1", value: 4});
    T(false, "doc insertion should have failed");
  } catch (x) {
    TEquals("forbidden", x.error);
  }

  var doc = db.open("doc1");
  TEquals(null, doc);
  ddoc._deleted = true;
  TEquals(true, db.save(ddoc).ok);

  try {
    TEquals(true, db.save({_id: "doc1", value: 4}).ok);
  } catch (x) {
    T(false, "doc insertion should have succeeded");
  }

  doc = db.open("doc1");
  TEquals(true, doc !== null, "doc was not persisted");
  TEquals(4, doc.value);

  // cleanup
  db.deleteDb();
  db2.deleteDb();
};
