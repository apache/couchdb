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

couchTests.view_builtin = function(debug) {
  var db = new CouchDB("test_suite_db");
  db.deleteDb();
  db.createDb();
  if (debug) debugger;

  // test _conflicts view

  // no rows
    var result = db.conflicts();
    TEquals(0, result.rows.length, "should return 0 conflicts");

  // one doc with a conflict
    var doc_a = db.save({_id:"a", a:1});

    // create conflict
    var bulk_result = db.bulkSave([{_id:"a",a:2}], {all_or_nothing:true});
    var result = db.conflicts();
    TEquals(1, result.rows.length, "should return 1 conflicts");
    TEquals("a", result.rows[0].id, "should return row id 'a'");
    TEquals("a", result.rows[0].key, "should return row key 'a'");
    TEquals(bulk_result[0].rev, result.rows[0].rev, 
      "should return row key 'a'");

  // multiple docs with conflicts
    var doc_b = db.save({_id:"b", a:3});
    var bulk_result = db.bulkSave([{_id:"b",a:4}], {all_or_nothing:true});
    var result = db.conflicts();
    TEquals(2, result.rows.length, "should return 2 conflicts");

  // key, startkey, endkey, skip & count
    var result = db.conflicts({key:"b"});
    TEquals(1, result.rows.length, "should return 1 conflicts");

    var result = db.conflicts({startkey:"b"});
    TEquals(1, result.rows.length, "should return 1 conflicts");

    var result = db.conflicts({startkey:"a", endkey:"b"});
    TEquals(2, result.rows.length, "should return 2 conflicts");

    var result = db.conflicts({startkey:"c"});
    TEquals(0, result.rows.length, "should return 0 conflicts");

    var result = db.conflicts({limit:1});
    TEquals(1, result.rows.length, "should return 1 conflicts");

    var result = db.conflicts({skip:1});
    TEquals(1, result.rows.length, "should return 1 conflicts");
    TEquals("b", result.rows[0].key, "should return row key 'b'");

  // POST is not allowed yet
    try {
      var result = db.conflicts({}, ["a"]);
    } catch (e) {
      TEquals("method_not_allowed", e.error, "should not allow POST requests");
    }

  // multi key get
    // var result = db.conflicts({}, ["a"]);
    //  TEquals(1, result.rows.length, "should return 1 conflicts");
    //  TEquals("a", result.rows[0].key, "should return row key 'a'");
};
