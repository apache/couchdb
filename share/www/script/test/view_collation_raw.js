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

couchTests.view_collation_raw = function(debug) {
  var db = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"false"});
  db.deleteDb();
  db.createDb();
  if (debug) debugger;

  // NOTE, the values are already in their correct sort order. Consider this
  // a specification of collation of json types.

  var values = [];

  //  numbers
  values.push(1);
  values.push(2);
  values.push(3);
  values.push(4);
  
  values.push(false);
  values.push(null);
  values.push(true);
  
  // then object, compares each key value in the list until different.
  // larger objects sort after their subset objects.
  values.push({a:1});
  values.push({a:2});
  values.push({b:1});
  values.push({b:2});
  values.push({b:2, a:1}); // Member order does matter for collation.
                           // CouchDB preserves member order
                           // but doesn't require that clients will.
                           // (this test might fail if used with a js engine
                           // that doesn't preserve order)
  values.push({b:2, c:2});

  // then arrays. compared element by element until different.
  // Longer arrays sort after their prefixes
  values.push(["a"]);
  values.push(["b"]);
  values.push(["b","c"]);
  values.push(["b","c", "a"]);
  values.push(["b","d"]);
  values.push(["b","d", "e"]);


  // then text, case sensitive
  values.push("A");
  values.push("B");
  values.push("a");
  values.push("aa");
  values.push("b");
  values.push("ba");
  values.push("bb");

  for (var i=0; i<values.length; i++) {
    db.save({_id:(i).toString(), foo:values[i]});
  }

  var designDoc = {
    _id:"_design/test", // turn off couch.js id escaping?
    language: "javascript",
    views: {
      test: {map: "function(doc) { emit(doc.foo, null); }",
            options: {collation:"raw"}}
    }
  }
  T(db.save(designDoc).ok);

  // Confirm that everything collates correctly.
  var rows = db.view("test/test").rows;
  for (i=0; i<values.length; i++) {
    T(equals(rows[i].key, values[i]));
  }

  // Confirm that couch allows raw semantics in key ranges.
  rows = db.view("test/test", {startkey:"Z", endkey:"a"}).rows;
  TEquals(1, rows.length);
  TEquals("a", rows[0].key);

  // Check the descending output.
  rows = db.view("test/test", {descending: true}).rows;
  for (i=0; i<values.length; i++) {
    T(equals(rows[i].key, values[values.length - 1 -i]));
  }

  // now check the key query args
  for (i=1; i<values.length; i++) {
    rows = db.view("test/test", {key:values[i]}).rows;
    T(rows.length == 1 && equals(rows[0].key, values[i]));
  }

  // test inclusive_end=true (the default)
  // the inclusive_end=true functionality is limited to endkey currently
  // if you need inclusive_start=false for startkey, please do implement. ;)
  var rows = db.view("test/test", {endkey : "b", inclusive_end:true}).rows;
  T(rows[rows.length-1].key == "b");
  // descending=true
  var rows = db.view("test/test", {endkey : "b",
    descending:true, inclusive_end:true}).rows;
  T(rows[rows.length-1].key == "b");

  // test inclusive_end=false
  var rows = db.view("test/test", {endkey : "b", inclusive_end:false}).rows;
  T(rows[rows.length-1].key == "aa");
  // descending=true
  var rows = db.view("test/test", {endkey : "b",
    descending:true, inclusive_end:false}).rows;
  T(rows[rows.length-1].key == "ba");
  
  var rows = db.view("test/test", {
    endkey : "b", endkey_docid: "10",
    inclusive_end:false}).rows;
  T(rows[rows.length-1].key == "aa");
  
  var rows = db.view("test/test", {
    endkey : "b", endkey_docid: "11",
    inclusive_end:false}).rows;
  T(rows[rows.length-1].key == "aa");
};
