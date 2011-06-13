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

couchTests.reduce_builtin = function(debug) {
  var db = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"false"});
  db.deleteDb();
  db.createDb();
  if (debug) debugger;

  var numDocs = 500;
  var docs = makeDocs(1,numDocs + 1);
  db.bulkSave(docs);

  var summate = function(N) {return (N+1)*N/2;};

  var sumsqr = function(N) { 
    var acc = 0;
    for (var i=1; i<=N; ++i) {
      acc += i*i;
    }
    return acc;
  };

  // this is the same test as the reduce.js test
  // only we'll let CouchDB run reduce in Erlang
  var map = function (doc) {
      emit(doc.integer, doc.integer);
      emit(doc.integer, doc.integer);
  };

  var result = db.query(map, "_sum");
  T(result.rows[0].value == 2*summate(numDocs));
  result = db.query(map, "_count");
  T(result.rows[0].value == 1000);
  result = db.query(map, "_stats");
  T(result.rows[0].value.sum == 2*summate(numDocs));
  T(result.rows[0].value.count == 1000);
  T(result.rows[0].value.min == 1);
  T(result.rows[0].value.max == 500);
  T(result.rows[0].value.sumsqr == 2*sumsqr(numDocs));

  result = db.query(map, "_sum", {startkey: 4, endkey: 4});
  T(result.rows[0].value == 8);
  result = db.query(map, "_count", {startkey: 4, endkey: 4});
  T(result.rows[0].value == 2);

  result = db.query(map, "_sum", {startkey: 4, endkey: 5});
  T(result.rows[0].value == 18);
  result = db.query(map, "_count", {startkey: 4, endkey: 5});
  T(result.rows[0].value == 4);

  result = db.query(map, "_sum", {startkey: 4, endkey: 6});
  T(result.rows[0].value == 30);
  result = db.query(map, "_count", {startkey: 4, endkey: 6});
  T(result.rows[0].value == 6);

  result = db.query(map, "_sum", {group:true, limit:3});
  T(result.rows[0].value == 2);
  T(result.rows[1].value == 4);
  T(result.rows[2].value == 6);

  for(var i=1; i<numDocs/2; i+=30) {
    result = db.query(map, "_sum", {startkey: i, endkey: numDocs - i});
    T(result.rows[0].value == 2*(summate(numDocs-i) - summate(i-1)));
  }

  // test for trailing characters after builtin functions, desired behaviour
  // is to disregard any trailing characters
  // I think the behavior should be a prefix test, so that even "_statsorama" 
  // or "_stats\nare\awesome" should work just as "_stats" does. - JChris

  var trailing = ["\u000a", "orama", "\nare\nawesome", " ", "     \n  "];

  for(var i=0; i < trailing.length; i++) {
    result = db.query(map, "_sum" + trailing[i]);
    T(result.rows[0].value == 2*summate(numDocs));
    result = db.query(map, "_count" + trailing[i]);
    T(result.rows[0].value == 1000);
    result = db.query(map, "_stats" + trailing[i]);
    T(result.rows[0].value.sum == 2*summate(numDocs));
    T(result.rows[0].value.count == 1000);
    T(result.rows[0].value.min == 1);
    T(result.rows[0].value.max == 500);
    T(result.rows[0].value.sumsqr == 2*sumsqr(numDocs));
  }

  db.deleteDb();
  db.createDb();

  for(var i=1; i <= 5; i++) {

    for(var j=0; j < 10; j++) {
      // these docs are in the order of the keys collation, for clarity
      var docs = [];
      docs.push({keys:["a"]});
      docs.push({keys:["a"]});
      docs.push({keys:["a", "b"]});
      docs.push({keys:["a", "b"]});
      docs.push({keys:["a", "b", "c"]});
      docs.push({keys:["a", "b", "d"]});
      docs.push({keys:["a", "c", "d"]});
      docs.push({keys:["d"]});
      docs.push({keys:["d", "a"]});
      docs.push({keys:["d", "b"]});
      docs.push({keys:["d", "c"]});
      db.bulkSave(docs);
      T(db.info().doc_count == ((i - 1) * 10 * 11) + ((j + 1) * 11));
    }

    map = function (doc) { emit(doc.keys, 1); };
    // with emitted values being 1, count should be the same as sum
    var builtins = ["_sum", "_count"];

    for (var b=0; b < builtins.length; b++) {
      var fun = builtins[b];
      var results = db.query(map, fun, {group:true});

      //group by exact key match
      T(equals(results.rows[0], {key:["a"],value:20*i}));
      T(equals(results.rows[1], {key:["a","b"],value:20*i}));
      T(equals(results.rows[2], {key:["a", "b", "c"],value:10*i}));
      T(equals(results.rows[3], {key:["a", "b", "d"],value:10*i}));

      // test to make sure group reduce and limit params provide valid json
      var results = db.query(map, fun, {group: true, limit: 2});
      T(equals(results.rows[0], {key: ["a"], value: 20*i}));
      T(equals(results.rows.length, 2));

      //group by the first element in the key array
      var results = db.query(map, fun, {group_level:1});
      T(equals(results.rows[0], {key:["a"],value:70*i}));
      T(equals(results.rows[1], {key:["d"],value:40*i}));

      //group by the first 2 elements in the key array
      var results = db.query(map, fun, {group_level:2});
      T(equals(results.rows[0], {key:["a"],value:20*i}));
      T(equals(results.rows[1], {key:["a","b"],value:40*i}));
      T(equals(results.rows[2], {key:["a","c"],value:10*i}));
      T(equals(results.rows[3], {key:["d"],value:10*i}));
      T(equals(results.rows[4], {key:["d","a"],value:10*i}));
      T(equals(results.rows[5], {key:["d","b"],value:10*i}));
      T(equals(results.rows[6], {key:["d","c"],value:10*i}));
    };

    map = function (doc) { emit(doc.keys, [1, 1]); };

    var results = db.query(map, "_sum", {group:true});
    T(equals(results.rows[0], {key:["a"],value:[20*i,20*i]}));
    T(equals(results.rows[1], {key:["a","b"],value:[20*i,20*i]}));
    T(equals(results.rows[2], {key:["a", "b", "c"],value:[10*i,10*i]}));
    T(equals(results.rows[3], {key:["a", "b", "d"],value:[10*i,10*i]}));

    var results = db.query(map, "_sum", {group: true, limit: 2});
    T(equals(results.rows[0], {key: ["a"], value: [20*i,20*i]}));
    T(equals(results.rows.length, 2));

    var results = db.query(map, "_sum", {group_level:1});
    T(equals(results.rows[0], {key:["a"],value:[70*i,70*i]}));
    T(equals(results.rows[1], {key:["d"],value:[40*i,40*i]}));

    var results = db.query(map, "_sum", {group_level:2});
    T(equals(results.rows[0], {key:["a"],value:[20*i,20*i]}));
    T(equals(results.rows[1], {key:["a","b"],value:[40*i,40*i]}));
    T(equals(results.rows[2], {key:["a","c"],value:[10*i,10*i]}));
    T(equals(results.rows[3], {key:["d"],value:[10*i,10*i]}));
    T(equals(results.rows[4], {key:["d","a"],value:[10*i,10*i]}));
    T(equals(results.rows[5], {key:["d","b"],value:[10*i,10*i]}));
    T(equals(results.rows[6], {key:["d","c"],value:[10*i,10*i]}));
  }
}
