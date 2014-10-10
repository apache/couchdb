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

couchTests.reduce = function(debug) {
  var db = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"false"});
  db.deleteDb();
  db.createDb();
  if (debug) debugger;
  var numDocs = 500;
  var docs = makeDocs(1,numDocs + 1);
  db.bulkSave(docs);
  var summate = function(N) {return (N+1)*N/2;};

  var map = function (doc) {
      emit(doc.integer, doc.integer);
      emit(doc.integer, doc.integer);
  };
  var reduce = function (keys, values) { return sum(values); };
  var result = db.query(map, reduce);
  T(result.rows[0].value == 2*summate(numDocs));

  result = db.query(map, reduce, {startkey: 4, endkey: 4});
  T(result.rows[0].value == 8);

  result = db.query(map, reduce, {startkey: 4, endkey: 5});
  T(result.rows[0].value == 18);

  result = db.query(map, reduce, {startkey: 4, endkey: 6});
  T(result.rows[0].value == 30);

  result = db.query(map, reduce, {group:true, limit:3});
  T(result.rows[0].value == 2);
  T(result.rows[1].value == 4);
  T(result.rows[2].value == 6);

  for(var i=1; i<numDocs/2; i+=30) {
    result = db.query(map, reduce, {startkey: i, endkey: numDocs - i});
    T(result.rows[0].value == 2*(summate(numDocs-i) - summate(i-1)));
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
    reduce = function (keys, values) { return sum(values); };

    var results = db.query(map, reduce, {group:true});

    //group by exact key match
    T(equals(results.rows[0], {key:["a"],value:20*i}));
    T(equals(results.rows[1], {key:["a","b"],value:20*i}));
    T(equals(results.rows[2], {key:["a", "b", "c"],value:10*i}));
    T(equals(results.rows[3], {key:["a", "b", "d"],value:10*i}));

    // test to make sure group reduce and limit params provide valid json
    var results = db.query(map, reduce, {group: true, limit: 2});
    T(equals(results.rows[0], {key: ["a"], value: 20*i}));
    T(equals(results.rows.length, 2));

    //group by the first element in the key array
    var results = db.query(map, reduce, {group_level:1});
    T(equals(results.rows[0], {key:["a"],value:70*i}));
    T(equals(results.rows[1], {key:["d"],value:40*i}));

    //group by the first 2 elements in the key array
    var results = db.query(map, reduce, {group_level:2});
    T(equals(results.rows[0], {key:["a"],value:20*i}));
    T(equals(results.rows[1], {key:["a","b"],value:40*i}));
    T(equals(results.rows[2], {key:["a","c"],value:10*i}));
    T(equals(results.rows[3], {key:["d"],value:10*i}));
    T(equals(results.rows[4], {key:["d","a"],value:10*i}));
    T(equals(results.rows[5], {key:["d","b"],value:10*i}));
    T(equals(results.rows[6], {key:["d","c"],value:10*i}));

    // endkey test with inclusive_end=true
    var results = db.query(map, reduce, {group_level:2,endkey:["d"],inclusive_end:true});
    T(equals(results.rows[0], {key:["a"],value:20*i}));
    T(equals(results.rows[1], {key:["a","b"],value:40*i}));
    T(equals(results.rows[2], {key:["a","c"],value:10*i}));
    T(equals(results.rows[3], {key:["d"],value:10*i}));
    TEquals(4, results.rows.length);

    // endkey test with inclusive_end=false
    var results = db.query(map, reduce, {group_level:2,endkey:["d"],inclusive_end:false});
    T(equals(results.rows[0], {key:["a"],value:20*i}));
    T(equals(results.rows[1], {key:["a","b"],value:40*i}));
    T(equals(results.rows[2], {key:["a","c"],value:10*i}));
    TEquals(3, results.rows.length);
  }

  // now test out more complex reductions that need to use the combine option.

  db.deleteDb();
  db.createDb();


  var map = function (doc) { emit(doc.val, doc.val); };
  var reduceCombine = function (keys, values, rereduce) {
      // This computes the standard deviation of the mapped results
      var stdDeviation=0.0;
      var count=0;
      var total=0.0;
      var sqrTotal=0.0;

      if (!rereduce) {
        // This is the reduce phase, we are reducing over emitted values from
        // the map functions.
        for(var i in values) {
          total = total + values[i];
          sqrTotal = sqrTotal + (values[i] * values[i]);
        }
        count = values.length;
      }
      else {
        // This is the rereduce phase, we are re-reducing previosuly
        // reduced values.
        for(var i in values) {
          count = count + values[i].count;
          total = total + values[i].total;
          sqrTotal = sqrTotal + values[i].sqrTotal;
        }
      }

      var variance =  (sqrTotal - ((total * total)/count)) / count;
      stdDeviation = Math.sqrt(variance);

      // the reduce result. It contains enough information to be rereduced
      // with other reduce results.
      return {"stdDeviation":stdDeviation,"count":count,
          "total":total,"sqrTotal":sqrTotal};
    };

    // Save a bunch a docs.

  for(var i=0; i < 10; i++) {
    var docs = [];
    docs.push({val:10});
    docs.push({val:20});
    docs.push({val:30});
    docs.push({val:40});
    docs.push({val:50});
    docs.push({val:60});
    docs.push({val:70});
    docs.push({val:80});
    docs.push({val:90});
    docs.push({val:100});
    db.bulkSave(docs);
  }

  var results = db.query(map, reduceCombine);

  var difference = results.rows[0].value.stdDeviation - 28.722813232690143;
  // account for floating point rounding error
  T(Math.abs(difference) < 0.0000000001);

  function testReducePagination() {
    var ddoc = {
      "_id": "_design/test",
      "language": "javascript",
      "views": {
        "view1": {
          "map": "function(doc) {" +
             "emit(doc.int, doc._id);" +
             "emit(doc.int + 1, doc._id);" +
             "emit(doc.int + 2, doc._id);" +
          "}",
          "reduce": "_count"
        }
      }
    };
    var result, docs = [];

    function randVal() {
        return Math.random() * 100000000;
    }

    db.deleteDb();
    db.createDb();

    for (var i = 0; i < 1123; i++) {
      docs.push({"_id": String(i), "int": i});
    }
    db.bulkSave(docs.concat([ddoc]));

    // ?group=false tests
    result = db.view('test/view1', {startkey: 400, endkey: 402, foobar: randVal()});
    TEquals(9, result.rows[0].value);
    result = db.view('test/view1', {startkey: 402, endkey: 400, descending: true,
      foobar: randVal()});
    TEquals(9, result.rows[0].value);

    result = db.view('test/view1', {startkey: 400, endkey: 402, inclusive_end: false,
      foobar: randVal()});
    TEquals(6, result.rows[0].value);
    result = db.view('test/view1', {startkey: 402, endkey: 400, inclusive_end: false,
      descending: true, foobar: randVal()});
    TEquals(6, result.rows[0].value);

    result = db.view('test/view1', {startkey: 400, endkey: 402, endkey_docid: "400",
      foobar: randVal()});
    TEquals(7, result.rows[0].value);
    result = db.view('test/view1', {startkey: 400, endkey: 402, endkey_docid: "400",
      inclusive_end: false, foobar: randVal()});
    TEquals(6, result.rows[0].value);

    result = db.view('test/view1', {startkey: 400, endkey: 402, endkey_docid: "401",
      foobar: randVal()});
    TEquals(8, result.rows[0].value);
    result = db.view('test/view1', {startkey: 400, endkey: 402, endkey_docid: "401",
      inclusive_end: false, foobar: randVal()});
    TEquals(7, result.rows[0].value);

    result = db.view('test/view1', {startkey: 400, endkey: 402, endkey_docid: "402",
      foobar: randVal()});
    TEquals(9, result.rows[0].value);
    result = db.view('test/view1', {startkey: 400, endkey: 402, endkey_docid: "402",
      inclusive_end: false, foobar: randVal()});
    TEquals(8, result.rows[0].value);

    result = db.view('test/view1', {startkey: 402, endkey: 400, endkey_docid: "398",
      descending: true, foobar: randVal()});
    TEquals(9, result.rows[0].value);
    result = db.view('test/view1', {startkey: 402, endkey: 400, endkey_docid: "398",
      descending: true, inclusive_end: false, foobar: randVal()}),
    TEquals(8, result.rows[0].value);

    result = db.view('test/view1', {startkey: 402, endkey: 400, endkey_docid: "399",
      descending: true, foobar: randVal()});
    TEquals(8, result.rows[0].value);
    result = db.view('test/view1', {startkey: 402, endkey: 400, endkey_docid: "399",
      descending: true, inclusive_end: false, foobar: randVal()}),
    TEquals(7, result.rows[0].value);

    result = db.view('test/view1', {startkey: 402, endkey: 400, endkey_docid: "400",
      descending: true, foobar: randVal()}),
    TEquals(7, result.rows[0].value);
    result = db.view('test/view1', {startkey: 402, endkey: 400, endkey_docid: "400",
      descending: true, inclusive_end: false, foobar: randVal()}),
    TEquals(6, result.rows[0].value);

    result = db.view('test/view1', {startkey: 402, startkey_docid: "400", endkey: 400,
      descending: true, foobar: randVal()});
    TEquals(7, result.rows[0].value);

    result = db.view('test/view1', {startkey: 402, startkey_docid: "401", endkey: 400,
      descending: true, inclusive_end: false, foobar: randVal()});
    TEquals(5, result.rows[0].value);

    // ?group=true tests
    result = db.view('test/view1', {group: true, startkey: 400, endkey: 402,
      foobar: randVal()});
    TEquals(3, result.rows.length);
    TEquals(400, result.rows[0].key);
    TEquals(3, result.rows[0].value);
    TEquals(401, result.rows[1].key);
    TEquals(3, result.rows[1].value);
    TEquals(402, result.rows[2].key);
    TEquals(3, result.rows[2].value);

    result = db.view('test/view1', {group: true, startkey: 402, endkey: 400,
      descending: true, foobar: randVal()});
    TEquals(3, result.rows.length);
    TEquals(402, result.rows[0].key);
    TEquals(3, result.rows[0].value);
    TEquals(401, result.rows[1].key);
    TEquals(3, result.rows[1].value);
    TEquals(400, result.rows[2].key);
    TEquals(3, result.rows[2].value);

    result = db.view('test/view1', {group: true, startkey: 400, endkey: 402,
      inclusive_end: false, foobar: randVal()});
    TEquals(2, result.rows.length);
    TEquals(400, result.rows[0].key);
    TEquals(3, result.rows[0].value);
    TEquals(401, result.rows[1].key);
    TEquals(3, result.rows[1].value);

    result = db.view('test/view1', {group: true, startkey: 402, endkey: 400,
      descending: true, inclusive_end: false, foobar: randVal()});
    TEquals(2, result.rows.length);
    TEquals(402, result.rows[0].key);
    TEquals(3, result.rows[0].value);
    TEquals(401, result.rows[1].key);
    TEquals(3, result.rows[1].value);

    result = db.view('test/view1', {group: true, startkey: 400, endkey: 402,
      endkey_docid: "401", foobar: randVal()});
    TEquals(3, result.rows.length);
    TEquals(400, result.rows[0].key);
    TEquals(3, result.rows[0].value);
    TEquals(401, result.rows[1].key);
    TEquals(3, result.rows[1].value);
    TEquals(402, result.rows[2].key);
    TEquals(2, result.rows[2].value);

    result = db.view('test/view1', {group: true, startkey: 400, endkey: 402,
      endkey_docid: "400", foobar: randVal()});
    TEquals(3, result.rows.length);
    TEquals(400, result.rows[0].key);
    TEquals(3, result.rows[0].value);
    TEquals(401, result.rows[1].key);
    TEquals(3, result.rows[1].value);
    TEquals(402, result.rows[2].key);
    TEquals(1, result.rows[2].value);

    result = db.view('test/view1', {group: true, startkey: 402, startkey_docid: "401",
      endkey: 400, descending: true, foobar: randVal()});
    TEquals(3, result.rows.length);
    TEquals(402, result.rows[0].key);
    TEquals(2, result.rows[0].value);
    TEquals(401, result.rows[1].key);
    TEquals(3, result.rows[1].value);
    TEquals(400, result.rows[2].key);
    TEquals(3, result.rows[2].value);

    result = db.view('test/view1', {group: true, startkey: 402, startkey_docid: "400",
      endkey: 400, descending: true, foobar: randVal()});
    TEquals(3, result.rows.length);
    TEquals(402, result.rows[0].key);
    TEquals(1, result.rows[0].value);
    TEquals(401, result.rows[1].key);
    TEquals(3, result.rows[1].value);
    TEquals(400, result.rows[2].key);
    TEquals(3, result.rows[2].value);

    result = db.view('test/view1', {group: true, startkey: 402, startkey_docid: "401",
      endkey: 400, descending: true, inclusive_end: false, foobar: randVal()});
    TEquals(2, result.rows.length);
    TEquals(402, result.rows[0].key);
    TEquals(2, result.rows[0].value);
    TEquals(401, result.rows[1].key);
    TEquals(3, result.rows[1].value);

    result = db.view('test/view1', {group: true, startkey: 402, startkey_docid: "400",
      endkey: 400, descending: true, inclusive_end: false, foobar: randVal()});
    TEquals(2, result.rows.length);
    TEquals(402, result.rows[0].key);
    TEquals(1, result.rows[0].value);
    TEquals(401, result.rows[1].key);
    TEquals(3, result.rows[1].value);

    result = db.view('test/view1', {group: true, startkey: 402, endkey: 400,
      endkey_docid: "398", descending: true, inclusive_end: true, foobar: randVal()});
    TEquals(3, result.rows.length);
    TEquals(402, result.rows[0].key);
    TEquals(3, result.rows[0].value);
    TEquals(401, result.rows[1].key);
    TEquals(3, result.rows[1].value);
    TEquals(400, result.rows[2].key);
    TEquals(3, result.rows[2].value);

    result = db.view('test/view1', {group: true, startkey: 402, endkey: 400,
      endkey_docid: "399", descending: true, inclusive_end: true, foobar: randVal()});
    TEquals(3, result.rows.length);
    TEquals(402, result.rows[0].key);
    TEquals(3, result.rows[0].value);
    TEquals(401, result.rows[1].key);
    TEquals(3, result.rows[1].value);
    TEquals(400, result.rows[2].key);
    TEquals(2, result.rows[2].value);

    result = db.view('test/view1', {group: true, startkey: 402, endkey: 400,
      endkey_docid: "399", descending: true, inclusive_end: false, foobar: randVal()});
    TEquals(3, result.rows.length);
    TEquals(402, result.rows[0].key);
    TEquals(3, result.rows[0].value);
    TEquals(401, result.rows[1].key);
    TEquals(3, result.rows[1].value);
    TEquals(400, result.rows[2].key);
    TEquals(1, result.rows[2].value);

    result = db.view('test/view1', {group: true, startkey: 402, endkey: 400,
      endkey_docid: "400", descending: true, inclusive_end: false, foobar: randVal()});
    TEquals(2, result.rows.length);
    TEquals(402, result.rows[0].key);
    TEquals(3, result.rows[0].value);
    TEquals(401, result.rows[1].key);
    TEquals(3, result.rows[1].value);

    db.deleteDb();
  }

  testReducePagination();

};
