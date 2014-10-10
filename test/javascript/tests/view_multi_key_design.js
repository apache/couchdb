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

couchTests.view_multi_key_design = function(debug) {
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
      multi_emit: {
        map: "function(doc) {for(var i = 0 ; i < 3 ; i++) { emit(i, doc.integer) ; } }"
      },
      summate: {
        map:"function (doc) {emit(doc.integer, doc.integer)};",
        reduce:"function (keys, values) { return sum(values); };"
      }
    }
  };
  T(db.save(designDoc).ok);

  // Test that missing keys work too
  var keys = [101,30,15,37,50];
  var reduce = db.view("test/summate",{group:true},keys).rows;
  T(reduce.length == keys.length-1); // 101 is missing
  for(var i=0; i<reduce.length; i++) {
    T(keys.indexOf(reduce[i].key) != -1);
    T(reduce[i].key == reduce[i].value);
  }

  // First, the goods:
  var keys = [10,15,30,37,50];
  var rows = db.view("test/all_docs",{},keys).rows;
  for(var i=0; i<rows.length; i++) {
    T(keys.indexOf(rows[i].key) != -1);
    T(rows[i].key == rows[i].value);
  }

  // with GET keys
  rows = db.view("test/all_docs",{keys:keys},null).rows;
  for(var i=0;i<rows.length; i++) {
    T(keys.indexOf(rows[i].key) != -1);
    T(rows[i].key == rows[i].value);
  }

  // with empty keys
  rows = db.view("test/all_docs",{keys:[]},null).rows;
  T(rows.length == 0);

  var reduce = db.view("test/summate",{group:true},keys).rows;
  T(reduce.length == keys.length);
  for(var i=0; i<reduce.length; i++) {
    T(keys.indexOf(reduce[i].key) != -1);
    T(reduce[i].key == reduce[i].value);
  }

  // with GET keys
  reduce = db.view("test/summate",{group:true,keys:keys},null).rows;
  T(reduce.length == keys.length);
  for(var i=0; i<reduce.length; i++) {
    T(keys.indexOf(reduce[i].key) != -1);
    T(reduce[i].key == reduce[i].value);
  }

  // Test that invalid parameter combinations get rejected
  var badargs = [{startkey:0}, {endkey:0}, {key: 0}, {group_level: 2}];
  var getbadargs = [{startkey:0, keys:keys}, {endkey:0, keys:keys}, 
      {key:0, keys:keys}, {group_level: 2, keys:keys}];
  for(var i in badargs)
  {
      try {
          db.view("test/all_docs",badargs[i],keys);
          T(0==1);
      } catch (e) {
          T(e.error == "query_parse_error");
      }

      try {
          db.view("test/all_docs",getbadargs[i],null);
          T(0==1);
      } catch (e) {
          T(e.error = "query_parse_error");
      }
  }

  try {
      db.view("test/summate",{},keys);
      T(0==1);
  } catch (e) {
      T(e.error == "query_parse_error");
  }

  try {
      db.view("test/summate",{keys:keys},null);
      T(0==1);
  } catch (e) {
      T(e.error == "query_parse_error");
  }

  // Test that a map & reduce containing func support keys when reduce=false
  var resp = db.view("test/summate", {reduce: false}, keys);
  T(resp.rows.length == 5);

  resp = db.view("test/summate", {reduce: false, keys: keys}, null);
  T(resp.rows.length == 5);

  // Check that limiting by startkey_docid and endkey_docid get applied
  // as expected.
  var curr = db.view("test/multi_emit", {startkey_docid: 21, endkey_docid: 23}, [0, 2]).rows;
  var exp_key = [ 0,  0,  0,  2,  2,  2] ;
  var exp_val = [21, 22, 23, 21, 22, 23] ;
  T(curr.length == 6);
  for( var i = 0 ; i < 6 ; i++)
  {
      T(curr[i].key == exp_key[i]);
      T(curr[i].value == exp_val[i]);
  }

  curr = db.view("test/multi_emit", {startkey_docid: 21, endkey_docid: 23, keys: [0, 2]}, null).rows;
  T(curr.length == 6);
  for( var i = 0 ; i < 6 ; i++)
  {
      T(curr[i].key == exp_key[i]);
      T(curr[i].value == exp_val[i]);
  }

  // Check limit works
  curr = db.view("test/all_docs", {limit: 1}, keys).rows;
  T(curr.length == 1);
  T(curr[0].key == 10);

  curr = db.view("test/all_docs", {limit: 1, keys: keys}, null).rows;
  T(curr.length == 1);
  T(curr[0].key == 10);

  // Check offset works
  curr = db.view("test/multi_emit", {skip: 1}, [0]).rows;
  T(curr.length == 99);
  T(curr[0].value == 1);

  curr = db.view("test/multi_emit", {skip: 1, keys: [0]}, null).rows;
  T(curr.length == 99);
  T(curr[0].value == 1);

  // Check that dir works
  curr = db.view("test/multi_emit", {descending: "true"}, [1]).rows;
  T(curr.length == 100);
  T(curr[0].value == 99);
  T(curr[99].value == 0);

  curr = db.view("test/multi_emit", {descending: "true", keys: [1]}, null).rows;
  T(curr.length == 100);
  T(curr[0].value == 99);
  T(curr[99].value == 0);

  // Check a couple combinations
  curr = db.view("test/multi_emit", {descending: "true", skip: 3, limit: 2}, [2]).rows;
  T(curr.length, 2);
  T(curr[0].value == 96);
  T(curr[1].value == 95);

  curr = db.view("test/multi_emit", {descending: "true", skip: 3, limit: 2, keys: [2]}, null).rows;
  T(curr.length, 2);
  T(curr[0].value == 96);
  T(curr[1].value == 95);

  curr = db.view("test/multi_emit", {skip: 2, limit: 3, startkey_docid: "13"}, [0]).rows;
  T(curr.length == 3);
  T(curr[0].value == 15);
  T(curr[1].value == 16);
  T(curr[2].value == 17);

  curr = db.view("test/multi_emit", {skip: 2, limit: 3, startkey_docid: "13", keys: [0]}, null).rows;
  T(curr.length == 3);
  T(curr[0].value == 15);
  T(curr[1].value == 16);
  T(curr[2].value == 17);

  curr = db.view("test/multi_emit",
          {skip: 1, limit: 5, startkey_docid: "25", endkey_docid: "27"}, [1]).rows;
  T(curr.length == 2);
  T(curr[0].value == 26);
  T(curr[1].value == 27);

  curr = db.view("test/multi_emit",
          {skip: 1, limit: 5, startkey_docid: "25", endkey_docid: "27", keys: [1]}, null).rows;
  T(curr.length == 2);
  T(curr[0].value == 26);
  T(curr[1].value == 27);

  curr = db.view("test/multi_emit",
          {skip: 1, limit: 5, startkey_docid: "28", endkey_docid: "26", descending: "true"}, [1]).rows;
  T(curr.length == 2);
  T(curr[0].value == 27);
  T(curr[1].value == 26);

  curr = db.view("test/multi_emit",
          {skip: 1, limit: 5, startkey_docid: "28", endkey_docid: "26", descending: "true", keys: [1]}, null).rows;
  T(curr.length == 2);
  T(curr[0].value == 27);
  T(curr[1].value == 26);
};
