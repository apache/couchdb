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

var NUM_DOCS = 2000;
var NUM_BATCHES = 20;

var init = function() {
  var db = new CouchDB("bench_mark_db", {"X-Couch-Full-Commit": "false"});
  db.deleteDb();
  db.createDb();
  return db;
};

var timeit = function(func) {
  var startTime = (new Date()).getTime();
  func();
  return ((new Date()).getTime() - startTime) / 1000;
};

var report = function(name, rate) {
  rate = Math.round(parseFloat(rate) * 100) / 100;
  console.log("" + name + ": " + rate + " docs/second");
};

var makeDocs = function(n) {
  docs = [];
  for (var i=0; i < n; i++) {
    docs.push({"foo":"bar"});
  };
  return docs;
};

var couchTests = {};

couchTests.single_doc_insert = function() {
  var db = init();
  var len = timeit(function() {
    for(var i = 0; i < NUM_DOCS; i++) {
      db.save({"foo": "bar"});
    }
  });
  report("Single doc inserts", NUM_DOCS/len);
};

couchTests.batch_ok_doc_insert = function() {
  var db = init();
  var len = timeit(function() {
    for(var i = 0; i < NUM_DOCS; i++) {
      db.save({"foo":"bar"}, {"batch":"ok"});
    }
  });
  report("Single doc inserts with batch=ok", NUM_DOCS/len);
};

couchTests.bulk_doc_100 = function() {
  var db = init();
  var len = timeit(function() {
    for(var i = 0; i < NUM_BATCHES; i++) {
      db.bulkSave(makeDocs(100));
    }
  });
  report("Bulk docs - 100", (NUM_BATCHES*100)/len);
};
      
couchTests.bulk_doc_1000 = function() {
  var db = init();
  var len = timeit(function() {
    for(var i = 0; i < NUM_BATCHES; i++) {
      db.bulkSave(makeDocs(1000));
    }
  });
  report("Bulk docs - 1000", (NUM_BATCHES*1000)/len);
};


couchTests.bulk_doc_5000 = function() {
  var db = init();
  var len = timeit(function() {
    for(var i = 0; i < NUM_BATCHES; i++) {
      db.bulkSave(makeDocs(5000));
    }
  });
  report("Bulk docs - 5000", (NUM_BATCHES*5000)/len);
};

couchTests.bulk_doc_10000 = function() {
  var db = init();
  var len = timeit(function() {
    for(var i = 0; i < NUM_BATCHES; i++) {
      db.bulkSave(makeDocs(10000));
    }
  });
  report("Bulk docs - 10000", (NUM_BATCHES*10000)/len);
};
