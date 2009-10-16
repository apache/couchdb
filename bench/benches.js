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


var allBenches = {};

allBenches.single_doc_insert = function(db, acc) {
  db.save({"foo":"bar"});
  acc.docs++;
  return acc;
};

allBenches.batch_ok_doc_insert = function(db, acc) {
  db.save({"foo":"bar"}, {"batch":"ok"});
  acc.docs++;
  return acc;  
};

function makeDocs(n) {
  docs = [];
  for (var i=0; i < n; i++) {
    docs.push({"foo":"bar"});
  };
  return docs;
};

allBenches.bulk_doc_100 = function(db, acc) {
  var docs = makeDocs(100);
  db.bulkSave(docs);
  acc.docs += 100;
  return acc;  
};

allBenches.bulk_doc_1000 = function(db, acc) {
  var docs = makeDocs(1000);
  db.bulkSave(docs);
  acc.docs += 1000;
  return acc;  
};

allBenches.bulk_doc_5000 = function(db, acc) {
  var docs = makeDocs(5000);
  db.bulkSave(docs);
  acc.docs += 5000;
  return acc;  
};

allBenches.bulk_doc_10000 = function(db, acc) {
  var docs = makeDocs(10000);
  db.bulkSave(docs);
  acc.docs += 10000;
  return acc;  
};