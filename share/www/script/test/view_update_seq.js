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

couchTests.view_update_seq = function(debug) {
  var db = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"true"});
  db.deleteDb();
  db.createDb();
  if (debug) debugger;

  T(db.info().update_seq == 0);

  var resp = db.allDocs({update_seq:true});

  T(resp.rows.length == 0);
  T(resp.update_seq == 0);

  var designDoc = {
    _id:"_design/test",
    language: "javascript",
    views: {
      all_docs: {
        map: "function(doc) { emit(doc.integer, doc.string) }"
      },
      summate: {
        map:"function (doc) {emit(doc.integer, doc.integer)};",
        reduce:"function (keys, values) { return sum(values); };"
      }
    }
  };
  T(db.save(designDoc).ok);

  T(db.info().update_seq == 1);

  resp = db.allDocs({update_seq:true});

  T(resp.rows.length == 1);
  T(resp.update_seq == 1);

  var docs = makeDocs(0, 100);
  db.bulkSave(docs);

  resp = db.allDocs({limit: 1});
  T(resp.rows.length == 1);
  T(!resp.update_seq, "all docs");

  resp = db.allDocs({limit: 1, update_seq:true});
  T(resp.rows.length == 1);
  T(resp.update_seq == 101);

  resp = db.view('test/all_docs', {limit: 1, update_seq:true});
  T(resp.rows.length == 1);
  T(resp.update_seq == 101);

  resp = db.view('test/all_docs', {limit: 1, update_seq:false});
  T(resp.rows.length == 1);
  T(!resp.update_seq, "view");

  resp = db.view('test/summate', {update_seq:true});
  T(resp.rows.length == 1);
  T(resp.update_seq == 101);

  db.save({"id":"0"});
  resp = db.view('test/all_docs', {limit: 1,stale: "ok", update_seq:true});
  T(resp.rows.length == 1);
  T(resp.update_seq == 101);

  db.save({"id":"00"});
  resp = db.view('test/all_docs',
    {limit: 1, stale: "update_after", update_seq: true});
  T(resp.rows.length == 1);
  T(resp.update_seq == 101);

  // wait 5 seconds for the next assertions to pass in very slow machines
  var t0 = new Date(), t1;
  do {
    CouchDB.request("GET", "/");
    t1 = new Date();
  } while ((t1 - t0) < 5000);

  resp = db.view('test/all_docs', {limit: 1, stale: "ok", update_seq: true});
  T(resp.rows.length == 1);
  T(resp.update_seq == 103);

  resp = db.view('test/all_docs', {limit: 1, update_seq:true});
  T(resp.rows.length == 1);
  T(resp.update_seq == 103);

  resp = db.view('test/all_docs',{update_seq:true},["0","1"]);
  T(resp.update_seq == 103);

  resp = db.view('test/all_docs',{update_seq:true},["0","1"]);
  T(resp.update_seq == 103);

  resp = db.view('test/summate',{group:true, update_seq:true},["0","1"]);
  T(resp.update_seq == 103);

};
