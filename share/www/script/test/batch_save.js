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

couchTests.batch_save = function(debug) {
  var db = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"false"});
  db.deleteDb();
  db.createDb();
  if (debug) debugger;

  // commit should work fine with no batches
  T(db.ensureFullCommit().ok);

  // PUT a doc with ?batch=ok
  T(db.save({_id:"0",a:1,b:1},  {batch : "ok"}).ok);

  // test that response is 202 Accepted
  T(db.last_req.status == 202);

  T(db.allDocs().total_rows == 0);

  restartServer();

  // lost the updates
  T(db.allDocs().total_rows == 0);

  T(db.save({_id:"0",a:1,b:1},  {batch : "ok"}).ok);
  T(db.save({_id:"1",a:1,b:1},  {batch : "ok"}).ok);
  T(db.save({_id:"2",a:1,b:1},  {batch : "ok"}).ok);

  T(db.ensureFullCommit().ok);
  T(db.allDocs().total_rows == 3);

  // repeat the tests for POST
  var resp = db.request("POST", db.uri + "?batch=ok", {body: JSON.stringify({a:1})});
  T(JSON.parse(resp.responseText).ok);

  // test that response is 202 Accepted
  T(resp.status == 202);

  T(db.allDocs().total_rows == 3);
  // restartServer();
  // // lost the POSTed doc
  // T(db.allDocs().total_rows == 3);

  var resp = db.request("POST", db.uri + "?batch=ok", {body: JSON.stringify({a:1})});
  T(JSON.parse(resp.responseText).ok);

  T(db.ensureFullCommit().ok);
  T(db.allDocs().total_rows == 5);

};
