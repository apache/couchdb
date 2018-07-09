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

couchTests.doc_crud_overridden_quorum = function(debug) {
  var db_name = get_random_db_name();
  var db = new CouchDB(db_name, {"X-Couch-Full-Commit":"false"},{"w":3});
  db.createDb();
  if (debug) debugger;

  db.save({_id:"0",a:1});
  T(db.last_req.status=="202","Should return 202 status");

  var doc = db.open("0");
  db.save(doc);
  T(db.last_req.status=="202","Should return 202 status");

  doc = db.open("0");
  db.deleteDoc(doc);
  T(db.last_req.status="202","Should return 202 status");

  db.deleteDb();
}
