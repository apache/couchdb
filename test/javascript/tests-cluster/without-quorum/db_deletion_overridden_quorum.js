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

// Do DB deletion in a cluster with quorum conditions.
couchTests.db_deletion_overridden_quorum = function(debug) {

  if (debug) debugger;

  var db_name = get_random_db_name()
  var db = new CouchDB(db_name, {"X-Couch-Full-Commit":"false"},{"w":1});
  db.createDb();


  //db.deleteDb();
  // TODO DB deletions fails if the quorum is not met.
  xhr = CouchDB.request("DELETE", "/" + db_name + "/");
  //T(db.last_req.status="200","Should return 200");
  console.log("Skipped-TODO: Fix issue 500 Error on delete - Not considering overriden quorum. 200->"+xhr.status);

};
