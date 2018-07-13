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

// Do DB creation under cluster without quorum conditions.
couchTests.db_creation = function(debug) {

  if (debug) debugger;

  var db_name = get_random_db_name()
  var db = new CouchDB(db_name, {"X-Couch-Full-Commit":"false"});

  // DB Creation should return 202- Accepted
  xhr = CouchDB.request("PUT", "/" + db_name + "/");
  T(xhr.status == 202);

  // cleanup
  db.deleteDb();
};
