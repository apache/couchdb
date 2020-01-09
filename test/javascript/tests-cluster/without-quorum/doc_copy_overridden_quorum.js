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

couchTests.skip = true;
couchTests.doc_copy_overriden_quorum = function(debug) {
  return console.log('done in test/elixir/test/cluster_without_quorum_test.exs');
  var db_name = get_random_db_name();
  var db = new CouchDB(db_name, {"X-Couch-Full-Commit":"false"},{"w":1});
  db.createDb();
  if (debug) debugger;

  db.save({_id:"dummy"});

  var xhr = CouchDB.request("COPY", "/" + db_name + "/dummy", {
    headers: {"Destination":"dummy2"}
  });
  console.log("TODO: Clarify correct behaviour. Is not considering overridden quorum. 201->"+xhr.status);
  //TODO Defie correct behaviour
  //T(xhr.status=="201","Should return 201");

  //db.deleteDb();
  // cleanup
  // TODO DB deletions fails if the quorum is not met.
  xhr = CouchDB.request("DELETE", "/" + db_name + "/");

}
