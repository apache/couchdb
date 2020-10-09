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
couchTests.attachments_delete_overridden_quorum= function(debug) {
  return console.log('done in test/elixir/test/cluster_without_quorum_test.exs');
  var db_name = get_random_db_name();
  var db = new CouchDB(db_name, {"X-Couch-Full-Commit":"false"},{"w":1});
  db.createDb();
  if (debug) debugger;

  var doc = db.save({_id:"dummy"});
  T(doc.ok);
  var xhr = CouchDB.request("PUT", "/" + db_name + "/dummy/foo.txt?rev=" + doc.rev, {
    body:"This is no base64 encoded text",
    headers:{"Content-Type": "text/plain;charset=utf-8"}
  });
  var rev = JSON.parse(xhr.responseText).rev;

  xhr = CouchDB.request("DELETE", "/" + db_name + "/dummy/foo.txt?rev=" + rev);
  console.log("TODO: Clarify correct behaviour. Is not considering quorum. 202->"+xhr.status);
  //TODO: Define correct behaviour
  //T(xhr.status == 200,"Should return 200 but returns "+xhr.status);

 //db.deleteDb();
 // cleanup
 // TODO DB deletions fails if the quorum is not met.
 xhr = CouchDB.request("DELETE", "/" + db_name + "/");
}
