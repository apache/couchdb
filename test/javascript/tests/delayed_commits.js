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

couchTests.delayed_commits = function(debug) {

  // Note that delayed_commits is deprecated in 2.0, so this is a minimal
  // test to show it still works. delayed_commits will be removed in 3.0.

  db_name = get_random_db_name();
  var db = new CouchDB(db_name, {"X-Couch-Full-Commit":"false"});
  db.deleteDb();
  db.createDb();
  if (debug) debugger;

  run_on_modified_server(
    [{section: "couchdb",
      key: "delayed_commits",
      value: "true"}],

    function () {
      // By default, couchdb doesn't fully commit documents to disk right away,
      // it waits about a second to batch the full commit flush along with any
      // other updates. If it crashes or is restarted you may lose the most
      // recent commits.

      T(db.save({_id:"1",a:2,b:4}).ok);
      T(db.open("1") != null);

      restartServer();

      T(db.open("1") == null); // lost the update.
      // note if we waited > 1 sec before the restart, the doc would likely
      // commit.
    });
};
