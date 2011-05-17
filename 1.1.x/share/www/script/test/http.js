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

couchTests.http = function(debug) {
  var db = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"false"});
  db.deleteDb();

  // bug COUCHDB-100: DELETE on non-existent DB returns 500 instead of 404
  db.deleteDb();

  db.createDb();

  // PUT on existing DB should return 412 instead of 500
  if (debug) debugger;

  var xhr = CouchDB.request("PUT", "/test_suite_db/test", {body: "{}"});
  var host = CouchDB.host;

  TEquals(CouchDB.protocol + host + "/test_suite_db/test", 
    xhr.getResponseHeader("Location"),
    "should include ip address");

  xhr = CouchDB.request("PUT", "/test_suite_db/test2", {
    body: "{}",
    headers: {"X-Forwarded-Host": "mysite.com"}
  });

  TEquals(CouchDB.protocol + "mysite.com/test_suite_db/test2",
    xhr.getResponseHeader("Location"),
    "should include X-Forwarded-Host");

  run_on_modified_server([{
    section:"httpd",
    key:"x_forwarded_host",
    value:"X-Host"}],
    function() {
      xhr = CouchDB.request("PUT", "/test_suite_db/test3", {
        body: "{}",
        headers: {"X-Host": "mysite2.com"}
      });
      TEquals(CouchDB.protocol + "mysite2.com/test_suite_db/test3",
        xhr.getResponseHeader("Location"),
        "should include X-Host");
    });
}
