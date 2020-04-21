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
couchTests.elixir = true;
couchTests.http = function(debug) {
  return console.log('done in test/elixir/test/http_test.exs');
  var db_name = get_random_db_name();
  var db = new CouchDB(db_name, {"X-Couch-Full-Commit":"false"});

  // bug COUCHDB-100: DELETE on non-existent DB returns 500 instead of 404

  db.createDb();

  // PUT on existing DB should return 412 instead of 500
  if (debug) debugger;

  var xhr = CouchDB.request("PUT", "/" + db_name + "/test", {body: "{}"});
  var host = CouchDB.host;

  TEquals(CouchDB.protocol + host + "/" + db_name + "/test", 
    xhr.getResponseHeader("Location"),
    "should include ip address");

  xhr = CouchDB.request("PUT", "/" + db_name + "/test2", {
    body: "{}",
    headers: {"X-Forwarded-Host": "mysite.com"}
  });

  TEquals(CouchDB.protocol + "mysite.com/" + db_name + "/test2",
    xhr.getResponseHeader("Location"),
    "should include X-Forwarded-Host");

  run_on_modified_server([{
    section:"httpd",
    key:"x_forwarded_host",
    value:"X-Host"}],
    function() {
      xhr = CouchDB.request("PUT", "/" + db_name + "/test3", {
        body: "{}",
        headers: {"X-Host": "mysite2.com"}
      });
      TEquals(CouchDB.protocol + "mysite2.com/" + db_name + "/test3",
        xhr.getResponseHeader("Location"),
        "should include X-Host");
    });

  // COUCHDB-708: newlines document names
  xhr = CouchDB.request("PUT", "/" + db_name + "/docid%0A/attachment.txt", {
    headers: {"Content-Type": "text/plain;charset=utf-8"},
    body: ""
  });
  TEquals(CouchDB.protocol + host + "/" + db_name + "/docid%0A/attachment.txt",
    xhr.getResponseHeader("Location"),
    "should work with newlines in document names for attachments");

  xhr = CouchDB.request("PUT", "/" + db_name + "/docidtest%0A", {
    body: JSON.stringify({"foo": "bar"}),
    headers: {"Content-Type": "application/json"}
  });
  TEquals(CouchDB.protocol + host + "/" + db_name + "/docidtest%0A",
    xhr.getResponseHeader("Location"),
    "should work with newlines in document names");

  xhr = CouchDB.request("POST", "/" + db_name + "/", {
    body: JSON.stringify({"_id": "docidtestpost%0A"}),
    headers: {"Content-Type": "application/json"}
  });
  TEquals(CouchDB.protocol + host + "/" + db_name + "/docidtestpost%250A",
    xhr.getResponseHeader("Location"),
    "should work with newlines in document names");

    // cleanup
    db.deleteDb();
}
