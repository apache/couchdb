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

couchTests.content_negotiation = function(debug) {
  var db = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"false"});
  db.deleteDb();
  db.createDb();
  if (debug) debugger;
  var xhr;

  xhr = CouchDB.request("GET", "/test_suite_db/");
  T(xhr.getResponseHeader("Content-Type") == "text/plain;charset=utf-8");

  // make sure JSON responses end in a newline
  var text = xhr.responseText;
  T(text[text.length-1] == "\n");

  xhr = CouchDB.request("GET", "/test_suite_db/", {
    headers: {"Accept": "text/html;text/plain;*/*"}
  });
  T(xhr.getResponseHeader("Content-Type") == "text/plain;charset=utf-8");

  xhr = CouchDB.request("GET", "/test_suite_db/", {
    headers: {"Accept": "application/json"}
  });
  T(xhr.getResponseHeader("Content-Type") == "application/json");
};
