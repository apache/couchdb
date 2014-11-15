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

  // with no accept header
  var req = CouchDB.newXhr();
  req.open("GET", CouchDB.proxyUrl("/test_suite_db/"), false);
  req.send("");
  TEquals("text/plain; charset=utf-8", req.getResponseHeader("Content-Type"));

  // make sure JSON responses end in a newline
  var text = req.responseText;
  TEquals("\n", text[text.length-1]);

  xhr = CouchDB.request("GET", "/test_suite_db/", {
    headers: {"Accept": "text/html; text/plain;*/*"}
  });
  TEquals("text/plain; charset=utf-8", xhr.getResponseHeader("Content-Type"));

  xhr = CouchDB.request("GET", "/test_suite_db/", {
    headers: {"Accept": "application/json"}
  });
  TEquals("application/json", xhr.getResponseHeader("Content-Type"));
};
