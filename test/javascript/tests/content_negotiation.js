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
  var db_name = get_random_db_name();
  var db = new CouchDB(db_name, {"X-Couch-Full-Commit":"false"});
  db.createDb();
  if (debug) debugger;
  var xhr;

  // with no accept header
  var req = CouchDB.newXhr();
  req.open("GET", CouchDB.proxyUrl("/" + db_name + "/"), false);
  req.send("");
  TEquals("text/plain; charset=utf-8", req.getResponseHeader("Content-Type"));

  // make sure JSON responses end in a newline
  var text = req.responseText;
  TEquals("\n", text[text.length-1]);

  xhr = CouchDB.request("GET", "/" + db_name + "/", {
    headers: {"Accept": "text/html; text/plain;*/*"}
  });
  TEquals("text/plain; charset=utf-8", xhr.getResponseHeader("Content-Type"));

  xhr = CouchDB.request("GET", "/" + db_name + "/", {
    headers: {"Accept": "application/json"}
  });
  TEquals("application/json", xhr.getResponseHeader("Content-Type"));

  // cleanup
  db.deleteDb();
};
