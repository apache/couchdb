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

couchTests.csrf = function(debug) {
  var db = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"false"});
  db.deleteDb();
  db.createDb();

  if (debug) debugger;

  // Handy function to cause CouchDB to delete the CSRF cookie
  var deleteCsrf = function() {
    var xhr = CouchDB.request("POST", "/test_suite_db/_all_docs", {
                              body: '{"keys": []}',
                              headers: {'X-CouchDB-CSRF': 'foo', 'Cookie': 'CouchDB-CSRF=foo'}});
    TEquals(403, xhr.status);
  };

  // Shouldn't receive header if we didn't ask for it
  var xhr = CouchDB.request("GET", "/");
  TEquals(null, xhr.getResponseHeader("X-CouchDB-CSRF-Valid"), "Didn't ask for CSRF");
  TEquals(200, xhr.status);

  // Matching but invalid cookie/header should 403
  xhr = CouchDB.request("POST", "/test_suite_db/_all_docs", {
                        body: '{"keys": []}',
                        headers: {'X-CouchDB-CSRF': 'foo', 'Cookie': 'CouchDB-CSRF=foo'}});
  TEquals(403, xhr.status);
  TEquals(null, xhr.getResponseHeader("X-CouchDB-CSRF-Valid"), "We sent invalid cookie and header");

  // Can I acquire a CouchDB-CSRF cookie?
  xhr = CouchDB.request("GET", "/", {headers: {'X-CouchDB-CSRF': 'true'}});
  var cookie = xhr.getResponseHeader("Set-Cookie").match('^CouchDB-CSRF=([^;]+)');
  T(cookie, "Should receive cookie");

  // If I have a cookie, do I get a 403 if I don't send the header?
  xhr = CouchDB.request("POST", "/test_suite_db/_all_docs", {body: '{"keys": []}'});
  TEquals(403, xhr.status);
  TEquals(null, xhr.getResponseHeader("X-CouchDB-CSRF-Valid"), "We didn't send the header");

  // If I have a cookie, do I get a 200 if I send a matching header?
  xhr = CouchDB.request("POST", "/test_suite_db/_all_docs", {body: '{"keys": []}',
                                                             headers: {"X-CouchDB-CSRF": cookie[1]}});
  TEquals(200, xhr.status);
  TEquals("true", xhr.getResponseHeader("X-CouchDB-CSRF-Valid"), "Server should have sent this");

  // How about the wrong header?
  xhr = CouchDB.request("POST", "/test_suite_db/_all_docs", {body: '{"keys": []}',
                                                             headers: {'X-CouchDB-CSRF': 'foo'}});
  TEquals(403, xhr.status);
  TEquals(null, xhr.getResponseHeader("X-CouchDB-CSRF-Valid"), "We sent a mismatched header");

  deleteCsrf();
};
