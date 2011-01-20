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

// Allow broken HTTP clients to fake a full method vocabulary with an X-HTTP-METHOD-OVERRIDE header
couchTests.method_override = function(debug) {
  var result = JSON.parse(CouchDB.request("GET", "/").responseText);
  T(result.couchdb == "Welcome");

  var db = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"false"});
  db.deleteDb();

  db.createDb();

  var doc = {bob : "connie"};
  xhr = CouchDB.request("POST", "/test_suite_db/fnord", {body: JSON.stringify(doc), headers:{"X-HTTP-Method-Override" : "PUT"}});
  T(xhr.status == 201);

  doc = db.open("fnord");
  T(doc.bob == "connie");

  xhr = CouchDB.request("POST", "/test_suite_db/fnord?rev=" + doc._rev, {headers:{"X-HTTP-Method-Override" : "DELETE"}});
  T(xhr.status == 200);

  xhr = CouchDB.request("GET", "/test_suite_db/fnord2", {body: JSON.stringify(doc), headers:{"X-HTTP-Method-Override" : "PUT"}});
  // Method Override is ignored when original Method isn't POST
  T(xhr.status == 404);

  doc = db.open("fnord");
  T(doc == null);  

};
