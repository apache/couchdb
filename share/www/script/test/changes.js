// Licensed under the Apache License, Version 2.0 (the "License"); you may not
// use this file except in compliance with the License.  You may obtain a copy
// of the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
// WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
// License for the specific language governing permissions and limitations under
// the License.

couchTests.changes = function(debug) {
  var db = new CouchDB("test_suite_db");
  db.deleteDb();
  db.createDb();
  if (debug) debugger;
  
  
  var req = CouchDB.newXhr();
  
  req.open("GET", "/test_suite_db/_changes", false);
  req.send("");
  var resp = JSON.parse(req.responseText);
  
  T(resp.results.length == 0 && resp.last_seq==0)
  
  var doc = {_id:"foo", bar:1};
  db.save(doc);
  
  req.open("GET", "/test_suite_db/_changes", false);
  req.send("");
  var resp = JSON.parse(req.responseText);
  
  T(resp.results.length == 1 && resp.last_seq==1)
  T(resp.results[0].changes[0].rev == doc._rev)
  
};
