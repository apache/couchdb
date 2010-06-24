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

couchTests.batch_save = function(debug) {
  var db = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"false"});
  db.deleteDb();
  db.createDb();
  if (debug) debugger;

  var i
  for(i=0; i < 100; i++) {
    T(db.save({_id:i.toString(),a:i,b:i},  {batch : "ok"}).ok);
    
    // test that response is 202 Accepted
    T(db.last_req.status == 202);
  }
  
  for(i=0; i < 100; i++) {
    // attempt to save the same document a bunch of times
    T(db.save({_id:"foo",a:i,b:i},  {batch : "ok"}).ok);
    
    // test that response is 202 Accepted
    T(db.last_req.status == 202);
  }
  
  while(db.allDocs().total_rows != 101){};

  // repeat the tests for POST
  for(i=0; i < 100; i++) {
    var resp = db.request("POST", db.uri + "?batch=ok", {
      headers: {"Content-Type": "application/json"},
      body: JSON.stringify({a:1})
    });
    T(JSON.parse(resp.responseText).ok);
  }
  
  while(db.allDocs().total_rows != 201){};

};
