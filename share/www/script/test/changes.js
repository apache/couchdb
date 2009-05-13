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
  
  var req = CouchDB.request("GET", "/test_suite_db/_changes");
  var resp = JSON.parse(req.responseText);
  
  T(resp.results.length == 0 && resp.last_seq==0)
  
  var docFoo = {_id:"foo", bar:1};
  db.save(docFoo);
  
  req = CouchDB.request("GET", "/test_suite_db/_changes");
  var resp = JSON.parse(req.responseText);
  
  T(resp.results.length == 1 && resp.last_seq==1)
  T(resp.results[0].changes[0].rev == docFoo._rev)
  
  var xhr;
  
  try {
    xhr = CouchDB.newXhr();
  } catch (err) {  
  }
  
  if (xhr) {
    // Only test the continuous stuff if we have a real XHR object
    // with real async support.
    
    var sleep = function(msecs) {
      // by making a slow sync request, weallows the waiting XHR request data
      // to be received.
      var req = CouchDB.request("GET", "/_sleep?time=" + msecs);
      T(JSON.parse(req.responseText).ok == true);
    }
  
    var parse_changes_line = function(line) {
      if (line.charAt(line.length-1) == ",") {
        line = line.substring(0, line.length-1);
      }
      return JSON.parse(line);
    }
  
    xhr.open("GET", "/test_suite_db/_changes?continuous=true", true);
    xhr.send("");
  
    var docBar = {_id:"bar", bar:1};
    db.save(docBar);
  
    sleep(100);
    var lines = xhr.responseText.split("\n");
  
    T(lines[0]='{"results":[');
  
    var change = parse_changes_line(lines[1]);
  
    T(change.seq == 1)
    T(change.id == "foo")
  
    change = parse_changes_line(lines[2]);
  
    T(change.seq == 2)
    T(change.id == "bar")
    T(change.changes[0].rev == docBar._rev)
  
    var docBaz = {_id:"baz", baz:1};
    db.save(docBaz);
  
    sleep(100);
    var lines = xhr.responseText.split("\n");
  
    change = parse_changes_line(lines[3]);
  
    T(change.seq == 3);
    T(change.id == "baz");
    T(change.changes[0].rev == docBaz._rev);
  
  }
};
