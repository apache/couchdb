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

couchTests.uuids = function(debug) {
  var etags = [];
  var testHashBustingHeaders = function(xhr) {
    T(xhr.getResponseHeader("Cache-Control").match(/no-cache/));
    T(xhr.getResponseHeader("Pragma") == "no-cache");

    var newetag = xhr.getResponseHeader("ETag");
    T(etags.indexOf(newetag) < 0);
    etags[etags.length] = newetag;
    
    // Removing the time based tests as they break easily when
    // running CouchDB on a remote server in regards to the browser
    // running the Futon test suite.
    //
    //var currentTime = new Date();
    //var expiresHeader = Date.parse(xhr.getResponseHeader("Expires"));
    //var dateHeader = Date.parse(xhr.getResponseHeader("Date"));

    //T(expiresHeader < currentTime);
    //T(currentTime - dateHeader < 3000);
  };

  var db = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"false"});
  db.deleteDb();
  db.createDb();
  if (debug) debugger;

  // a single UUID without an explicit count
  var xhr = CouchDB.request("GET", "/_uuids");
  T(xhr.status == 200);
  var result = JSON.parse(xhr.responseText);
  T(result.uuids.length == 1);
  var first = result.uuids[0];
  testHashBustingHeaders(xhr);

  // a single UUID with an explicit count
  xhr = CouchDB.request("GET", "/_uuids?count=1");
  T(xhr.status == 200);
  result = JSON.parse(xhr.responseText);
  T(result.uuids.length == 1);
  var second = result.uuids[0];
  T(first != second);

  // no collisions with 1,000 UUIDs
  xhr = CouchDB.request("GET", "/_uuids?count=1000");
  T(xhr.status == 200);
  result = JSON.parse(xhr.responseText);
  T( result.uuids.length == 1000 );
  var seen = {};
  for(var i in result.uuids) {
    var id = result.uuids[i];
    T(seen[id] === undefined);
    seen[id] = 1;
  }

  // ensure we return a 405 on POST
  xhr = CouchDB.request("POST", "/_uuids?count=1000");
  T(xhr.status == 405);

  // Test sequential uuids
  var seq_testfun = function() {
    xhr = CouchDB.request("GET", "/_uuids?count=1000");
    T(xhr.status == 200);
    result = JSON.parse(xhr.responseText);
    for(var i = 1; i < result.uuids.length; i++) {
      T(result.uuids[i].length == 32);
      T(result.uuids[i-1] < result.uuids[i], "Sequential uuids are ordered.");
    }
  };

  run_on_modified_server([{
      "section": "uuids",
      "key": "algorithm",
      "value": "sequential",
    }],
    seq_testfun
  );

  // Test utc_random uuids
  var utc_testfun = function() {
    xhr = CouchDB.request("GET", "/_uuids?count=1000");
    T(xhr.status == 200);
    result = JSON.parse(xhr.responseText);
    T(result.uuids[1].length == 32);

    // no collisions
    var seen = {};
    for(var i in result.uuids) {
      var id = result.uuids[i];
      T(seen[id] === undefined);
      seen[id] = 1;
    }

    // roughly ordered
    var u1 = result.uuids[1].substr(0, 13);
    var u2 = result.uuids[result.uuids.length-1].substr(0, 13);
    T(u1 < u2, "UTC uuids are only roughly ordered, so this assertion may fail occasionally. Don't sweat it.");
  };

  run_on_modified_server([{
      "section": "uuids",
      "key": "algorithm",
      "value": "utc_random"
    }],
    utc_testfun
  );

};
