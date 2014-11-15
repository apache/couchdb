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

couchTests.design_paths = function(debug) {
  if (debug) debugger;
  var dbNames = ["test_suite_db", "test_suite_db/with_slashes"];
  for (var i=0; i < dbNames.length; i++) {
    var db = new CouchDB(dbNames[i]);
    var dbName = encodeURIComponent(dbNames[i]);
    db.deleteDb();
    db.createDb();

    // create a ddoc w bulk_docs
    db.bulkSave([{
      _id : "_design/test",
      views : {
        "testing" : {
          "map" : "function(){emit(1,1)}"
        }
      }
    }]);

    // ddoc is getable
    var xhr = CouchDB.request("GET", "/"+dbName+"/_design/test");
    var resp = JSON.parse(xhr.responseText);
    T(resp._id == "_design/test");

    // it's at 2 urls...
    var xhr = CouchDB.request("GET", "/"+dbName+"/_design%2Ftest");
    var resp = JSON.parse(xhr.responseText);
    T(resp._id == "_design/test");

    // ensure that views are addressable
    resp = db.view("test/testing")
    T(resp.total_rows == 0)

    // create a ddoc by putting to url with raw slash
    var xhr = CouchDB.request("PUT", "/"+dbName+"/_design/test2",{
      body : JSON.stringify({
        _id : "_design/test2",
        views : {
          "testing" : {
            "map" : "function(){emit(1,1)}"
          }
        }
      })
    });

    // ddoc is getable
    var xhr = CouchDB.request("GET", "/"+dbName+"/_design/test2");
    var resp = JSON.parse(xhr.responseText);
    T(resp._id == "_design/test2");

    // it's at 2 urls...
    var xhr = CouchDB.request("GET", "/"+dbName+"/_design%2Ftest2");
    var resp = JSON.parse(xhr.responseText);
    T(resp._id == "_design/test2");

    // ensure that views are addressable
    resp = db.view("test2/testing");
    T(resp.total_rows == 0);
  };
};
