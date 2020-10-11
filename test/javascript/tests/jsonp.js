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
couchTests.elixir = true;

// Verify callbacks ran
var jsonp_flag = 0;

// Callbacks
function jsonp_no_chunk(doc) {
  T(jsonp_flag == 0);
  T(doc._id == "0");
  jsonp_flag = 1;
}

function jsonp_chunk(doc) {
  T(jsonp_flag == 0);
  T(doc.total_rows == 1);
  jsonp_flag = 1;
}

// Do some jsonp tests.
couchTests.jsonp = function(debug) {
  return console.log('done in test/elixir/test/jsonp_test.exs');
  var db_name = get_random_db_name();
  var db = new CouchDB(db_name, {"X-Couch-Full-Commit":"false"});
  db.createDb();
  if (debug) debugger;
  
  var doc = {_id:"0",a:0,b:0};
  T(db.save(doc).ok);
  
  // callback param is ignored unless jsonp is configured
  var xhr = CouchDB.request("GET", "/" + db_name + "/0?callback=jsonp_not_configured");
  JSON.parse(xhr.responseText);

  run_on_modified_server(
    [{section: "httpd",
      key: "allow_jsonp",
      value: "true"}],
  function() {

    // Test unchunked callbacks.
    var xhr = CouchDB.request("GET", "/" + db_name + "/0?callback=jsonp_no_chunk");
    TEquals("application/javascript", xhr.getResponseHeader("Content-Type"));
    T(xhr.status == 200);
    jsonp_flag = 0;
    eval(xhr.responseText);
    T(jsonp_flag == 1);
    xhr = CouchDB.request("GET", "/" + db_name + "/0?callback=foo\"");
    T(xhr.status == 400);

    // Test chunked responses
    var doc = {_id:"1",a:1,b:1};
    T(db.save(doc).ok);

    var designDoc = {
      _id:"_design/test",
      language: "javascript",
      views: {
        all_docs: {map: "function(doc) {if(doc.a) emit(null, doc.a);}"}
      }
    };
    T(db.save(designDoc).ok);

    var url = "/" + db_name + "/_design/test/_view/all_docs?callback=jsonp_chunk";
    xhr = CouchDB.request("GET", url);
    TEquals("application/javascript", xhr.getResponseHeader("Content-Type"));
    T(xhr.status == 200);
    jsonp_flag = 0;
    eval(xhr.responseText);
    T(jsonp_flag == 1);
    xhr = CouchDB.request("GET", url + "\'");
    T(xhr.status == 400);
  });

  // cleanup
  db.deleteDb();
};
