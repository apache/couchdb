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

function jsonp(obj) {
  T(jsonp_flag == 0);
  T(obj.results.length == 1 && obj.last_seq==1)
  jsonp_flag = 1;
}

couchTests.changes = function(debug) {
  var db = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"false"});
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

  // test with callback
  var xhr = CouchDB.request("GET", "/test_suite_db/_changes?callback=jsonp");
  T(xhr.status == 200);
  jsonp_flag = 0;
  eval(xhr.responseText);
  T(jsonp_flag == 1);


  req = CouchDB.request("GET", "/test_suite_db/_changes?feed=continuous&timeout=10");
  var lines = req.responseText.split("\n");
  T(JSON.parse(lines[0]).changes[0].rev == docFoo._rev);
  T(JSON.parse(lines[1]).last_seq == 1);

  var xhr;

  try {
    xhr = CouchDB.newXhr();
  } catch (err) {
  }

  // poor man's browser detection
  var is_safari = navigator.userAgent.match(/AppleWebKit/);
  if (!is_safari && xhr) {
    // Only test the continuous stuff if we have a real XHR object
    // with real async support.

    // WebKit (last checked on nightly #47686) does fail on processing
    // the async-request properly while javascript is executed.

    var sleep = function(msecs) {
      // by making a slow sync request, we allow the waiting XHR request data
      // to be received.
      var req = CouchDB.request("GET", "/_sleep?time=" + msecs);
      T(JSON.parse(req.responseText).ok == true);
    }

    xhr.open("GET", "/test_suite_db/_changes?feed=continuous", true);
    xhr.send("");

    var docBar = {_id:"bar", bar:1};
    db.save(docBar);

    sleep(100);
    var lines = xhr.responseText.split("\n");
  
    var change = JSON.parse(lines[0]);

    T(change.seq == 1)
    T(change.id == "foo")

    change = JSON.parse(lines[1]);

    T(change.seq == 2)
    T(change.id == "bar")
    T(change.changes[0].rev == docBar._rev)

    var docBaz = {_id:"baz", baz:1};
    db.save(docBaz);

    sleep(100);
    var lines = xhr.responseText.split("\n");

    change = JSON.parse(lines[2]);

    T(change.seq == 3);
    T(change.id == "baz");
    T(change.changes[0].rev == docBaz._rev);


    xhr = CouchDB.newXhr();

    //verify the hearbeat newlines are sent
    xhr.open("GET", "/test_suite_db/_changes?feed=continuous&heartbeat=10", true);
    xhr.send("");

    sleep(100);

    var str = xhr.responseText;

    T(str.charAt(str.length - 1) == "\n")
    T(str.charAt(str.length - 2) == "\n")


    // test longpolling
    xhr = CouchDB.newXhr();

    xhr.open("GET", "/test_suite_db/_changes?feed=longpoll", true);
    xhr.send("");

    sleep(100);
    var lines = xhr.responseText.split("\n");
    T(lines[5]=='"last_seq":3}');

    xhr = CouchDB.newXhr();

    xhr.open("GET", "/test_suite_db/_changes?feed=longpoll&since=3", true);
    xhr.send("");

    sleep(100);

    var docBarz = {_id:"barz", bar:1};
    db.save(docBarz);

    sleep(100);

    var lines = xhr.responseText.split("\n");

    var parse_changes_line = function(line) {
      if (line.charAt(line.length-1) == ",") {
        line = line.substring(0, line.length-1);
      }
      return JSON.parse(line);
    }

    change = parse_changes_line(lines[1]);

    T(change.seq == 4);
    T(change.id == "barz");
    T(change.changes[0].rev == docBarz._rev);
    T(lines[3]=='"last_seq":4}');
	
  }
  
  // test the filtered changes
  var ddoc = {
    _id : "_design/changes_filter",
    "filters" : {
      "bop" : "function(doc, req) { return (doc.bop);}",
      "dynamic" : stringFun(function(doc, req) { 
        var field = req.query.field;
        return doc[field];
      }),
      "userCtx" : stringFun(function(doc, req) {
        return doc.user && (doc.user == req.userCtx.name);
      })
    }
  }

  db.save(ddoc);

  var req = CouchDB.request("GET", "/test_suite_db/_changes?filter=changes_filter/bop");
  var resp = JSON.parse(req.responseText);
  T(resp.results.length == 0); 

  db.save({"bop" : "foom"});
  
  var req = CouchDB.request("GET", "/test_suite_db/_changes?filter=changes_filter/bop");
  var resp = JSON.parse(req.responseText);
  T(resp.results.length == 1);
    
  req = CouchDB.request("GET", "/test_suite_db/_changes?filter=changes_filter/dynamic&field=woox");
  resp = JSON.parse(req.responseText);
  T(resp.results.length == 0);
  
  req = CouchDB.request("GET", "/test_suite_db/_changes?filter=changes_filter/dynamic&field=bop");
  resp = JSON.parse(req.responseText);
  T(resp.results.length == 1);

  // error conditions

  // non-existing design doc
  var req = CouchDB.request("GET", 
    "/test_suite_db/_changes?filter=nothingtosee/bop");
  TEquals(400, req.status, "should return 400 for non existant design doc");

  // non-existing filter 
  var req = CouchDB.request("GET", 
    "/test_suite_db/_changes?filter=changes_filter/movealong");
  TEquals(400, req.status, "should return 400 for non existant filter fun");

  // both
  var req = CouchDB.request("GET", 
    "/test_suite_db/_changes?filter=nothingtosee/movealong");
  TEquals(400, req.status, 
    "should return 400 for non existant design doc and filter fun");

  // changes get all_docs style with deleted docs
  var doc = {a:1};
  db.save(doc);
  db.deleteDoc(doc);
  var req = CouchDB.request("GET", 
    "/test_suite_db/_changes?filter=changes_filter/bop&style=all_docs");
  var resp = JSON.parse(req.responseText);
  TEquals(1, resp.results.length, "should return one result row");
  
  // test for userCtx
  run_on_modified_server(
    [{section: "httpd",
      key: "authentication_handlers",
      value: "{couch_httpd_auth, special_test_authentication_handler}"},
     {section:"httpd",
      key: "WWW-Authenticate",
      value:  "X-Couch-Test-Auth"}],

    function() {
      var authOpts = {"headers":{"WWW-Authenticate": "X-Couch-Test-Auth Chris Anderson:mp3"}};
      
      T(db.save({"user" : "Noah Slater"}).ok);
      var req = CouchDB.request("GET", "/test_suite_db/_changes?filter=changes_filter/userCtx", authOpts);
      var resp = JSON.parse(req.responseText);
      T(resp.results.length == 0);

      var docResp = db.save({"user" : "Chris Anderson"});
      T(docResp.ok);
      req = CouchDB.request("GET", "/test_suite_db/_changes?filter=changes_filter/userCtx", authOpts);
      resp = JSON.parse(req.responseText);
      T(resp.results.length == 1);
      T(resp.results[0].id == docResp.id);
    });
  
  // todo implement adhoc filters...
};
