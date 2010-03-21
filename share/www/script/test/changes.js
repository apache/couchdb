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
  T(obj.results.length == 1 && obj.last_seq==1, "jsonp")
  jsonp_flag = 1;
}

couchTests.changes = function(debug) {
  var db = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"false"});
  db.deleteDb();
  db.createDb();
  if (debug) debugger;

  var req = CouchDB.request("GET", "/test_suite_db/_changes");
  var resp = JSON.parse(req.responseText);

  T(resp.results.length == 0 && resp.last_seq==0, "empty db")
  var docFoo = {_id:"foo", bar:1};
  T(db.save(docFoo).ok);
  T(db.ensureFullCommit().ok);
  
  req = CouchDB.request("GET", "/test_suite_db/_changes");
  var resp = JSON.parse(req.responseText);

  T(resp.results.length == 1 && resp.last_seq==1, "one doc db")
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
  var is_safari = false;
  if(typeof(navigator) == "undefined") {
    is_safari = true; // For CouchHTTP based runners
  } else if(navigator.userAgent.match(/AppleWebKit/)) {
    is_safari = true;
  };
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
      }),
      "conflicted" : "function(doc, req) { return (doc._conflicts);}",
    }
  }

  db.save(ddoc);

  var req = CouchDB.request("GET", "/test_suite_db/_changes?filter=changes_filter/bop");
  var resp = JSON.parse(req.responseText);
  T(resp.results.length == 0); 

  db.save({"bop" : "foom"});
  db.save({"bop" : false});
  
  var req = CouchDB.request("GET", "/test_suite_db/_changes?filter=changes_filter/bop");
  var resp = JSON.parse(req.responseText);
  T(resp.results.length == 1);
    
  req = CouchDB.request("GET", "/test_suite_db/_changes?filter=changes_filter/dynamic&field=woox");
  resp = JSON.parse(req.responseText);
  T(resp.results.length == 0);
  
  req = CouchDB.request("GET", "/test_suite_db/_changes?filter=changes_filter/dynamic&field=bop");
  resp = JSON.parse(req.responseText);
  T(resp.results.length == 1);

  if (!is_safari && xhr) { // full test requires parallel connections
    // filter with longpoll
    // longpoll filters full history when run without a since seq
    xhr = CouchDB.newXhr();
    xhr.open("GET", "/test_suite_db/_changes?feed=longpoll&filter=changes_filter/bop", true);
    xhr.send("");
    sleep(100);
    var resp = JSON.parse(xhr.responseText);
    T(resp.last_seq == 7);
    // longpoll waits until a matching change before returning
    xhr = CouchDB.newXhr();
    xhr.open("GET", "/test_suite_db/_changes?feed=longpoll&since=7&filter=changes_filter/bop", true);
    xhr.send("");
    db.save({"_id":"falsy", "bop" : ""}); // empty string is falsy
    db.save({"_id":"bingo","bop" : "bingo"});
    sleep(100);
    var resp = JSON.parse(xhr.responseText);
    T(resp.last_seq == 9);
    T(resp.results && resp.results.length > 0 && resp.results[0]["id"] == "bingo", "filter the correct update");

    // filter with continuous
    xhr = CouchDB.newXhr();
    xhr.open("GET", "/test_suite_db/_changes?feed=continuous&filter=changes_filter/bop&timeout=200", true);
    xhr.send("");
    db.save({"_id":"rusty", "bop" : "plankton"});
    T(db.ensureFullCommit().ok);
    sleep(300);
    var lines = xhr.responseText.split("\n");
    T(JSON.parse(lines[1]).id == "bingo", lines[1]);
    T(JSON.parse(lines[2]).id == "rusty", lines[2]);
    T(JSON.parse(lines[3]).last_seq == 10, lines[3]);
  }
  // error conditions

  // non-existing design doc
  var req = CouchDB.request("GET", 
    "/test_suite_db/_changes?filter=nothingtosee/bop");
  TEquals(404, req.status, "should return 404 for non existant design doc");

  // non-existing filter 
  var req = CouchDB.request("GET", 
    "/test_suite_db/_changes?filter=changes_filter/movealong");
  TEquals(404, req.status, "should return 404 for non existant filter fun");

  // both
  var req = CouchDB.request("GET", 
    "/test_suite_db/_changes?filter=nothingtosee/movealong");
  TEquals(404, req.status, 
    "should return 404 for non existant design doc and filter fun");

  // changes get all_docs style with deleted docs
  var doc = {a:1};
  db.save(doc);
  db.deleteDoc(doc);
  var req = CouchDB.request("GET", 
    "/test_suite_db/_changes?filter=changes_filter/bop&style=all_docs");
  var resp = JSON.parse(req.responseText);
  var expect = (!is_safari && xhr) ? 3: 1;
  TEquals(expect, resp.results.length, "should return matching rows");
  
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

      var req = CouchDB.request("GET", "/_session", authOpts);
      var resp = JSON.parse(req.responseText);
      
      T(db.save({"user" : "Noah Slater"}).ok);
      var req = CouchDB.request("GET", "/test_suite_db/_changes?filter=changes_filter/userCtx", authOpts);
      var resp = JSON.parse(req.responseText);
      T(resp.results.length == 0);

      var docResp = db.save({"user" : "Chris Anderson"});
      T(docResp.ok);
      T(db.ensureFullCommit().ok);
      req = CouchDB.request("GET", "/test_suite_db/_changes?filter=changes_filter/userCtx", authOpts);
      resp = JSON.parse(req.responseText);
      T(resp.results.length == 1, "userCtx");
      T(resp.results[0].id == docResp.id);
    }
  );

  req = CouchDB.request("GET", "/test_suite_db/_changes?limit=1");
  resp = JSON.parse(req.responseText);
  TEquals(1, resp.results.length)

  //filter includes _conflicts
  var id = db.save({'food' : 'pizza'}).id;
  db.bulkSave([{_id: id, 'food' : 'pasta'}], {all_or_nothing:true});

  req = CouchDB.request("GET", "/test_suite_db/_changes?filter=changes_filter/conflicted");
  resp = JSON.parse(req.responseText);
  T(resp.results.length == 1);
};

