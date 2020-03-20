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
 
function jsonp(obj) {
  T(jsonp_flag == 0);
  T(obj.results.length == 1 && obj.last_seq == 1, "jsonp");
  jsonp_flag = 1;
}

couchTests.changes = function(debug) {
  return console.log('done in test/elixir/test/changes_test.exs and changes_async_test.exs');
  
  var db;
  if (debug) debugger;

  // poor man's browser detection
  var is_safari = false;
  if (typeof (navigator) == "undefined") {
    is_safari = true; // For CouchHTTP based runners
  } else if (navigator.userAgent.match(/AppleWebKit/)) {
    is_safari = true;
  }

  testChanges("live");
  testChanges("continuous");
  function testChanges(feed) {
    var db_name = get_random_db_name();
    // (write-quorums help keep a consistent feed)
    db = new CouchDB(db_name, {"X-Couch-Full-Commit":"true"}, {"w": 3});
    db.createDb();

    var req = CouchDB.request("GET", "/" + db_name + "/_changes");
    var resp = JSON.parse(req.responseText);

    TEquals(0, resp.results.length, "db must be empty")
    TEquals("0", resp.last_seq.substr(0, 1), "seq must start with 0")
    var docFoo = {_id:"foo", bar:1};
    T(db.save(docFoo).ok);
    T(db.ensureFullCommit().ok);
    T(db.open(docFoo._id)._id == docFoo._id);

    retry_part(function(){ // avoid Heisenbugs
      req = CouchDB.request("GET", "/" + db_name + "/_changes");
      var resp = JSON.parse(req.responseText);
      TEquals("1", resp.last_seq.substr(0, 1), "seq must start with 1");
      T(resp.results.length == 1, "one doc db");
      T(resp.results[0].changes[0].rev == docFoo._rev);
    });

    // test with callback
// TODO: either allow jsonp in the default global config or implement a config chg mechanism analogouts 2 sebastianrothbucher:clustertest - or leave out
//    run_on_modified_server(
//      [{section: "httpd",
//        key: "allow_jsonp",
//        value: "true"}],
//    function() {
//      var xhr = CouchDB.request("GET", "/" + db_name + "/_changes?callback=jsonp");
//      T(xhr.status == 200);
//      jsonp_flag = 0;
//      eval(xhr.responseText);
//      T(jsonp_flag == 1);
//    });

    // increase timeout to 100 to have enough time 2 assemble (seems like too little timeouts kill
    req = CouchDB.request("GET", "/" + db_name + "/_changes?feed=" + feed + "&timeout=100");
    var lines = req.responseText.split("\n");
    T(JSON.parse(lines[0]).changes[0].rev == docFoo._rev);
    // the sequence is not fully ordered and a complex structure now
    T(JSON.parse(lines[1]).last_seq[0] == 1);

    var xhr;

    try {
      xhr = CouchDB.newXhr();
    } catch (err) {
    }

    // these will NEVER run as we're always in navigator == undefined
    if (!is_safari && xhr) {
      // Only test the continuous stuff if we have a real XHR object
      // with real async support.

      // WebKit (last checked on nightly #47686) does fail on processing
      // the async-request properly while javascript is executed.

      xhr.open("GET", CouchDB.proxyUrl("/" + db_name + "/_changes?feed=" + feed + "&timeout=500"), true);
      xhr.send("");

      var docBar = {_id:"bar", bar:1};
      db.save(docBar);

      var lines, change1, change2;
      waitForSuccess(function() {
        lines = xhr.responseText.split("\n");
        change1 = JSON.parse(lines[0]);
        change2 = JSON.parse(lines[1]);
        if (change2.seq != 2) {
            throw "bad seq, try again";
        }
        return true;
      }, "bar-only");

      T(change1.seq == 1);
      T(change1.id == "foo");

      T(change2.seq == 2);
      T(change2.id == "bar");
      T(change2.changes[0].rev == docBar._rev);


      var docBaz = {_id:"baz", baz:1};
      db.save(docBaz);

      var change3;
      waitForSuccess(function() {
        lines = xhr.responseText.split("\n");
        change3 = JSON.parse(lines[2]);
        if (change3.seq != 3) {
          throw "bad seq, try again";
        }
        return true;
      });

      T(change3.seq == 3);
      T(change3.id == "baz");
      T(change3.changes[0].rev == docBaz._rev);


      xhr = CouchDB.newXhr();

      //verify the heartbeat newlines are sent
      xhr.open("GET", CouchDB.proxyUrl("/" + db_name + "/_changes?feed=" + feed + "&heartbeat=10&timeout=500"), true);
      xhr.send("");

      var str;
      waitForSuccess(function() {
        str = xhr.responseText;
        if (str.charAt(str.length - 1) != "\n" || str.charAt(str.length - 2) != "\n") {
          throw("keep waiting");
        }
        return true;
      }, "heartbeat");

      T(str.charAt(str.length - 1) == "\n");
      T(str.charAt(str.length - 2) == "\n");

      // otherwise we'll continue to receive heartbeats forever
      xhr.abort();
    }
    db.deleteDb();
  }

  // these will NEVER run as we're always in navigator == undefined
  if (!is_safari && xhr) {
    // test Server Sent Event (eventsource)
    if (!!window.EventSource) {
      var source = new EventSource(
              "/" + db_name + "/_changes?feed=eventsource");
      var results = [];
      var sourceListener = function(e) {
        var data = JSON.parse(e.data);
        results.push(data);
      };

      source.addEventListener('message', sourceListener , false);

      waitForSuccess(function() {
        if (results.length != 3) {
          throw "bad seq, try again";
        }
        return true;
      });

      source.removeEventListener('message', sourceListener, false);

      T(results[0].seq == 1);
      T(results[0].id == "foo");

      T(results[1].seq == 2);
      T(results[1].id == "bar");
      T(results[1].changes[0].rev == docBar._rev);
    }

    // test that we receive EventSource heartbeat events
    if (!!window.EventSource) {
      var source = new EventSource(
              "/" + db_name + "/_changes?feed=eventsource&heartbeat=10");

      var count_heartbeats = 0;
      source.addEventListener('heartbeat', function () { count_heartbeats = count_heartbeats + 1; } , false);

      waitForSuccess(function() {
        if (count_heartbeats < 3) {
          throw "keep waiting";
        }
        return true;
      }, "eventsource-heartbeat");

      T(count_heartbeats >= 3);
      source.close();
    }

    // test longpolling
    xhr = CouchDB.newXhr();

    xhr.open("GET", CouchDB.proxyUrl("/" + db_name + "/_changes?feed=longpoll"), true);
    xhr.send("");

    waitForSuccess(function() {
      lines = xhr.responseText.split("\n");
      if (lines[5] != '"last_seq":3}') {
        throw("still waiting");
      }
      return true;
    }, "last_seq");

    xhr = CouchDB.newXhr();

    xhr.open("GET", CouchDB.proxyUrl("/" + db_name + "/_changes?feed=longpoll&since=3"), true);
    xhr.send("");

    var docBarz = {_id:"barz", bar:1};
    db.save(docBarz);

    var parse_changes_line = function(line) {
      if (line.charAt(line.length-1) == ",") {
        var linetrimmed = line.substring(0, line.length-1);
      } else {
        var linetrimmed = line;
      }
      return JSON.parse(linetrimmed);
    };

    waitForSuccess(function() {
      lines = xhr.responseText.split("\n");
      if (lines[3] != '"last_seq":4}') {
        throw("still waiting");
      }
      return true;
    }, "change_lines");

    var change = parse_changes_line(lines[1]);
    T(change.seq == 4);
    T(change.id == "barz");
    T(change.changes[0].rev == docBarz._rev);
    T(lines[3]=='"last_seq":4}');


    // test since=now
    xhr = CouchDB.newXhr();

    xhr.open("GET", "/" + db_name + "/_changes?feed=longpoll&since=now", true);
    xhr.send("");

    var docBarz = {_id:"barzzzz", bar:1};
    db.save(docBarz);

    var parse_changes_line = function(line) {
      if (line.charAt(line.length-1) == ",") {
        var linetrimmed = line.substring(0, line.length-1);
      } else {
        var linetrimmed = line;
      }
      return JSON.parse(linetrimmed);
    };

    waitForSuccess(function() {
      lines = xhr.responseText.split("\n");
      if (lines[3] != '"last_seq":5}') {
        throw("still waiting");
      }
      return true;
    }, "change_lines");

    var change = parse_changes_line(lines[1]);
    T(change.seq == 5);
    T(change.id == "barzzzz");
    T(change.changes[0].rev == docBarz._rev);
    T(lines[3]=='"last_seq":5}');
  }

  db.deleteDb();
  // test on a new DB
  var db_name = get_random_db_name();
  db = new CouchDB(db_name, {"X-Couch-Full-Commit":"true"}, {"w": 3});
  db.createDb();

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
      "conflicted" : "function(doc, req) { return (doc._conflicts);}"
    },
    options : {
      local_seq : true
    },
    views : {
      local_seq : {
        map : "function(doc) {emit(doc._local_seq, null)}"
      },
      blah: {
        map : 'function(doc) {' +
              '  if (doc._id == "blah") {' +
              '    emit(null, null);' +
              '  }' +
              '}'
      }
    }
  };

  db.save(ddoc);

  var req = CouchDB.request("GET", "/" + db_name + "/_changes?filter=changes_filter/bop");
  var resp = JSON.parse(req.responseText);
  T(resp.results.length == 0);

  var docres1 = db.save({"bop" : "foom"});
  T(docres1.ok);
  var docres2 = db.save({"bop" : false});
  T(docres2.ok);

  var req = CouchDB.request("GET", "/" + db_name + "/_changes?filter=changes_filter/bop");
  var resp = JSON.parse(req.responseText);
  var seqold = resp.results[0].seq;
  T(resp.results.length == 1, "filtered/bop");
  T(resp.results[0].changes[0].rev == docres1.rev, "filtered/bop rev");
  // save and reload (substitute for all those parts that never run)
  var chgdoc1 = db.open(docres1.id);
  chgdoc1.newattr = "s/th new";
  docres1 = db.save(chgdoc1);
  T(docres1.ok);
  req = CouchDB.request("GET", "/" + db_name + "/_changes?filter=changes_filter/bop");
  resp = JSON.parse(req.responseText);
  var seqchg = resp.results[0].seq;
  T(resp.results.length == 1, "filtered/bop new");
  T(resp.results[0].changes[0].rev == docres1.rev, "filtered/bop rev new");
  T(seqold != seqchg, "filtered/bop new seq number");

  req = CouchDB.request("GET", "/" + db_name + "/_changes?filter=changes_filter/dynamic&field=woox");
  resp = JSON.parse(req.responseText);
  T(resp.results.length == 0);

  req = CouchDB.request("GET", "/" + db_name + "/_changes?filter=changes_filter/dynamic&field=bop");
  resp = JSON.parse(req.responseText);
  T(resp.results.length == 1, "changes_filter/dynamic&field=bop");
  T(resp.results[0].changes[0].rev == docres1.rev, "filtered/dynamic&field=bop rev");

  // these will NEVER run as we're always in navigator == undefined
  if (!is_safari && xhr) { // full test requires parallel connections
    // filter with longpoll
    // longpoll filters full history when run without a since seq
    xhr = CouchDB.newXhr();
    xhr.open("GET", CouchDB.proxyUrl("/" + db_name + "/_changes?feed=longpoll&filter=changes_filter/bop"), false);
    xhr.send("");
    var resp = JSON.parse(xhr.responseText);
    T(resp.last_seq == 8);
    // longpoll waits until a matching change before returning
    xhr = CouchDB.newXhr();
    xhr.open("GET", CouchDB.proxyUrl("/" + db_name + "/_changes?feed=longpoll&since=7&filter=changes_filter/bop"), true);
    xhr.send("");
    db.save({"_id":"falsy", "bop" : ""}); // empty string is falsy
    db.save({"_id":"bingo","bop" : "bingo"});

    waitForSuccess(function() {
      resp = JSON.parse(xhr.responseText);
      return true;
    }, "longpoll-since");

    T(resp.last_seq == 10);
    T(resp.results && resp.results.length > 0 && resp.results[0]["id"] == "bingo", "filter the correct update");
    xhr.abort();

    var timeout = 500;
    var last_seq = 11;
    while (true) {

      // filter with continuous
      xhr = CouchDB.newXhr();
      xhr.open("GET", CouchDB.proxyUrl("/" + db_name + "/_changes?feed=continuous&filter=changes_filter/bop&timeout="+timeout), true);
      xhr.send("");

      db.save({"_id":"rusty", "bop" : "plankton"});
      T(xhr.readyState != 4, "test client too slow");
      var rusty = db.open("rusty", {cache_bust : new Date()});
      T(rusty._id == "rusty");

      waitForSuccess(function() { // throws an error after 5 seconds
        if (xhr.readyState != 4) {
          throw("still waiting");
        }
        return true;
      }, "continuous-rusty");
      lines = xhr.responseText.split("\n");
      var good = false;
      try {
        JSON.parse(lines[3]);
        good = true;
      } catch(e) {
      }
      if (good) {
        T(JSON.parse(lines[1]).id == "bingo", lines[1]);
        T(JSON.parse(lines[2]).id == "rusty", lines[2]);
        T(JSON.parse(lines[3]).last_seq == last_seq, lines[3]);
        break;
      } else {
        xhr.abort();
        db.deleteDoc(rusty);
        timeout = timeout * 2;
        last_seq = last_seq + 2;
      }
    }
  }
  // error conditions

  // non-existing design doc
  var req = CouchDB.request("GET",
    "/" + db_name + "/_changes?filter=nothingtosee/bop");
  TEquals(404, req.status, "should return 404 for non existant design doc");

  // non-existing filter
  var req = CouchDB.request("GET",
    "/" + db_name + "/_changes?filter=changes_filter/movealong");
  TEquals(404, req.status, "should return 404 for non existant filter fun");

  // both
  var req = CouchDB.request("GET",
    "/" + db_name + "/_changes?filter=nothingtosee/movealong");
  TEquals(404, req.status,
    "should return 404 for non existant design doc and filter fun");

  // changes get all_docs style with deleted docs
  var doc = {a:1};
  db.save(doc);
  db.deleteDoc(doc);
  var req = CouchDB.request("GET",
    "/" + db_name + "/_changes?filter=changes_filter/bop&style=all_docs");
  var resp = JSON.parse(req.responseText);
  var expect = (!is_safari && xhr) ? 3: 1;
  TEquals(expect, resp.results.length, "should return matching rows");

  // test filter on view function (map)
  //
  T(db.save({"_id":"blah", "bop" : "plankton"}).ok);
  var req = CouchDB.request("GET", "/" + db_name + "/_changes?filter=_view&view=changes_filter/blah");
  var resp = JSON.parse(req.responseText);
  T(resp.results.length === 1);
  T(resp.results[0].id === "blah");


  // test for userCtx
// TODO: either make part of global config, or allow 4 config changes - or leave out
/*
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
      var req = CouchDB.request("GET", "/" + db_name + "/_changes?filter=changes_filter/userCtx", authOpts);
      var resp = JSON.parse(req.responseText);
      T(resp.results.length == 0);

      var docResp = db.save({"user" : "Chris Anderson"});
      T(docResp.ok);
      T(db.ensureFullCommit().ok);
      req = CouchDB.request("GET", "/" + db_name + "/_changes?filter=changes_filter/userCtx", authOpts);
      resp = JSON.parse(req.responseText);
      T(resp.results.length == 1, "userCtx");
      T(resp.results[0].id == docResp.id);
    }
  );
*/

  req = CouchDB.request("GET", "/" + db_name + "/_changes?limit=1");
  resp = JSON.parse(req.responseText);
  TEquals(1, resp.results.length);

  //filter includes _conflicts
// TODO: all_or_nothing not yet in place
//  var id = db.save({'food' : 'pizza'}).id;
//  db.bulkSave([{_id: id, 'food' : 'pasta'}], {all_or_nothing:true});
//
//  req = CouchDB.request("GET", "/" + db_name + "/_changes?filter=changes_filter/conflicted");
//  resp = JSON.parse(req.responseText);
//  T(resp.results.length == 1, "filter=changes_filter/conflicted");

  // test with erlang filter function
// TODO: either make part of global config, or allow 4 config changes - or leave out
/*
  run_on_modified_server([{
    section: "native_query_servers",
    key: "erlang",
    value: "{couch_native_process, start_link, []}"
  }], function() {
    var erl_ddoc = {
      _id: "_design/erlang",
      language: "erlang",
      filters: {
        foo:
          'fun({Doc}, Req) -> ' +
          '  case couch_util:get_value(<<"value">>, Doc) of' +
          '  undefined -> false;' +
          '  Value -> (Value rem 2) =:= 0;' +
          '  _ -> false' +
          '  end ' +
          'end.'
      }
    };

    db.deleteDb();
    db.createDb();
    T(db.save(erl_ddoc).ok);

    var req = CouchDB.request("GET", "/" + db_name + "/_changes?filter=erlang/foo");
    var resp = JSON.parse(req.responseText);
    T(resp.results.length === 0);

    T(db.save({_id: "doc1", value : 1}).ok);
    T(db.save({_id: "doc2", value : 2}).ok);
    T(db.save({_id: "doc3", value : 3}).ok);
    T(db.save({_id: "doc4", value : 4}).ok);

    var req = CouchDB.request("GET", "/" + db_name + "/_changes?filter=erlang/foo");
    var resp = JSON.parse(req.responseText);
    T(resp.results.length === 2);
    T(resp.results[0].id === "doc2");
    T(resp.results[1].id === "doc4");

    // test filtering on docids
    //

    var options = {
        headers: {"Content-Type": "application/json"},
        body: JSON.stringify({"doc_ids": ["something", "anotherthing", "andmore"]})
    };

    var req = CouchDB.request("POST", "/" + db_name + "/_changes?filter=_doc_ids", options);
    var resp = JSON.parse(req.responseText);
    T(resp.results.length === 0);

    T(db.save({"_id":"something", "bop" : "plankton"}).ok);
    var req = CouchDB.request("POST", "/" + db_name + "/_changes?filter=_doc_ids", options);
    var resp = JSON.parse(req.responseText);
    T(resp.results.length === 1);
    T(resp.results[0].id === "something");

    T(db.save({"_id":"anotherthing", "bop" : "plankton"}).ok);
    var req = CouchDB.request("POST", "/" + db_name + "/_changes?filter=_doc_ids", options);
    var resp = JSON.parse(req.responseText);
    T(resp.results.length === 2);
    T(resp.results[0].id === "something");
    T(resp.results[1].id === "anotherthing");

    var docids = JSON.stringify(["something", "anotherthing", "andmore"]),
        req = CouchDB.request("GET", "/" + db_name + "/_changes?filter=_doc_ids&doc_ids="+docids, options);
    var resp = JSON.parse(req.responseText);
    T(resp.results.length === 2);
    T(resp.results[0].id === "something");
    T(resp.results[1].id === "anotherthing");

    var req = CouchDB.request("GET", "/" + db_name + "/_changes?filter=_design");
    var resp = JSON.parse(req.responseText);
    T(resp.results.length === 1);
    T(resp.results[0].id === "_design/erlang");


    if (!is_safari && xhr) {
        // filter docids with continuous
        xhr = CouchDB.newXhr();
        xhr.open("POST", CouchDB.proxyUrl("/" + db_name + "/_changes?feed=continuous&timeout=500&since=7&filter=_doc_ids"), true);
        xhr.setRequestHeader("Content-Type", "application/json");

        xhr.send(options.body);

        T(db.save({"_id":"andmore", "bop" : "plankton"}).ok);

        waitForSuccess(function() {
            if (xhr.readyState != 4) {
              throw("still waiting");
            }
            return true;
        }, "andmore-only");

        var line = JSON.parse(xhr.responseText.split("\n")[0]);
        T(line.seq == 8);
        T(line.id == "andmore");
    }
  });
*/

  db.deleteDb();
  // COUCHDB-1037 - empty result for ?limit=1&filter=foo/bar in some cases
  // test w/ new temp DB
  db_name = get_random_db_name();
  db = new CouchDB(db_name, {"X-Couch-Full-Commit":"true"}, {"w": 3});
  T(db.createDb());

  ddoc = {
    _id: "_design/testdocs",
    filters: {
      testdocsonly: (function(doc, req) {
        return (typeof doc.integer === "number");
      }).toString()
    }
  };
  T(db.save(ddoc));

  ddoc = {
    _id: "_design/foobar",
    foo: "bar"
  };
  T(db.save(ddoc));

  db.bulkSave(makeDocs(0, 5));

// for n>1 you can't be sure all docs are there immediately - so either stick w/ -n 1 or implement check-wait-check or use the quorum (for now, the latter seems 2 suffice)

  req = CouchDB.request("GET", "/" + db.name + "/_changes");
  resp = JSON.parse(req.responseText);
  // you can't know wether 7 is the last seq as you don't know how many collapse into one number
  //TEquals(7, resp.last_seq);
  TEquals(7, resp.results.length);

  req = CouchDB.request(
    "GET", "/"+ db.name + "/_changes?limit=1&filter=testdocs/testdocsonly");
  resp = JSON.parse(req.responseText);
  // (seq as before)
  //TEquals(3, resp.last_seq);
  TEquals(1, resp.results.length);
  // also, we can't guarantee ordering
  T(resp.results[0].id.match("[0-5]"));

  req = CouchDB.request(
    "GET", "/" + db.name + "/_changes?limit=2&filter=testdocs/testdocsonly");
  resp = JSON.parse(req.responseText);
  // (seq as before)
  //TEquals(4, resp.last_seq);
  TEquals(2, resp.results.length);
  // also, we can't guarantee ordering
  T(resp.results[0].id.match("[0-5]"));
  T(resp.results[1].id.match("[0-5]"));

// TODO: either use local port for stats (and aggregate when n>1) or leave out
//  TEquals(0, CouchDB.requestStats(['couchdb', 'httpd', 'clients_requesting_changes'], true).value);
//  CouchDB.request("GET", "/" + db.name + "/_changes");
//  TEquals(0, CouchDB.requestStats(['couchdb', 'httpd', 'clients_requesting_changes'], true).value);

  db.deleteDb();
  // COUCHDB-1256
  // test w/ new temp DB
  db_name = get_random_db_name();
  db = new CouchDB(db_name, {"X-Couch-Full-Commit":"true"}, {"w": 3});
  T(db.createDb());

  T(db.save({"_id":"foo", "a" : 123}).ok);
  T(db.save({"_id":"bar", "a" : 456}).ok);

  options = {
      headers: {"Content-Type": "application/json"},
      body: JSON.stringify({"_rev":"1-cc609831f0ca66e8cd3d4c1e0d98108a", "a":456})
  };
  req = CouchDB.request("PUT", "/" + db.name + "/foo?new_edits=false", options);

  req = CouchDB.request("GET", "/" + db.name + "/_changes?style=all_docs");
  resp = JSON.parse(req.responseText);

  // (seq as before)
  //TEquals(3, resp.last_seq);
  TEquals(2, resp.results.length);

  // we can no longer pass a number into 'since' - but we have the 2nd last above - so we can use it (puh!)
  req = CouchDB.request("GET", "/" + db.name + "/_changes?style=all_docs&since=" + encodeURIComponent(resp.results[0].seq));
  resp = JSON.parse(req.responseText);

  // (seq as before)
  //TEquals(3, resp.last_seq);
  TEquals(1, resp.results.length);
  // TEquals(2, resp.results[0].changes.length);

  db.deleteDb();
  // COUCHDB-1852
  // test w/ new temp DB
  db_name = get_random_db_name();
  db = new CouchDB(db_name, {"X-Couch-Full-Commit":"true"}, {"w": 3});
  T(db.createDb());

  // create 4 documents... this assumes the update sequnce will start from 0 and then do sth in the cluster
  db.save({"bop" : "foom"});
  db.save({"bop" : "foom"});
  db.save({"bop" : "foom"});
  db.save({"bop" : "foom"});
  // because of clustering, we need the 2nd entry as since value
  req = CouchDB.request("GET", "/" + db_name + "/_changes");

  // simulate an EventSource request with a Last-Event-ID header
  // increase timeout to 100 to have enough time 2 assemble (seems like too little timeouts kill
  req = CouchDB.request("GET", "/" + db_name + "/_changes?feed=eventsource&timeout=100&since=0",
        {"headers": {"Accept": "text/event-stream", "Last-Event-ID": JSON.parse(req.responseText).results[1].seq}});

  // "parse" the eventsource response and collect only the "id: ..." lines
  var changes = req.responseText.split('\n')
     .map(function (el) {
        return el.split(":").map(function (el) { return el.trim()});
     })
     .filter(function (el) { return (el[0] === "id"); })

  // make sure we only got 2 changes, and they are update_seq=3 and update_seq=4
  T(changes.length === 2);
  // seq is different now
  //T(changes[0][1] === "3");
  //T(changes[1][1] === "4");

  db.deleteDb();
  // COUCHDB-1923
  // test w/ new temp DB
  db_name = get_random_db_name();
  db = new CouchDB(db_name, {"X-Couch-Full-Commit":"true"}, {"w": 3});
  T(db.createDb());

  var attachmentData = "VGhpcyBpcyBhIGJhc2U2NCBlbmNvZGVkIHRleHQ=";

  db.bulkSave(makeDocs(20, 30, {
    _attachments:{
      "foo.txt": {
        content_type:"text/plain",
        data: attachmentData
      },
      "bar.txt": {
        content_type:"text/plain",
        data: attachmentData
      }
    }
  }));

  var mapFunction = function(doc) {
    var count = 0;

    for(var idx in doc._attachments) {
      count = count + 1;
    }

    emit(parseInt(doc._id), count);
  };

  var req = CouchDB.request("GET", "/" + db_name + "/_changes?include_docs=true");
  var resp = JSON.parse(req.responseText);

  T(resp.results.length == 10);
  T(resp.results[0].doc._attachments['foo.txt'].stub === true);
  T(resp.results[0].doc._attachments['foo.txt'].data === undefined);
  T(resp.results[0].doc._attachments['foo.txt'].encoding === undefined);
  T(resp.results[0].doc._attachments['foo.txt'].encoded_length === undefined);
  T(resp.results[0].doc._attachments['bar.txt'].stub === true);
  T(resp.results[0].doc._attachments['bar.txt'].data === undefined);
  T(resp.results[0].doc._attachments['bar.txt'].encoding === undefined);
  T(resp.results[0].doc._attachments['bar.txt'].encoded_length === undefined);

  var req = CouchDB.request("GET", "/" + db_name + "/_changes?include_docs=true&attachments=true");
  var resp = JSON.parse(req.responseText);

  T(resp.results.length == 10);
  T(resp.results[0].doc._attachments['foo.txt'].stub === undefined);
  T(resp.results[0].doc._attachments['foo.txt'].data === attachmentData);
  T(resp.results[0].doc._attachments['foo.txt'].encoding === undefined);
  T(resp.results[0].doc._attachments['foo.txt'].encoded_length === undefined);
  T(resp.results[0].doc._attachments['bar.txt'].stub === undefined);
  T(resp.results[0].doc._attachments['bar.txt'].data == attachmentData);
  T(resp.results[0].doc._attachments['bar.txt'].encoding === undefined);
  T(resp.results[0].doc._attachments['bar.txt'].encoded_length === undefined);

  var req = CouchDB.request("GET", "/" + db_name + "/_changes?include_docs=true&att_encoding_info=true");
  var resp = JSON.parse(req.responseText);

  T(resp.results.length == 10);
  T(resp.results[0].doc._attachments['foo.txt'].stub === true);
  T(resp.results[0].doc._attachments['foo.txt'].data === undefined);
  T(resp.results[0].doc._attachments['foo.txt'].encoding === "gzip");
  T(resp.results[0].doc._attachments['foo.txt'].encoded_length === 47);
  T(resp.results[0].doc._attachments['bar.txt'].stub === true);
  T(resp.results[0].doc._attachments['bar.txt'].data === undefined);
  T(resp.results[0].doc._attachments['bar.txt'].encoding === "gzip");
  T(resp.results[0].doc._attachments['bar.txt'].encoded_length === 47);

  db.deleteDb();
};
