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

couchTests.show_documents = function(debug) {

  var db_name = get_random_db_name();
  var db = new CouchDB(db_name, {"X-Couch-Full-Commit":"false"});
  db.createDb();
  if (debug) debugger;

  var designDoc = {
    _id:"_design/template",
    language: "javascript",
    shows: {
      "hello" : stringFun(function(doc, req) {
        log("hello fun");
        if (doc) {
          return "Hello World";
        } else {
          if(req.id) {
            return "New World";
          } else {
            return "Empty World";
          }
        }
      }),
      "just-name" : stringFun(function(doc, req) {
        if (doc) {
          return {
            body : "Just " + doc.name
          };
        } else {
          return {
            body : "No such doc",
            code : 404
          };
        }
      }),
      "json" : stringFun(function(doc, req) {
        return {
          json : doc
        }
      }),
      "req-info" : stringFun(function(doc, req) {
        return {
          json : req
        }
      }),
      "show-deleted" : stringFun(function(doc, req) {
        if(doc) {
          return doc._id;
        } else {
          return "No doc " + req.id;
        }
      }),
      "render-error" : stringFun(function(doc, req) {
        return noSuchVariable;
      }),
      "empty" : stringFun(function(doc, req) {
          return "";
        }),
      "fail" : stringFun(function(doc, req) {
        return doc._id;
      }),
      "no-set-etag" : stringFun(function(doc, req) {
        return {
          headers : {
            "Etag" : "skipped"
          },
          "body" : "something"
        }
      }),
      "list-api" : stringFun(function(doc, req) {
        start({"X-Couch-Test-Header": "Yeah"});
        send("Hey");
      }),
      "list-api-provides" : stringFun(function(doc, req) {
        provides("text", function(){
            send("foo, ");
            send("bar, ");
            send("baz!");
        })
      }),
      "list-api-provides-and-return" : stringFun(function(doc, req) {
        provides("text", function(){
            send("4, ");
            send("5, ");
            send("6, ");
            return "7!";
        })
        send("1, ");
        send("2, ");
        return "3, ";
      }),
      "list-api-mix" : stringFun(function(doc, req) {
        start({"X-Couch-Test-Header": "Yeah"});
        send("Hey ");
        return "Dude";
      }),
      "list-api-mix-with-header" : stringFun(function(doc, req) {
        start({"X-Couch-Test-Header": "Yeah"});
        send("Hey ");
        return {
          headers: {
            "X-Couch-Test-Header-Awesome": "Oh Yeah!"
          },
          body: "Dude"
        };
      }),
      "accept-switch" : stringFun(function(doc, req) {
        if (req.headers["Accept"].match(/image/)) {
          return {
            // a 16x16 px version of the CouchDB logo
            "base64" :
["iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAMAAAAoLQ9TAAAAsV",
"BMVEUAAAD////////////////////////5ur3rEBn////////////////wDBL/",
"AADuBAe9EB3IEBz/7+//X1/qBQn2AgP/f3/ilpzsDxfpChDtDhXeCA76AQH/v7",
"/84eLyWV/uc3bJPEf/Dw/uw8bRWmP1h4zxSlD6YGHuQ0f6g4XyQkXvCA36MDH6",
"wMH/z8/yAwX64ODeh47BHiv/Ly/20dLQLTj98PDXWmP/Pz//39/wGyJ7Iy9JAA",
"AADHRSTlMAbw8vf08/bz+Pv19jK/W3AAAAg0lEQVR4Xp3LRQ4DQRBD0QqTm4Y5",
"zMxw/4OleiJlHeUtv2X6RbNO1Uqj9g0RMCuQO0vBIg4vMFeOpCWIWmDOw82fZx",
"vaND1c8OG4vrdOqD8YwgpDYDxRgkSm5rwu0nQVBJuMg++pLXZyr5jnc1BaH4GT",
"LvEliY253nA3pVhQqdPt0f/erJkMGMB8xucAAAAASUVORK5CYII="].join(''),
            headers : {
              "Content-Type" : "image/png",
              "Vary" : "Accept" // we set this for proxy caches
            }
          };
        } else {
          return {
            "body" : "accepting text requests",
            headers : {
              "Content-Type" : "text/html",
              "Vary" : "Accept"
            }
          };
        }
      }),
      "provides" : stringFun(function(doc, req) {
        registerType("foo", "application/foo","application/x-foo");

        provides("html", function() {
          return "Ha ha, you said \"" + doc.word + "\".";
        });

        provides("foo", function() {
          return "foofoo";
        });
      }),
      "withSlash": stringFun(function(doc, req) {
        return { json: doc }
      }),
      "secObj": stringFun(function(doc, req) {
        return { json: req.secObj };
      })
    }
  };
  T(db.save(designDoc).ok);

  var doc = {"word":"plankton", "name":"Rusty"}
  var resp = db.save(doc);
  T(resp.ok);
  var docid = resp.id;

  // show error
  var xhr = CouchDB.request("GET", "/" + db_name + "/_design/template/_show/");
  T(xhr.status == 404, 'Should be missing');
  T(JSON.parse(xhr.responseText).reason == "Invalid path.");

  // hello template world
  xhr = CouchDB.request("GET", "/" + db_name + "/_design/template/_show/hello/"+docid);
  T(xhr.responseText == "Hello World", "hello");
  T(/charset=utf-8/.test(xhr.getResponseHeader("Content-Type")));


  // Fix for COUCHDB-379
  T(equals(xhr.getResponseHeader("Server").substr(0,7), "CouchDB"));

  // // error stacktraces
  // xhr = CouchDB.request("GET", "/" + db_name + "/_design/template/_show/render-error/"+docid);
  // T(JSON.parse(xhr.responseText).error == "render_error");

  // hello template world (no docid)
  xhr = CouchDB.request("GET", "/" + db_name + "/_design/template/_show/hello");
  T(xhr.responseText == "Empty World");

  // hello template world (no docid)
  xhr = CouchDB.request("GET", "/" + db_name + "/_design/template/_show/empty");
  T(xhr.responseText == "");

  // // hello template world (non-existing docid)
  xhr = CouchDB.request("GET", "/" + db_name + "/_design/template/_show/fail/nonExistingDoc");
  T(xhr.status == 404);
  var resp = JSON.parse(xhr.responseText);
  T(resp.error == "not_found");

  // show with doc
  xhr = CouchDB.request("GET", "/" + db_name + "/_design/template/_show/just-name/"+docid);
  T(xhr.responseText == "Just Rusty");

  // show with missing doc
  xhr = CouchDB.request("GET", "/" + db_name + "/_design/template/_show/just-name/missingdoc");
  T(xhr.status == 404);
  TEquals("No such doc", xhr.responseText);

  // show with missing func
  xhr = CouchDB.request("GET", "/" + db_name + "/_design/template/_show/missing/"+docid);
  T(xhr.status == 404, "function is missing");

  // missing design doc
  xhr = CouchDB.request("GET", "/" + db_name + "/_design/missingddoc/_show/just-name/"+docid);
  T(xhr.status == 404);
  var resp = JSON.parse(xhr.responseText);
  T(resp.error == "not_found");

  // query parameters
  xhr = CouchDB.request("GET", "/" + db_name + "/_design/template/_show/req-info/"+docid+"?foo=bar", {
    headers: {
      "Accept": "text/html;text/plain;*/*",
      "X-Foo" : "bar"
    }
  });
  var resp = JSON.parse(xhr.responseText);
  T(equals(resp.headers["X-Foo"], "bar"));
  T(equals(resp.query, {foo:"bar"}));
  T(equals(resp.method, "GET"));
  T(equals(resp.path[5], docid));
  T(equals(resp.info.db_name, "" + db_name + ""));

  // accept header switching
  // different mime has different etag
  xhr = CouchDB.request("GET", "/" + db_name + "/_design/template/_show/accept-switch/"+docid, {
    headers: {"Accept": "text/html;text/plain;*/*"}
  });
  var ct = xhr.getResponseHeader("Content-Type");
  T(/text\/html/.test(ct))
  T("Accept" == xhr.getResponseHeader("Vary"));
  var etag = xhr.getResponseHeader("etag");

  xhr = CouchDB.request("GET", "/" + db_name + "/_design/template/_show/accept-switch/"+docid, {
    headers: {"Accept": "image/png;*/*"}
  });
  T(xhr.responseText.match(/PNG/))
  T("image/png" == xhr.getResponseHeader("Content-Type"));
  var etag2 = xhr.getResponseHeader("etag");
  T(etag2 != etag);

  // proper etags
  // show with doc
  xhr = CouchDB.request("GET", "/" + db_name + "/_design/template/_show/just-name/"+docid);
  // extract the ETag header values
  etag = xhr.getResponseHeader("etag");
  // get again with etag in request
  xhr = CouchDB.request("GET", "/" + db_name + "/_design/template/_show/just-name/"+docid, {
    headers: {"if-none-match": etag}
  });
  // should be 304
  T(xhr.status == 304);

  // update the doc
  doc.name = "Crusty";
  resp = db.save(doc);
  T(resp.ok);
  // req with same etag
  xhr = CouchDB.request("GET", "/" + db_name + "/_design/template/_show/just-name/"+docid, {
    headers: {"if-none-match": etag}
  });
  // status is 200
  T(xhr.status == 200);

  // JS can't set etag
  xhr = CouchDB.request("GET", "/" + db_name + "/_design/template/_show/no-set-etag/"+docid);
  // extract the ETag header values
  etag = xhr.getResponseHeader("etag");
  T(etag != "skipped")

  // test the provides mime matcher
  xhr = CouchDB.request("GET", "/" + db_name + "/_design/template/_show/provides/"+docid, {
    headers: {
      "Accept": 'text/html,application/atom+xml; q=0.9'
    }
  });
  var ct = xhr.getResponseHeader("Content-Type");
  T(/charset=utf-8/.test(ct))
  T(/text\/html/.test(ct))
  T(xhr.responseText == "Ha ha, you said \"plankton\".");

  // registering types works
  xhr = CouchDB.request("GET", "/" + db_name + "/_design/template/_show/provides/"+docid, {
    headers: {
      "Accept": "application/x-foo"
    }
  });
  T(xhr.getResponseHeader("Content-Type") == "application/x-foo");
  T(xhr.responseText.match(/foofoo/));

  // test the provides mime matcher without a match
  xhr = CouchDB.request("GET", "/" + db_name + "/_design/template/_show/provides/"+docid, {
   headers: {
     "Accept": 'text/monkeys'
   }
  });
  var rs = JSON.parse(xhr.responseText);
  T(rs.error == "not_acceptable")


  // test inclusion of conflict state
  var doc1 = {_id:"foo", a:1};
  var doc2 = {_id:"foo", a:2};
  db.save(doc1);

  var doc3 = {_id:"a/b/c", a:1};
  db.save(doc3);
  xhr = CouchDB.request("GET", "/" + db_name + "/_design/template/_show/withSlash/a/b/c");
  T(xhr.status == 200);

  // hello template world (non-existing docid)
  xhr = CouchDB.request("GET", "/" + db_name + "/_design/template/_show/hello/nonExistingDoc");
  T(xhr.responseText == "New World");

  // test list() compatible API
  xhr = CouchDB.request("GET", "/" + db_name + "/_design/template/_show/list-api/foo");
  T(xhr.responseText == "Hey");
  TEquals("Yeah", xhr.getResponseHeader("X-Couch-Test-Header"), "header should be cool");

  // test list() compatible API with provides function
  xhr = CouchDB.request("GET", "/" + db_name + "/_design/template/_show/list-api-provides/foo?format=text");
  TEquals(xhr.responseText, "foo, bar, baz!", "should join chunks to response body");

  // should keep next result order: chunks + return value + provided chunks + provided return value
  xhr = CouchDB.request("GET", "/" + db_name + "/_design/template/_show/list-api-provides-and-return/foo?format=text");
  TEquals(xhr.responseText, "1, 2, 3, 4, 5, 6, 7!", "should not break 1..7 range");

  xhr = CouchDB.request("GET", "/" + db_name + "/_design/template/_show/list-api-mix/foo");
  T(xhr.responseText == "Hey Dude");
  TEquals("Yeah", xhr.getResponseHeader("X-Couch-Test-Header"), "header should be cool");

  xhr = CouchDB.request("GET", "/" + db_name + "/_design/template/_show/list-api-mix-with-header/foo");
  T(xhr.responseText == "Hey Dude");
  TEquals("Yeah", xhr.getResponseHeader("X-Couch-Test-Header"), "header should be cool");
  TEquals("Oh Yeah!", xhr.getResponseHeader("X-Couch-Test-Header-Awesome"), "header should be cool");

  // test deleted docs
  var doc = {_id:"testdoc",foo:1};
  db.save(doc);
  var xhr = CouchDB.request("GET", "/" + db_name + "/_design/template/_show/show-deleted/testdoc");
  TEquals("testdoc", xhr.responseText, "should return 'testdoc'");

  db.deleteDoc(doc);
  var xhr = CouchDB.request("GET", "/" + db_name + "/_design/template/_show/show-deleted/testdoc");
  TEquals("No doc testdoc", xhr.responseText, "should return 'no doc testdoc'");

  // (we don't need no modified server!)
  T(db.setDbProperty("_security", {foo: true}).ok);
  T(db.save({_id:"testdoc",foo:1}).ok);
  // nasty source of Heisenbugs - it replicates after a short time, so give it some tries
  // (needs PR #400 and #401 to be merged)
  retry_part(function(){
    xhr = CouchDB.request("GET", "/" + db_name + "/_design/template/_show/secObj");
    var resp = JSON.parse(xhr.responseText);
    T(resp.foo == true);
  }, 10);

  // cleanup
  db.deleteDb();

};
