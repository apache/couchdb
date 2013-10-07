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


couchTests.update_documents = function(debug) {
  var db = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"false"});
  db.deleteDb();
  db.createDb();
  if (debug) debugger;
      
  var designDoc = {
    _id:"_design/update",
    language: "javascript",
    updates: {
      "hello" : stringFun(function(doc, req) {
        log(doc);
        log(req);
        if (!doc) {
          if (req.id) {
            return [
            // Creates a new document with the PUT docid,
            { _id : req.id,
              reqs : [req] },
            // and returns an HTML response to the client.
            "<p>New World</p>"];
          };
          //
          return [null, "<p>Empty World</p>"];
        };
        // we can update the document inline
        doc.world = "hello";
        // we can record aspects of the request or use them in application logic.
        doc.reqs && doc.reqs.push(req);
        doc.edited_by = req.userCtx;
        return [doc, "<p>hello doc</p>"];
      }),
      "in-place" : stringFun(function(doc, req) {
        var field = req.query.field;
        var value = req.query.value;
        var message = "set "+field+" to "+value;
        doc[field] = value;
        return [doc, message];
      }),
      "form-update" : stringFun(function(doc, req) {
        for (var field in req.form) {
          doc[field] = req.form[field];
        }
        var message = "updated doc from form";
        return [doc, message];
      }),
      "bump-counter" : stringFun(function(doc, req) {
        if (!doc.counter) doc.counter = 0;
        doc.counter += 1;
        var message = "<h1>bumped it!</h1>";
        return [doc, message];
      }),
      "error" : stringFun(function(doc, req) {
        superFail.badCrash;
      }),
       "get-uuid" : stringFun(function(doc, req) {
         return [null, req.uuid];
       }),
       "code-n-bump" : stringFun(function(doc,req) {
         if (!doc.counter) doc.counter = 0;
         doc.counter += 1;
         var message = "<h1>bumped it!</h1>";
         resp = {"code": 302, "body": message}
         return [doc, resp];
       }),
       "resp-code" : stringFun(function(doc,req) {
         resp = {"code": 302}
         return [null, resp];
       }),
       "resp-code-and-json" : stringFun(function(doc,req) {
         resp = {"code": 302, "json": {"ok": true}}
         return [{"_id": req["uuid"]}, resp];
       }),
       "binary" : stringFun(function(doc, req) {
         var resp = {
           "headers" : {
             "Content-Type" : "application/octet-stream"
           },
           "base64" : "aGVsbG8gd29ybGQh" // "hello world!" encoded
         };
         return [doc, resp];
       }),
      "empty" : stringFun(function(doc, req) {
        return [{}, 'oops'];
      })
    }
  };
  T(db.save(designDoc).ok);
  
  var doc = {"word":"plankton", "name":"Rusty"}
  var resp = db.save(doc);
  T(resp.ok);
  var docid = resp.id;

  // update error
  var xhr = CouchDB.request("POST", "/test_suite_db/_design/update/_update/");
  T(xhr.status == 404, 'Should be missing');
  T(JSON.parse(xhr.responseText).reason == "Invalid path.");
  
  // hello update world
  xhr = CouchDB.request("PUT", "/test_suite_db/_design/update/_update/hello/"+docid);
  T(xhr.status == 201);
  T(xhr.responseText == "<p>hello doc</p>");
  T(/charset=utf-8/.test(xhr.getResponseHeader("Content-Type")));
  T(equals(docid, xhr.getResponseHeader("X-Couch-Id")));

  doc = db.open(docid);
  T(doc.world == "hello");

  // Fix for COUCHDB-379
  T(equals(xhr.getResponseHeader("Server").substr(0,7), "CouchDB"));

  // hello update world (no docid)
  xhr = CouchDB.request("POST", "/test_suite_db/_design/update/_update/hello");
  T(xhr.status == 200);
  T(xhr.responseText == "<p>Empty World</p>");

  // no GET allowed
  xhr = CouchDB.request("GET", "/test_suite_db/_design/update/_update/hello");
  // T(xhr.status == 405); // TODO allow qs to throw error code as well as error message
  T(JSON.parse(xhr.responseText).error == "method_not_allowed");

  // // hello update world (non-existing docid)
  xhr = CouchDB.request("GET", "/test_suite_db/nonExistingDoc");
  T(xhr.status == 404);
  xhr = CouchDB.request("PUT", "/test_suite_db/_design/update/_update/hello/nonExistingDoc");
  T(xhr.status == 201);
  T(xhr.responseText == "<p>New World</p>");
  xhr = CouchDB.request("GET", "/test_suite_db/nonExistingDoc");
  T(xhr.status == 200);

  // in place update
  xhr = CouchDB.request("PUT", "/test_suite_db/_design/update/_update/in-place/"+docid+'?field=title&value=test');
  T(xhr.status == 201);
  T(xhr.responseText == "set title to test");
  doc = db.open(docid);
  T(doc.title == "test");
  
  // form update via application/x-www-form-urlencoded
  xhr = CouchDB.request("PUT", "/test_suite_db/_design/update/_update/form-update/"+docid, {
    headers : {"Content-Type":"application/x-www-form-urlencoded"},
    body    : "formfoo=bar&formbar=foo"
  });
  TEquals(201, xhr.status);
  TEquals("updated doc from form", xhr.responseText);
  doc = db.open(docid);
  TEquals("bar", doc.formfoo);
  TEquals("foo", doc.formbar);
  
  // bump counter
  xhr = CouchDB.request("PUT", "/test_suite_db/_design/update/_update/bump-counter/"+docid, {
    headers : {"X-Couch-Full-Commit":"true"}
  });
  T(xhr.status == 201);
  T(xhr.responseText == "<h1>bumped it!</h1>");
  doc = db.open(docid);
  T(doc.counter == 1);
  
  // _update honors full commit if you need it to
  xhr = CouchDB.request("PUT", "/test_suite_db/_design/update/_update/bump-counter/"+docid, {
    headers : {"X-Couch-Full-Commit":"true"}
  });
  
  var NewRev = xhr.getResponseHeader("X-Couch-Update-NewRev");
  doc = db.open(docid);
  T(doc['_rev'] == NewRev);
  
  
  T(doc.counter == 2);

  // Server provides UUID when POSTing without an ID in the URL
  xhr = CouchDB.request("POST", "/test_suite_db/_design/update/_update/get-uuid/");
  T(xhr.status == 200);
  T(xhr.responseText.length == 32);

  // COUCHDB-1229 - allow slashes in doc ids for update handlers
  // /db/_design/doc/_update/handler/doc/id

  var doc = {
      _id:"with/slash",
      counter:1
  };
  db.save(doc);
  xhr = CouchDB.request("PUT", "/test_suite_db/_design/update/_update/bump-counter/with/slash");
  TEquals(201, xhr.status, "should return a 200 status");
  TEquals("<h1>bumped it!</h1>", xhr.responseText, "should report bumping");

  var doc = db.open("with/slash");
  TEquals(2, doc.counter, "counter should be 2");

  // COUCHDB-648 - the code in the JSON response should be honored

  xhr = CouchDB.request("PUT", "/test_suite_db/_design/update/_update/code-n-bump/"+docid, {
    headers : {"X-Couch-Full-Commit":"true"}
  });
  T(xhr.status == 302);
  T(xhr.responseText == "<h1>bumped it!</h1>");
  doc = db.open(docid);
  T(doc.counter == 3);

  xhr = CouchDB.request("POST", "/test_suite_db/_design/update/_update/resp-code/");
  T(xhr.status == 302);

  xhr = CouchDB.request("POST", "/test_suite_db/_design/update/_update/resp-code-and-json/");
  TEquals(302, xhr.status);
  T(JSON.parse(xhr.responseText).ok);

  // base64 response
  xhr = CouchDB.request("PUT", "/test_suite_db/_design/update/_update/binary/"+docid, {
    headers : {"X-Couch-Full-Commit":"false"},
    body    : 'rubbish'
  });
  T(xhr.status == 201);
  T(xhr.responseText == "hello world!");
  T(/application\/octet-stream/.test(xhr.getResponseHeader("Content-Type")));

  // Insert doc with empty id
  xhr = CouchDB.request("PUT", "/test_suite_db/_design/update/_update/empty/foo");
  TEquals(400, xhr.status);
  TEquals("Document id must not be empty", JSON.parse(xhr.responseText).reason);

};
