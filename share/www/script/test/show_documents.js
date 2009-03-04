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


couchTests.show_documents = function(debug) {
  var db = new CouchDB("test_suite_db");
  db.deleteDb();
  db.createDb();
  if (debug) debugger;
      
  var designDoc = {
    _id:"_design/template",
    language: "javascript",
    shows: {
      "hello" : stringFun(function(doc, req) { 
        if (doc) {
          return "Hello World";
        } else {
          if(req.docId) {
            return "New World";
          } else {
            return "Empty World";
          }
        }
      }),
      "just-name" : stringFun(function(doc, req) {
        return {
          body : "Just " + doc.name
        };
      }),
      "req-info" : stringFun(function(doc, req) {
        return {
          json : req
        }
      }),
      "xml-type" : stringFun(function(doc, req) {
         return {
           "headers" : {
             "Content-Type" : "application/xml"
           },
           "body" : new XML('<xml><node foo="bar"/></xml>')
         }
       }),
      "no-set-etag" : stringFun(function(doc, req) {
        return {
          headers : {
            "Etag" : "skipped"
          },
          "body" : "something"
        }
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
      "respondWith" : stringFun(function(doc, req) {
        registerType("foo", "application/foo","application/x-foo");
        return respondWith(req, {
          html : function() {
            return {
              body:"Ha ha, you said \"" + doc.word + "\"."
            };
          },
          xml : function() {
            var xml = new XML('<xml><node/></xml>');
            // Becase Safari can't stand to see that dastardly
            // E4X outside of a string. Outside of tests you
            // can just use E4X literals.
            this.eval('xml.node.@foo = doc.word');
            return {
              body: xml
            };
          },
          foo : function() {
            return {
              body: "foofoo"
            };
          },
          fallback : "html"
        });
      })
    }
  };
  T(db.save(designDoc).ok);
  
  var doc = {"word":"plankton", "name":"Rusty"}
  var resp = db.save(doc);
  T(resp.ok);
  var docid = resp.id;

  // show error
  var xhr = CouchDB.request("GET", "/test_suite_db/_show/");
  T(xhr.status == 404);
  T(JSON.parse(xhr.responseText).reason == "Invalid path.");
  
  // hello template world
  xhr = CouchDB.request("GET", "/test_suite_db/_show/template/hello/"+docid);
  T(xhr.responseText == "Hello World");
 
  // hello template world (no docid)
  xhr = CouchDB.request("GET", "/test_suite_db/_show/template/hello/");
  T(xhr.responseText == "Empty World");

  // // hello template world (non-existing docid)
  xhr = CouchDB.request("GET", "/test_suite_db/_show/template/hello/nonExistingDoc");
  T(xhr.responseText == "New World");
  
  // show with doc
  xhr = CouchDB.request("GET", "/test_suite_db/_show/template/just-name/"+docid);
  T(xhr.responseText == "Just Rusty");
  
  
  // show with missing func
  xhr = CouchDB.request("GET", "/test_suite_db/_show/template/missing/"+docid);
  T(xhr.status == 404);

  // missing design doc
  xhr = CouchDB.request("GET", "/test_suite_db/_show/missingdoc/just-name/"+docid);
  T(xhr.status == 404);
  var resp = JSON.parse(xhr.responseText);
  T(resp.error == "not_found");

  // query parameters
  xhr = CouchDB.request("GET", "/test_suite_db/_show/template/req-info/"+docid+"?foo=bar", {
   headers: {
     "Accept": "text/html;text/plain;*/*",
     "X-Foo" : "bar"
   }
  });
  var resp = JSON.parse(xhr.responseText);
  T(equals(resp.headers["X-Foo"], "bar"));
  T(equals(resp.query, {foo:"bar"}));
  T(equals(resp.verb, "GET"));
  T(equals(resp.path[4], docid));
  T(equals(resp.info.db_name, "test_suite_db"));

  // returning a content-type
  xhr = CouchDB.request("GET", "/test_suite_db/_show/template/xml-type/"+docid);
  T("application/xml" == xhr.getResponseHeader("Content-Type"));
  T("Accept" == xhr.getResponseHeader("Vary"));

  // accept header switching
  // different mime has different etag

  xhr = CouchDB.request("GET", "/test_suite_db/_show/template/accept-switch/"+docid, {
   headers: {"Accept": "text/html;text/plain;*/*"}
  });
  T("text/html" == xhr.getResponseHeader("Content-Type"));
  T("Accept" == xhr.getResponseHeader("Vary"));
  var etag = xhr.getResponseHeader("etag");

  xhr = CouchDB.request("GET", "/test_suite_db/_show/template/accept-switch/"+docid, {
   headers: {"Accept": "image/png;*/*"}
  });
  T(xhr.responseText.match(/PNG/))
  T("image/png" == xhr.getResponseHeader("Content-Type"));
  var etag2 = xhr.getResponseHeader("etag");
  T(etag2 != etag);

  // proper etags
  // show with doc
  xhr = CouchDB.request("GET", "/test_suite_db/_show/template/just-name/"+docid);
  // extract the ETag header values
  etag = xhr.getResponseHeader("etag");
  // get again with etag in request
  xhr = CouchDB.request("GET", "/test_suite_db/_show/template/just-name/"+docid, {
   headers: {"if-none-match": etag}
  });
  // should be 304
  T(xhr.status == 304);    

  // update the doc
  doc.name = "Crusty";
  resp = db.save(doc);
  T(resp.ok);
  // req with same etag
  xhr = CouchDB.request("GET", "/test_suite_db/_show/template/just-name/"+docid, {
   headers: {"if-none-match": etag}
  });
  // status is 200    
  T(xhr.status == 200);

  // get new etag and request again
  etag = xhr.getResponseHeader("etag");
  xhr = CouchDB.request("GET", "/test_suite_db/_show/template/just-name/"+docid, {
   headers: {"if-none-match": etag}
  });
  // should be 304
  T(xhr.status == 304);

  // update design doc (but not function)
  designDoc.isChanged = true;
  T(db.save(designDoc).ok);

  xhr = CouchDB.request("GET", "/test_suite_db/_show/template/just-name/"+docid, {
   headers: {"if-none-match": etag}
  });
  // should be 304
  T(xhr.status == 304);

  // update design doc function
  designDoc.shows["just-name"] = (function(doc, req) {
   return {
     body : "Just old " + doc.name
   };
  }).toString();
  T(db.save(designDoc).ok);

  xhr = CouchDB.request("GET", "/test_suite_db/_show/template/just-name/"+docid, {
   headers: {"if-none-match": etag}
  });
  // status is 200    
  T(xhr.status == 200);


  // JS can't set etag
  xhr = CouchDB.request("GET", "/test_suite_db/_show/template/no-set-etag/"+docid);
  // extract the ETag header values
  etag = xhr.getResponseHeader("etag");
  T(etag != "skipped")

  // test the respondWith mime matcher
  xhr = CouchDB.request("GET", "/test_suite_db/_show/template/respondWith/"+docid, {
   headers: {
     "Accept": 'text/html,application/atom+xml; q=0.9'
   }
  });
  T(xhr.getResponseHeader("Content-Type") == "text/html");
  T(xhr.responseText == "Ha ha, you said \"plankton\".");

  // now with xml
  xhr = CouchDB.request("GET", "/test_suite_db/_show/template/respondWith/"+docid, {
   headers: {
     "Accept": 'application/xml'
   }
  });
  T(xhr.getResponseHeader("Content-Type") == "application/xml");
  T(xhr.responseText.match(/node/));
  T(xhr.responseText.match(/plankton/));

  // registering types works
  xhr = CouchDB.request("GET", "/test_suite_db/_show/template/respondWith/"+docid, {
   headers: {
     "Accept": "application/x-foo"
   }
  });
  T(xhr.getResponseHeader("Content-Type") == "application/x-foo");
  T(xhr.responseText.match(/foofoo/));

  // test the respondWith mime matcher without
  xhr = CouchDB.request("GET", "/test_suite_db/_show/template/respondWith/"+docid, {
   headers: {
     "Accept": 'text/html,application/atom+xml; q=0.9'
   }
  });
  T(xhr.getResponseHeader("Content-Type") == "text/html");
  T(xhr.responseText == "Ha ha, you said \"plankton\".");
};
