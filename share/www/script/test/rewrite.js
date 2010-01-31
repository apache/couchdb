// Licensed under the Apache License, Version 2.0 (the "License"); you may not
// use this file except in compliance with the License. You may obtain a copy of
// the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
// WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
// License for the specific language governing permissions and limitations under
// the License.
 
 
 
couchTests.rewrite = function(debug) {
  // this test _rewrite handler
  
  
  var db = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"false"});
  db.deleteDb();
  db.createDb();
  
  
  if (debug) debugger;
  run_on_modified_server(
    [{section: "httpd",
      key: "authentication_handlers",
      value: "{couch_httpd_auth, special_test_authentication_handler}"},
     {section:"httpd",
      key: "WWW-Authenticate",
      value: "X-Couch-Test-Auth"}],
      
      function(){
        var designDoc = {
          _id:"_design/test",
          language: "javascript",
           _attachments:{
              "foo.txt": {
                content_type:"text/plain",
                data: "VGhpcyBpcyBhIGJhc2U2NCBlbmNvZGVkIHRleHQ="
              }
            },
          rewrites: [
            {
              "from": "foo",
              "to": "foo.txt"
            },
            {
              "from": "foo2",
              "to": "foo.txt",
              "method": "GET"
            },
            {
              "from": "hello/:id",
              "to": "_update/hello/:id",
              "method": "PUT"
            },
            {
              "from": "/welcome",
              "to": "_show/welcome"
            },
            {
              "from": "/welcome/:name",
              "to": "_show/welcome",
              "query": {
                "name": ":name"
              }
            },
            {
              "from": "/welcome2/:name",
              "to": "_update/welcome2/:name",
              "method": "PUT"
            },
            {
              "from": "/welcome2/:name",
              "to": "_show/welcome2/:name",
              "method": "GET"
            },
            {
              "from": "simpleForm/basicView",
              "to": "_list/simpleForm/basicView",
            },
            {
              "from": "simpleForm/basicViewFixed",
              "to": "_list/simpleForm/basicView",
              "query": {
                "startkey": 3,
                "endkey": 8
              }
            },
            {
              "from": "simpleForm/basicViewPath/:start/:end",
              "to": "_list/simpleForm/basicView",
              "query": {
                "startkey": ":start",
                "endkey": ":end"
              }
            }
            
          ],
          lists: {
            simpleForm: stringFun(function(head, req) {
              log("simpleForm");
              send('<ul>');
              var row, row_number = 0, prevKey, firstKey = null;
              while (row = getRow()) {
                row_number += 1;
                if (!firstKey) firstKey = row.key;
                prevKey = row.key;
                send('\n<li>Key: '+row.key
                +' Value: '+row.value
                +' LineNo: '+row_number+'</li>');
              }
              return '</ul><p>FirstKey: '+ firstKey + ' LastKey: '+ prevKey+'</p>';
            })
          },
          shows: {
            "welcome": stringFun(function(doc,req) {
              return "Welcome " + req.query["name"];
            }),
            "welcome2": stringFun(function(doc, req) {
              return "Welcome " + doc.name;
            }),
          },
          updates: {
            "hello" : stringFun(function(doc, req) {
              if (!doc) {
                if (req.id) {
                  return [{
                    _id : req.id
                  }, "New World"]
                }
                return [null, "Empty World"];
              }
              doc.world = "hello";
              doc.edited_by = req.userCtx;
              return [doc, "hello doc"];
            }),
            "welcome2": stringFun(function(doc, req) {
              if (!doc) {
                if (req.id) {
                  return [{
                    _id: req.id,
                    name: req.id
                  }, "New World"]
                }
                return [null, "Empty World"];
              }
              return [doc, "hello doc"];
            })
          },
          views : {
            basicView : {
              map : stringFun(function(doc) {
                emit(doc.integer, doc.string);
              })
            }
          }
        }
 
        db.save(designDoc);
        
        var docs = makeDocs(0, 10);
        db.bulkSave(docs);
 
        // test simple rewriting
 
        req = CouchDB.request("GET", "/test_suite_db/_design/test/_rewrite/foo");
        T(req.responseText == "This is a base64 encoded text");
        T(req.getResponseHeader("Content-Type") == "text/plain");
        
        req = CouchDB.request("GET", "/test_suite_db/_design/test/_rewrite/foo2");
        T(req.responseText == "This is a base64 encoded text");
        T(req.getResponseHeader("Content-Type") == "text/plain");
        
       
        // test POST
        // hello update world
        
        var doc = {"word":"plankton", "name":"Rusty"}
        var resp = db.save(doc);
        T(resp.ok);
        var docid = resp.id;
        
        xhr = CouchDB.request("PUT", "/test_suite_db/_design/test/_rewrite/hello/"+docid);
        T(xhr.status == 201);
        T(xhr.responseText == "hello doc");
        T(/charset=utf-8/.test(xhr.getResponseHeader("Content-Type")))
 
        doc = db.open(docid);
        T(doc.world == "hello");
        
        xhr = CouchDB.request("PUT", "/test_suite_db/_design/test/_rewrite/welcome2/test");
        T(xhr.status == 201);
        T(xhr.responseText == "New World");
        T(/charset=utf-8/.test(xhr.getResponseHeader("Content-Type")));
        
        xhr = CouchDB.request("GET", "/test_suite_db/_design/test/_rewrite/welcome2/test");
        T(xhr.responseText == "Welcome test");
        
        req = CouchDB.request("GET", "/test_suite_db/_design/test/_rewrite/welcome?name=user");
        T(req.responseText == "Welcome user");
        
        req = CouchDB.request("GET", "/test_suite_db/_design/test/_rewrite/welcome/user");
        T(req.responseText == "Welcome user");
        
        
        
        
        // get with query params
        xhr = CouchDB.request("GET", "/test_suite_db/_design/test/_rewrite/simpleForm/basicView?startkey=3&endkey=8");
        T(xhr.status == 200, "with query params");
        T(!(/Key: 1/.test(xhr.responseText)));
        T(/FirstKey: 3/.test(xhr.responseText));
        T(/LastKey: 8/.test(xhr.responseText));
        
        xhr = CouchDB.request("GET", "/test_suite_db/_design/test/_rewrite/simpleForm/basicViewFixed");
        T(xhr.status == 200, "with query params");
        T(!(/Key: 1/.test(xhr.responseText)));
        T(/FirstKey: 3/.test(xhr.responseText));
        T(/LastKey: 8/.test(xhr.responseText));
        
        // get with query params
        xhr = CouchDB.request("GET", "/test_suite_db/_design/test/_rewrite/simpleForm/basicViewFixed?startkey=4");
        T(xhr.status == 200, "with query params");
        T(!(/Key: 1/.test(xhr.responseText)));
        T(/FirstKey: 3/.test(xhr.responseText));
        T(/LastKey: 8/.test(xhr.responseText));
        
        // get with query params
        xhr = CouchDB.request("GET", "/test_suite_db/_design/test/_rewrite/simpleForm/basicViewPath/3/8");
        T(xhr.status == 200, "with query params");
        T(!(/Key: 1/.test(xhr.responseText)));
        T(/FirstKey: 3/.test(xhr.responseText));
        T(/LastKey: 8/.test(xhr.responseText));
        
        
        
  });
  
}