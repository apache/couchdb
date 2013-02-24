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
              "from": "/welcome2",
              "to": "_show/welcome",
              "query": {
                "name": "user"
              }
            },
            {
              "from": "/welcome3/:name",
              "to": "_update/welcome2/:name",
              "method": "PUT"
            },
            {
              "from": "/welcome3/:name",
              "to": "_show/welcome2/:name",
              "method": "GET"
            },
            {
             "from": "/welcome4/*",
             "to" : "_show/welcome3",
             "query": {
               "name": "*"
             }
            },
            {
             "from": "/welcome5/*",
             "to" : "_show/*",
             "query": {
               "name": "*"
             }
            },
            {
              "from": "basicView",
              "to": "_view/basicView",
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
              },
              "formats": {
                "start": "int",
                "end": "int"
              }
            },
            {
              "from": "simpleForm/complexView",
              "to": "_list/simpleForm/complexView",
              "query": {
                "key": [1, 2]
              }
            },
            {
              "from": "simpleForm/complexView2",
              "to": "_list/simpleForm/complexView",
              "query": {
                "key": ["test", {}]
              }
            },
            {
              "from": "simpleForm/complexView3",
              "to": "_list/simpleForm/complexView",
              "query": {
                "key": ["test", ["test", "essai"]]
              }
            },
            {
              "from": "simpleForm/complexView4",
              "to": "_list/simpleForm/complexView2",
              "query": {
                "key": {"c": 1}
              }
            },
            {
              "from": "simpleForm/complexView5/:a/:b",
              "to": "_list/simpleForm/complexView3",
              "query": {
                "key": [":a", ":b"]
              }
            },
            {
              "from": "simpleForm/complexView6",
              "to": "_list/simpleForm/complexView3",
              "query": {
                "key": [":a", ":b"]
              }
            },
            {
              "from": "simpleForm/complexView7/:a/:b",
              "to": "_view/complexView3",
              "query": {
                "key": [":a", ":b"],
                "include_docs": ":doc"
              },
              "format": {
                "doc": "bool"
              }

            },
            {
              "from": "/",
              "to": "_view/basicView",
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
            }),
          },
          shows: {
            "welcome": stringFun(function(doc,req) {
              return "Welcome " + req.query["name"];
            }),
            "welcome2": stringFun(function(doc, req) {
              return "Welcome " + doc.name;
            }),
            "welcome3": stringFun(function(doc,req) {
              return "Welcome " + req.query["name"];
            })
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
                if (doc.integer) {
                  emit(doc.integer, doc.string);
                }
                
              })
            },
            complexView: {
              map: stringFun(function(doc) {
                if (doc.type == "complex") {
                  emit([doc.a, doc.b], doc.string);
                }
              })
            },
            complexView2: {
              map: stringFun(function(doc) {
                if (doc.type == "complex") {
                  emit(doc.a, doc.string);
                }
              })
            },
            complexView3: {
              map: stringFun(function(doc) {
                if (doc.type == "complex") {
                  emit(doc.b, doc.string);
                }
              })
            }
          }
        }
 
        db.save(designDoc);
        
        var docs = makeDocs(0, 10);
        db.bulkSave(docs);

        var docs2 = [
          {"a": 1, "b": 1, "string": "doc 1", "type": "complex"},
          {"a": 1, "b": 2, "string": "doc 2", "type": "complex"},
          {"a": "test", "b": {}, "string": "doc 3", "type": "complex"},
          {"a": "test", "b": ["test", "essai"], "string": "doc 4", "type": "complex"},
          {"a": {"c": 1}, "b": "", "string": "doc 5", "type": "complex"}
        ];

        db.bulkSave(docs2);

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
        
        req = CouchDB.request("GET", "/test_suite_db/_design/test/_rewrite/welcome?name=user");
        T(req.responseText == "Welcome user");
        
        req = CouchDB.request("GET", "/test_suite_db/_design/test/_rewrite/welcome/user");
        T(req.responseText == "Welcome user");
        
        req = CouchDB.request("GET", "/test_suite_db/_design/test/_rewrite/welcome2");
        T(req.responseText == "Welcome user");
        
        xhr = CouchDB.request("PUT", "/test_suite_db/_design/test/_rewrite/welcome3/test");
        T(xhr.status == 201);
        T(xhr.responseText == "New World");
        T(/charset=utf-8/.test(xhr.getResponseHeader("Content-Type")));
        
        xhr = CouchDB.request("GET", "/test_suite_db/_design/test/_rewrite/welcome3/test");
        T(xhr.responseText == "Welcome test");

        req = CouchDB.request("GET", "/test_suite_db/_design/test/_rewrite/welcome4/user");
        T(req.responseText == "Welcome user");

        req = CouchDB.request("GET", "/test_suite_db/_design/test/_rewrite/welcome5/welcome3");
        T(req.responseText == "Welcome welcome3");
       
        xhr = CouchDB.request("GET", "/test_suite_db/_design/test/_rewrite/basicView");
        T(xhr.status == 200, "view call");
        T(/{"total_rows":9/.test(xhr.responseText)); 

        xhr = CouchDB.request("GET", "/test_suite_db/_design/test/_rewrite/");
        T(xhr.status == 200, "view call");
        T(/{"total_rows":9/.test(xhr.responseText)); 

        
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

        // get with query params        
        xhr = CouchDB.request("GET", "/test_suite_db/_design/test/_rewrite/simpleForm/complexView");
        T(xhr.status == 200, "with query params");
        T(/FirstKey: [1, 2]/.test(xhr.responseText));
        
        xhr = CouchDB.request("GET", "/test_suite_db/_design/test/_rewrite/simpleForm/complexView2");
        T(xhr.status == 200, "with query params");
        T(/Value: doc 3/.test(xhr.responseText));
        
        xhr = CouchDB.request("GET", "/test_suite_db/_design/test/_rewrite/simpleForm/complexView3");
        T(xhr.status == 200, "with query params");
        T(/Value: doc 4/.test(xhr.responseText));
        
        xhr = CouchDB.request("GET", "/test_suite_db/_design/test/_rewrite/simpleForm/complexView4");
        T(xhr.status == 200, "with query params");
        T(/Value: doc 5/.test(xhr.responseText));
        
        xhr = CouchDB.request("GET", "/test_suite_db/_design/test/_rewrite/simpleForm/complexView5/test/essai");
        T(xhr.status == 200, "with query params");
        T(/Value: doc 4/.test(xhr.responseText));
        
        xhr = CouchDB.request("GET", "/test_suite_db/_design/test/_rewrite/simpleForm/complexView6?a=test&b=essai");
        T(xhr.status == 200, "with query params");
        T(/Value: doc 4/.test(xhr.responseText));

        xhr = CouchDB.request("GET", "/test_suite_db/_design/test/_rewrite/simpleForm/complexView7/test/essai?doc=true");
        T(xhr.status == 200, "with query params");
        var result = JSON.parse(xhr.responseText);
        T(typeof(result.rows[0].doc) === "object");
        
        // test path relative to server
        designDoc.rewrites.push({
           "from": "uuids",
           "to": "../../../_uuids"
        });
        T(db.save(designDoc).ok);
        
        var xhr = CouchDB.request("GET", "/test_suite_db/_design/test/_rewrite/uuids");
        T(xhr.status == 500);
        var result = JSON.parse(xhr.responseText);
        T(result.error == "insecure_rewrite_rule");

        run_on_modified_server(
          [{section: "httpd",
            key: "secure_rewrites",
            value: "false"}],
            function() {
              var xhr = CouchDB.request("GET", "/test_suite_db/_design/test/_rewrite/uuids?cache=bust");
              T(xhr.status == 200);
              var result = JSON.parse(xhr.responseText);
              T(result.uuids.length == 1);
              var first = result.uuids[0];
        });
  });

  // test invalid rewrites
  // string
  var ddoc = {
    _id: "_design/invalid",
    rewrites: "[{\"from\":\"foo\",\"to\":\"bar\"}]"
  }
  db.save(ddoc);
  var res = CouchDB.request("GET", "/test_suite_db/_design/invalid/_rewrite/foo");
  TEquals(400, res.status, "should return 400");

  var ddoc_requested_path = {
    _id: "_design/requested_path",
    rewrites:[
        {"from": "show", "to": "_show/origin/0"},
        {"from": "show_rewritten", "to": "_rewrite/show"}
    ],
    shows: {
        origin: stringFun(function(doc, req) {
            return req.headers["x-couchdb-requested-path"];
    })}
  };

  db.save(ddoc_requested_path);
  var url = "/test_suite_db/_design/requested_path/_rewrite/show";
  var res = CouchDB.request("GET", url);
  TEquals(url, res.responseText, "should return the original url");

  var url = "/test_suite_db/_design/requested_path/_rewrite/show_rewritten";
  var res = CouchDB.request("GET", url);
  TEquals(url, res.responseText, "returned the original url");

  var ddoc_loop = {
    _id: "_design/loop",
    rewrites: [{ "from": "loop",  "to": "_rewrite/loop"}]
  };
  db.save(ddoc_loop);

  // Assert loop detection
  run_on_modified_server(
    [{section: "httpd",
      key: "rewrite_limit",
      value: "2"}],
      function(){
        var url = "/test_suite_db/_design/loop/_rewrite/loop";
        var xhr = CouchDB.request("GET", url);
        TEquals(400, xhr.status);
  });

  // Assert serial execution is not spuriously counted as loop
  run_on_modified_server(
    [{section: "httpd",
      key: "rewrite_limit",
      value: "2"},
     {section: "httpd",
      key: "secure_rewrites",
      value: "false"}],
    function(){
      var url = "/test_suite_db/_design/test/_rewrite/foo";
      for (var i=0; i < 5; i++) {
          var xhr = CouchDB.request("GET", url);
          TEquals(200, xhr.status);
      }
  });
}
