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
  return console.log('TODO: test not yet implemented');
  if (debug) debugger;
  var dbNames = ["test_suite_db", "test_suite_db/with_slashes"];
  for (var i=0; i < dbNames.length; i++) {
    var db = new CouchDB(dbNames[i]);
    var dbName = encodeURIComponent(dbNames[i]);
    db.deleteDb();
    db.createDb();

    var designDoc = {
      _id:"_design/test",
      language: "javascript",
      _attachments:{
        "foo.txt": {
          content_type:"text/plain",
          data: "VGhpcyBpcyBhIGJhc2U2NCBlbmNvZGVkIHRleHQ="
        }
      },
      rewrites: stringFun(function(req) {
        prefix = req.path[4];
        if (prefix === 'foo') {
            return 'foo.txt';
        }
        if (prefix === 'foo2') {
            return {path: 'foo.txt', method: 'GET'};
        }
        if (prefix === 'hello') {
            if (req.method != 'PUT') {
                return
            }
            id = req.path[5];
            return {path: '_update/hello/' + id};
        }
        if (prefix === 'welcome') {
            if (req.path.length == 6){
                name = req.path[5];
                return {path: '_show/welcome', query: {'name': name}};
            }
            return '_show/welcome';
        }
        if (prefix === 'welcome2') {
            return {path: '_show/welcome', query: {'name': 'user'}};
        }
        if (prefix === 'welcome3') {
            name = req.path[5];
            if (req.method == 'PUT') {
                path = '_update/welcome2/' + name;
            } else if (req.method == 'GET') {
                path = '_show/welcome2/' + name;
            } else {
                return;
            }
            return path;
        }
        if (prefix === 'welcome4') {
            return {path: '_show/welcome3',  query: {name: req.path[5]}};
        }
        if (prefix === 'welcome5') {
            rest = req.path.slice(5).join('/');
            return {path: '_show/' + rest,  query: {name: rest}};
        }
        if (prefix === 'basicView') {
            rest = req.path.slice(5).join('/');
            return {path: '_view/basicView'};
        }
        if (req.path.slice(4).join('/') === 'simpleForm/basicView') {
            return {path: '_list/simpleForm/basicView'};
        }
        if (req.path.slice(4).join('/') === 'simpleForm/basicViewFixed') {
            return {path: '_list/simpleForm/basicView',
                    query: {startkey: '"3"', endkey: '"8"'}};
        }
        if (req.path.slice(4).join('/') === 'simpleForm/complexView') {
            return {path: '_list/simpleForm/complexView',
                    query: {key: JSON.stringify([1,2])}};
        }
        if (req.path.slice(4).join('/') === 'simpleForm/complexView2') {
            return {path: '_list/simpleForm/complexView',
                    query: {key: JSON.stringify(['test', {}])}};
        }
        if (req.path.slice(4).join('/') === 'simpleForm/complexView3') {
            return {path: '_list/simpleForm/complexView',
                    query: {key: JSON.stringify(['test', ['test', 'essai']])}};
        }
        if (req.path.slice(4).join('/') === 'simpleForm/complexView4') {
            return {path: '_list/simpleForm/complexView2',
                    query: {key: JSON.stringify({"c": 1})}};
        }
        if (req.path.slice(4).join('/') === 'simpleForm/complexView4') {
            return {path: '_list/simpleForm/complexView2',
                    query: {key: JSON.stringify({"c": 1})}};
        }
        if (req.path.slice(4).join('/') === '/') {
            return {path: '_view/basicView'};
        }
        if (prefix === 'db') {
            return {path: '../../' + req.path.slice(5).join('/')};
        }
      }),
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

    req = CouchDB.request("GET", "/"+dbName+"/_design/test/_rewrite/foo");
    T(req.responseText == "This is a base64 encoded text");
    T(req.getResponseHeader("Content-Type") == "text/plain");

    req = CouchDB.request("GET", "/"+dbName+"/_design/test/_rewrite/foo2");
    T(req.responseText == "This is a base64 encoded text");
    T(req.getResponseHeader("Content-Type") == "text/plain");


    // test POST
    // hello update world

    var doc = {"word":"plankton", "name":"Rusty"}
    var resp = db.save(doc);
    T(resp.ok);
    var docid = resp.id;

    xhr = CouchDB.request("PUT", "/"+dbName+"/_design/test/_rewrite/hello/"+docid);
    T(xhr.status == 201);
    T(xhr.responseText == "hello doc");
    T(/charset=utf-8/.test(xhr.getResponseHeader("Content-Type")))

    doc = db.open(docid);
    T(doc.world == "hello");

    req = CouchDB.request("GET", "/"+dbName+"/_design/test/_rewrite/welcome?name=user");
    T(req.responseText == "Welcome user");

    req = CouchDB.request("GET", "/"+dbName+"/_design/test/_rewrite/welcome/user");
    T(req.responseText == "Welcome user");

    req = CouchDB.request("GET", "/"+dbName+"/_design/test/_rewrite/welcome2");
    T(req.responseText == "Welcome user");

    xhr = CouchDB.request("PUT", "/"+dbName+"/_design/test/_rewrite/welcome3/test");
    T(xhr.status == 201);
    T(xhr.responseText == "New World");
    T(/charset=utf-8/.test(xhr.getResponseHeader("Content-Type")));

    xhr = CouchDB.request("GET", "/"+dbName+"/_design/test/_rewrite/welcome3/test");
    T(xhr.responseText == "Welcome test");

    req = CouchDB.request("GET", "/"+dbName+"/_design/test/_rewrite/welcome4/user");
    T(req.responseText == "Welcome user");

    req = CouchDB.request("GET", "/"+dbName+"/_design/test/_rewrite/welcome5/welcome3");
    T(req.responseText == "Welcome welcome3");

    xhr = CouchDB.request("GET", "/"+dbName+"/_design/test/_rewrite/basicView");
    T(xhr.status == 200, "view call");
    T(/{"total_rows":9/.test(xhr.responseText));

    xhr = CouchDB.request("GET", "/"+dbName+"/_design/test/_rewrite/simpleForm/complexView");
    T(xhr.status == 200, "with query params");
    T(/FirstKey: [1, 2]/.test(xhr.responseText));

    xhr = CouchDB.request("GET", "/"+dbName+"/_design/test/_rewrite/simpleForm/complexView2");
    T(xhr.status == 200, "with query params");
    T(/Value: doc 3/.test(xhr.responseText));

    xhr = CouchDB.request("GET", "/"+dbName+"/_design/test/_rewrite/simpleForm/complexView3");
    T(xhr.status == 200, "with query params");
    T(/Value: doc 4/.test(xhr.responseText));

    xhr = CouchDB.request("GET", "/"+dbName+"/_design/test/_rewrite/simpleForm/complexView4");
    T(xhr.status == 200, "with query params");
    T(/Value: doc 5/.test(xhr.responseText));

    // COUCHDB-2031 - path normalization versus qs params
    xhr = CouchDB.request("GET", "/"+dbName+"/_design/test/_rewrite/db/_design/test?meta=true");
    T(xhr.status == 200, "path normalization works with qs params");
    var result = JSON.parse(xhr.responseText);
    T(result['_id'] == "_design/test");
    T(typeof(result['_revs_info']) === "object");

    // test early response
    var ddoc = {
      _id: "_design/response",
      rewrites: stringFun(function(req){
        status = parseInt(req.query.status);
        return {code: status,
                body: JSON.stringify({"status": status}),
                headers: {'x-foo': 'bar', 'Content-Type': 'application/json'}};
      })
    }
    T(db.save(ddoc).ok);
    var xhr = CouchDB.request("GET", "/"+dbName+"/_design/response/_rewrite?status=200");
    T(xhr.status == 200);
    T(xhr.headers['x-foo'] == 'bar');
    T(xhr.responseText == '{"status":200}');
    var xhr = CouchDB.request("GET", "/"+dbName+"/_design/response/_rewrite?status=451");
    T(xhr.status == 451);
    T(xhr.headers['Content-Type'] == 'application/json');
    var xhr = CouchDB.request("GET", "/"+dbName+"/_design/response/_rewrite?status=600");
    T(xhr.status == 500);


    // test path relative to server
    var ddoc = {
      _id: "_design/relative",
      rewrites: stringFun(function(req){
        return '../../../_uuids'
      })
    }
    T(db.save(ddoc).ok);
    var xhr = CouchDB.request("GET", "/"+dbName+"/_design/relative/_rewrite/uuids");
    T(xhr.status == 200);
    var result = JSON.parse(xhr.responseText);
    T(result.uuids.length == 1);

    // test loop
    var ddoc_loop = {
      _id: "_design/loop",
      rewrites: stringFun(function(req) {
        return '_rewrite/loop';
      })
    };
    db.save(ddoc_loop);
    var url = "/"+dbName+"/_design/loop/_rewrite/loop";
    var xhr = CouchDB.request("GET", url);
    TEquals(400, xhr.status);
  }
}
