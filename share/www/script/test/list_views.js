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

couchTests.list_views = function(debug) {
  var db = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"false"});
  db.deleteDb();
  db.createDb();
  if (debug) debugger;

  var designDoc = {
    _id:"_design/lists",
    language: "javascript",
    views : {
      basicView : {
        map : stringFun(function(doc) {
          emit(doc.integer, doc.string);
        })
      },
      withReduce : {
        map : stringFun(function(doc) {
          emit(doc.integer, doc.string);
        }),
        reduce : stringFun(function(keys, values, rereduce) {
          if (rereduce) {
            return sum(values);
          } else {
            return values.length;
          }
        })
      }
    },
    lists: {
      basicBasic : stringFun(function(head, req) {
        send("head");
        var row;
        while(row = getRow()) {
          log("row: "+toJSON(row));
          send(row.key);
        };
        return "tail";
      }),
      basicJSON : stringFun(function(head, req) {
        start({"headers":{"Content-Type" : "application/json"}});
        send('{"head":'+toJSON(head)+', ');
        send('"req":'+toJSON(req)+', ');
        send('"rows":[');
        var row, sep = '';
        while (row = getRow()) {
          send(sep + toJSON(row));
          sep = ', ';
        }
        return "]}";
      }),
      simpleForm: stringFun(function(head, req) {
        log("simpleForm");
        send('<h1>Total Rows: '
              // + head.total_rows
              // + ' Offset: ' + head.offset
              + '</h1><ul>');

        // rows
        var row, row_number = 0, prevKey, firstKey = null;
        while (row = getRow()) {
          row_number += 1;
          if (!firstKey) firstKey = row.key;
          prevKey = row.key;
          send('\n<li>Key: '+row.key
          +' Value: '+row.value
          +' LineNo: '+row_number+'</li>');
        }

        // tail
        return '</ul><p>FirstKey: '+ firstKey + ' LastKey: '+ prevKey+'</p>';
      }),
      acceptSwitch: stringFun(function(head, req) {
        // respondWith takes care of setting the proper headers
        provides("html", function() {
          send("HTML <ul>");

          var row, num = 0;
          while (row = getRow()) {
            num ++;
            send('\n<li>Key: '
              +row.key+' Value: '+row.value
              +' LineNo: '+num+'</li>');
          }

          // tail
          return '</ul>';
        });

        provides("xml", function() {
          send('<feed xmlns="http://www.w3.org/2005/Atom">'
            +'<title>Test XML Feed</title>');

          while (row = getRow()) {
            var entry = new XML('<entry/>');
            entry.id = row.id;
            entry.title = row.key;
            entry.content = row.value;
            send(entry);
          }
          return "</feed>";
        });
      }),
      qsParams: stringFun(function(head, req) {
        return toJSON(req.query) + "\n";
      }),
      stopIter: stringFun(function(req) {
        send("head");
        var row, row_number = 0;
        while(row = getRow()) {
          if(row_number > 2) break;
          send(" " + row_number);
          row_number += 1;
        };
        return " tail";
      }),
      stopIter2: stringFun(function(head, req) {
        provides("html", function() {
          send("head");
          var row, row_number = 0;
          while(row = getRow()) {
            if(row_number > 2) break;
            send(" " + row_number);
            row_number += 1;
          };
          return " tail";
        });
      }),
      tooManyGetRows : stringFun(function() {
        send("head");
        var row;
        while(row = getRow()) {
          send(row.key);
        };
        getRow();
        getRow();
        getRow();
        row = getRow();
        return "after row: "+toJSON(row);
      }),
      emptyList: stringFun(function() {
        return " ";
      }),
      rowError : stringFun(function(head, req) {
        send("head");
        var row = getRow();
        send(fooBarBam); // intentional error
        return "tail";
      }),
      docReference : stringFun(function(head, req) {
        send("head");
        var row = getRow();
        send(row.doc.integer);
        return "tail";
      })
    }
  };
  var viewOnlyDesignDoc = {
    _id:"_design/views",
    language: "javascript",
    views : {
      basicView : {
        map : stringFun(function(doc) {
          emit(-doc.integer, doc.string);
        })
      }
    }
  };

  T(db.save(designDoc).ok);

  var docs = makeDocs(0, 10);
  db.bulkSave(docs);

  var view = db.view('lists/basicView');
  T(view.total_rows == 10);

  // standard get
  var xhr = CouchDB.request("GET", "/test_suite_db/_design/lists/_list/basicBasic/basicView");
  T(xhr.status == 200, "standard get should be 200");
  T(/head0123456789tail/.test(xhr.responseText));

  var xhr = CouchDB.request("GET", "/test_suite_db/_view/lists/basicView?list=basicBasic");
  T(xhr.status == 200, "standard get should be 200");
  T(/head0123456789tail/.test(xhr.responseText));

  // test that etags are available
  var etag = xhr.getResponseHeader("etag");
  xhr = CouchDB.request("GET", "/test_suite_db/_design/lists/_list/basicBasic/basicView", {
    headers: {"if-none-match": etag}
  });
  T(xhr.status == 304);

  var etag = xhr.getResponseHeader("etag");
  xhr = CouchDB.request("GET", "/test_suite_db/_view/lists/basicView?list=basicBasic", {
    headers: {"if-none-match": etag}
  });
  T(xhr.status == 304);

  // test the richness of the arguments
  xhr = CouchDB.request("GET", "/test_suite_db/_design/lists/_list/basicJSON/basicView");
  T(xhr.status == 200, "standard get should be 200");
  var resp = JSON.parse(xhr.responseText);
  TEquals(resp.head, {total_rows:10, offset:0});
  T(resp.rows.length == 10);
  TEquals(resp.rows[0], {"id": "0","key": 0,"value": "0"});

  TEquals(resp.req.info.db_name, "test_suite_db");
  TEquals(resp.req.verb, "GET");
  TEquals(resp.req.path, [
      "test_suite_db",
      "_design",
      "lists",
      "_list",
      "basicJSON",
      "basicView"
  ]);
  T(resp.req.headers.Accept);
  T(resp.req.headers.Host);
  T(resp.req.headers["User-Agent"]);
  T(resp.req.cookie);

  // get with query params
  xhr = CouchDB.request("GET", "/test_suite_db/_design/lists/_list/simpleForm/basicView?startkey=3");
  T(xhr.status == 200, "with query params");
  T(/Total Rows/.test(xhr.responseText));
  T(!(/Key: 1/.test(xhr.responseText)));
  T(/FirstKey: 3/.test(xhr.responseText));
  T(/LastKey: 9/.test(xhr.responseText));

  var xhr = CouchDB.request("GET", "/test_suite_db/_view/lists/basicView?list=simpleForm&startkey=3");
  T(xhr.status == 200, "with query params");
  T(/Total Rows/.test(xhr.responseText));
  T(!(/Key: 1/.test(xhr.responseText)));
  T(/FirstKey: 3/.test(xhr.responseText));
  T(/LastKey: 9/.test(xhr.responseText));

  // with 0 rows
  var xhr = CouchDB.request("GET", "/test_suite_db/_design/lists/_list/simpleForm/basicView?startkey=30");
  T(xhr.status == 200, "0 rows");
  T(/Total Rows/.test(xhr.responseText));

   var xhr = CouchDB.request("GET", "/test_suite_db/_view/lists/basicView?list=simpleForm&startkey=30");
  T(xhr.status == 200, "0 rows");
  T(/Total Rows/.test(xhr.responseText));

  //too many Get Rows
  var xhr = CouchDB.request("GET", "/test_suite_db/_design/lists/_list/tooManyGetRows/basicView");
  T(xhr.status == 200, "tooManyGetRows");
  T(/9after row: null/.test(xhr.responseText));


  // reduce with 0 rows
  var xhr = CouchDB.request("GET", "/test_suite_db/_design/lists/_list/simpleForm/withReduce?startkey=30");
  T(xhr.status == 200, "reduce 0 rows");
  T(/Total Rows/.test(xhr.responseText));
  T(/LastKey: undefined/.test(xhr.responseText));

  // reduce with 0 rows
  var xhr = CouchDB.request("GET", "/test_suite_db/_view/lists/withReduce?list=simpleForm&startkey=30");
  T(xhr.status == 200, "reduce 0 rows");
  T(/Total Rows/.test(xhr.responseText));
  T(/LastKey: undefined/.test(xhr.responseText));

  // when there is a reduce present, but not used
  var xhr = CouchDB.request("GET", "/test_suite_db/_design/lists/_list/simpleForm/withReduce?reduce=false");
  T(xhr.status == 200, "reduce false");
  T(/Total Rows/.test(xhr.responseText));
  T(/Key: 1/.test(xhr.responseText));


  // when there is a reduce present, and used
  xhr = CouchDB.request("GET", "/test_suite_db/_design/lists/_list/simpleForm/withReduce?group=true");
  T(xhr.status == 200, "group reduce");
  T(/Key: 1/.test(xhr.responseText));

  // there should be etags on reduce as well
  var etag = xhr.getResponseHeader("etag");
  T(etag, "Etags should be served with reduce lists");
  xhr = CouchDB.request("GET", "/test_suite_db/_design/lists/_list/simpleForm/withReduce?group=true", {
    headers: {"if-none-match": etag}
  });
  T(xhr.status == 304);

  // verify the etags expire correctly
  var docs = makeDocs(11, 12);
  db.bulkSave(docs);

  xhr = CouchDB.request("GET", "/test_suite_db/_design/lists/_list/simpleForm/withReduce?group=true", {
    headers: {"if-none-match": etag}
  });
  T(xhr.status == 200, "reduce etag");

  // empty list
  var xhr = CouchDB.request("GET", "/test_suite_db/_design/lists/_list/emptyList/basicView");
  T(xhr.responseText.match(/^ $/));
  xhr = CouchDB.request("GET", "/test_suite_db/_design/lists/_list/emptyList/withReduce?group=true");
  T(xhr.responseText.match(/^ $/));

  // multi-key fetch
  var xhr = CouchDB.request("POST", "/test_suite_db/_design/lists/_list/simpleForm/basicView", {
    body: '{"keys":[2,4,5,7]}'
  });
  T(xhr.status == 200, "multi key");
  T(/Total Rows/.test(xhr.responseText));
  T(!(/Key: 1/.test(xhr.responseText)));
  T(/Key: 2/.test(xhr.responseText));
  T(/FirstKey: 2/.test(xhr.responseText));
  T(/LastKey: 7/.test(xhr.responseText));

  // no multi-key fetch allowed when group=false
  xhr = CouchDB.request("POST", "/test_suite_db/_design/lists/_list/simpleForm/withReduce?group=false", {
    body: '{"keys":[2,4,5,7]}'
  });
  T(xhr.status == 400);
  T(/query_parse_error/.test(xhr.responseText));

  var xhr = CouchDB.request("GET", "/test_suite_db/_design/lists/_list/rowError/basicView");
  T(/ReferenceError/.test(xhr.responseText));


  // with include_docs and a reference to the doc.
  var xhr = CouchDB.request("GET", "/test_suite_db/_design/lists/_list/docReference/basicView?include_docs=true");
  T(xhr.responseText.match(/head0tail/));

  // now with extra qs params
  var xhr = CouchDB.request("GET", "/test_suite_db/_design/lists/_list/qsParams/basicView?foo=blam");
  T(xhr.responseText.match(/blam/));

  var xhr = CouchDB.request("GET", "/test_suite_db/_design/lists/_list/stopIter/basicView");
  // T(xhr.getResponseHeader("Content-Type") == "text/plain");
  T(xhr.responseText.match(/^head 0 1 2 tail$/) && "basic stop");

  xhr = CouchDB.request("GET", "/test_suite_db/_design/lists/_list/stopIter2/basicView");
  T(xhr.responseText.match(/^head 0 1 2 tail$/) && "stop 2");

  // aborting iteration with reduce
  var xhr = CouchDB.request("GET", "/test_suite_db/_design/lists/_list/stopIter/withReduce?group=true");
  T(xhr.responseText.match(/^head 0 1 2 tail$/) && "reduce stop");
  xhr = CouchDB.request("GET", "/test_suite_db/_design/lists/_list/stopIter2/withReduce?group=true");
  T(xhr.responseText.match(/^head 0 1 2 tail$/) && "reduce stop 2");

  // with accept headers for HTML
  xhr = CouchDB.request("GET", "/test_suite_db/_design/lists/_list/acceptSwitch/basicView", {
    headers: {
      "Accept": 'text/html'
    }
  });
  T(xhr.getResponseHeader("Content-Type") == "text/html; charset=utf-8");
  T(xhr.responseText.match(/HTML/));
  T(xhr.responseText.match(/Value/));

  // now with xml
  xhr = CouchDB.request("GET", "/test_suite_db/_design/lists/_list/acceptSwitch/basicView", {
    headers: {
      "Accept": 'application/xml'
    }
  });
  T(xhr.getResponseHeader("Content-Type") == "application/xml");
  T(xhr.responseText.match(/XML/));
  T(xhr.responseText.match(/entry/));

  // Test we can run lists and views from separate docs.
  T(db.save(viewOnlyDesignDoc).ok);
  xhr = CouchDB.request("GET", "/test_suite_db/_design/lists/_list/simpleForm/views/basicView?startkey=-3");
  T(xhr.status == 200, "with query params");
  T(/Total Rows/.test(xhr.responseText));
  T(!(/Key: -4/.test(xhr.responseText)));
  T(/FirstKey: -3/.test(xhr.responseText));
  T(/LastKey: 0/.test(xhr.responseText));
};
