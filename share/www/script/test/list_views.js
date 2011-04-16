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
      }),
      secObj: stringFun(function(head, req) {
        return toJSON(req.secObj);
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
  var erlListDoc = {
    _id: "_design/erlang",
    language: "erlang",
    lists: {
        simple:
            'fun(Head, {Req}) -> ' +
            '  Send(<<"[">>), ' +
            '  Fun = fun({Row}, Sep) -> ' +
            '    Val = couch_util:get_value(<<"key">>, Row, 23), ' +
            '    Send(list_to_binary(Sep ++ integer_to_list(Val))), ' +
            '    {ok, ","} ' +
            '  end, ' +
            '  {ok, _} = FoldRows(Fun, ""), ' +
            '  Send(<<"]">>) ' +
            'end.'
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


  // test that etags are available
  var etag = xhr.getResponseHeader("etag");
  xhr = CouchDB.request("GET", "/test_suite_db/_design/lists/_list/basicBasic/basicView", {
    headers: {"if-none-match": etag}
  });
  T(xhr.status == 304);
  
  // confirm ETag changes with different POST bodies
  xhr = CouchDB.request("POST", "/test_suite_db/_design/lists/_list/basicBasic/basicView",
    {body: JSON.stringify({keys:[1]})}
  );
  var etag1 = xhr.getResponseHeader("etag");
  xhr = CouchDB.request("POST", "/test_suite_db/_design/lists/_list/basicBasic/basicView",
    {body: JSON.stringify({keys:[2]})}
  );
  var etag2 = xhr.getResponseHeader("etag");
  T(etag1 != etag2, "POST to map _list generates key-depdendent ETags");

  // test the richness of the arguments
  xhr = CouchDB.request("GET", "/test_suite_db/_design/lists/_list/basicJSON/basicView?update_seq=true");
  T(xhr.status == 200, "standard get should be 200");
  var resp = JSON.parse(xhr.responseText);
  TEquals(10, resp.head.total_rows);
  TEquals(0, resp.head.offset);
  TEquals(11, resp.head.update_seq);
  
  T(resp.rows.length == 10);
  TEquals(resp.rows[0], {"id": "0","key": 0,"value": "0"});

  TEquals(resp.req.info.db_name, "test_suite_db");
  TEquals(resp.req.method, "GET");
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
  xhr = CouchDB.request("GET", "/test_suite_db/_design/lists/_list/simpleForm/basicView?startkey=3&endkey=8");
  T(xhr.status == 200, "with query params");
  T(!(/Key: 1/.test(xhr.responseText)));
  T(/FirstKey: 3/.test(xhr.responseText));
  T(/LastKey: 8/.test(xhr.responseText));

  // with 0 rows
  var xhr = CouchDB.request("GET", "/test_suite_db/_design/lists/_list/simpleForm/basicView?startkey=30");
  T(xhr.status == 200, "0 rows");
  T(/<\/ul>/.test(xhr.responseText));

  //too many Get Rows
  var xhr = CouchDB.request("GET", "/test_suite_db/_design/lists/_list/tooManyGetRows/basicView");
  T(xhr.status == 200, "tooManyGetRows");
  T(/9after row: null/.test(xhr.responseText));


  // reduce with 0 rows
  var xhr = CouchDB.request("GET", "/test_suite_db/_design/lists/_list/simpleForm/withReduce?startkey=30");
  T(xhr.status == 200, "reduce 0 rows");
  T(/LastKey: undefined/.test(xhr.responseText));

  // when there is a reduce present, but not used
  var xhr = CouchDB.request("GET", "/test_suite_db/_design/lists/_list/simpleForm/withReduce?reduce=false");
  T(xhr.status == 200, "reduce false");
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

  // confirm ETag changes with different POST bodies
  xhr = CouchDB.request("POST", "/test_suite_db/_design/lists/_list/simpleForm/withReduce?group=true",
    {body: JSON.stringify({keys:[1]})}
  );
  var etag1 = xhr.getResponseHeader("etag");
  xhr = CouchDB.request("POST", "/test_suite_db/_design/lists/_list/simpleForm/withReduce?group=true",
    {body: JSON.stringify({keys:[2]})}
  );
  var etag2 = xhr.getResponseHeader("etag");
  T(etag1 != etag2, "POST to reduce _list generates key-depdendent ETags");

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
  T(!(/Key: 1 /.test(xhr.responseText)));
  T(/Key: 2/.test(xhr.responseText));
  T(/FirstKey: 2/.test(xhr.responseText));
  T(/LastKey: 7/.test(xhr.responseText));

  // multi-key fetch with GET
  var xhr = CouchDB.request("GET", "/test_suite_db/_design/lists/_list/simpleForm/basicView" +
    "?keys=[2,4,5,7]");

  T(xhr.status == 200, "multi key");
  T(!(/Key: 1 /.test(xhr.responseText)));
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

  xhr = CouchDB.request("GET", "/test_suite_db/_design/lists/_list/stopIter2/basicView", {
    headers : {
      "Accept" : "text/html"
    }
  });
  T(xhr.responseText.match(/^head 0 1 2 tail$/) && "stop 2");

  // aborting iteration with reduce
  var xhr = CouchDB.request("GET", "/test_suite_db/_design/lists/_list/stopIter/withReduce?group=true");
  T(xhr.responseText.match(/^head 0 1 2 tail$/) && "reduce stop");
  xhr = CouchDB.request("GET", "/test_suite_db/_design/lists/_list/stopIter2/withReduce?group=true", {
    headers : {
      "Accept" : "text/html"
    }
  });
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
  var url = "/test_suite_db/_design/lists/_list/simpleForm/views/basicView" +
                "?startkey=-3";
  xhr = CouchDB.request("GET", url); 
  T(xhr.status == 200, "multiple design docs.");
  T(!(/Key: -4/.test(xhr.responseText)));
  T(/FirstKey: -3/.test(xhr.responseText));
  T(/LastKey: 0/.test(xhr.responseText));

  // Test we do multi-key requests on lists and views in separate docs.
  var url = "/test_suite_db/_design/lists/_list/simpleForm/views/basicView";
  xhr = CouchDB.request("POST", url, {
    body: '{"keys":[-2,-4,-5,-7]}'
  });
  
  T(xhr.status == 200, "multi key separate docs");
  T(!(/Key: -3/.test(xhr.responseText)));
  T(/Key: -7/.test(xhr.responseText));
  T(/FirstKey: -2/.test(xhr.responseText));
  T(/LastKey: -7/.test(xhr.responseText));

    // Test if secObj is available
  var xhr = CouchDB.request("GET", "/test_suite_db/_design/lists/_list/secObj/basicView");
  T(xhr.status == 200, "standard get should be 200");
  var resp = JSON.parse(xhr.responseText);
  T(typeof(resp) == "object");

  var erlViewTest = function() {
    T(db.save(erlListDoc).ok);
    var url = "/test_suite_db/_design/erlang/_list/simple/views/basicView" +
                "?startkey=-3";
    xhr = CouchDB.request("GET", url);
    T(xhr.status == 200, "multiple languages in design docs.");
    var list = JSON.parse(xhr.responseText);
    T(list.length == 4);
    for(var i = 0; i < list.length; i++)
    {
        T(list[i] + 3 == i);
    }
  };

  run_on_modified_server([{
    section: "native_query_servers",
    key: "erlang",
    value: "{couch_native_process, start_link, []}"
  }], erlViewTest);

  // COUCHDB-1113
  var ddoc = {
    _id: "_design/test",
    views: {
      me: {
        map: (function(doc) { emit(null,null)}).toString()
      }
    },
    lists: {
      you: (function(head, req) {
        var row;
        while(row = getRow()) {
          send(row);
        }
      }).toString()
    }
  };
  db.save(ddoc);

  var resp = CouchDB.request("GET", "/" + db.name + "/_design/test/_list/you/me", {
    headers: {
      "Content-Type": "application/x-www-form-urlencoded"
    }
  });
  TEquals(200, resp.status, "should return a 200 response");
};
