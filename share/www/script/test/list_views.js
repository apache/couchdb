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

couchTests.list_views = function(debug) {
  var db = new CouchDB("test_suite_db");
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
      simpleForm: stringFun(function(head, row, req, row_info) {
        if (row) {
          // we ignore headers on rows and tail
          return {
                  body : '\n<li>Key: '+row.key
                  +' Value: '+row.value
                  +' LineNo: '+row_info.row_number+'</li>'
          };
        } else if (head) {
          // we return an object (like those used by external and show)
          // so that we can specify headers
          return {
            body : '<h1>Total Rows: '
              + head.total_rows
              + ' Offset: ' + head.offset
              + '</h1><ul>'
          };
        } else {
          // tail
          return {body : '</ul>'+
              '<p>FirstKey: '+(row_info ? row_info.first_key : '')+ 
              ' LastKey: '+(row_info ? row_info.prev_key : '')+'</p>'};
        }
      }),
      acceptSwitch: stringFun(function(head, row, req, row_info) {
        return respondWith(req, {
          html : function() {
            // If you're outputting text and you're not setting
            // any headers, you can just return a string.
            if (head) {
              return "HTML <ul>";
            } else if (row) {
              return '\n<li>Key: '
                +row.key+' Value: '+row.value
                +' LineNo: '+row_info.row_number+'</li>';
            } else { // tail
              return '</ul>';

            }
          },
          xml : function() {
            if (head) {
              return '<feed xmlns="http://www.w3.org/2005/Atom">'
                +'<title>Test XML Feed</title>';
            } else if (row) {
              // Becase Safari can't stand to see that dastardly
              // E4X outside of a string. Outside of tests you
              // can just use E4X literals.
              var entry = new XML('<entry/>');
              entry.id = row.id;
              entry.title = row.key;
              entry.content = row.value;
              // We'll also let you return just an E4X object
              // if you aren't setting headers.
              return entry;
            } else {
              return "</feed>";
            }
          }
        })
      }),
      qsParams: stringFun(function(head, row, req, row_info) {
        if(head) return {body: req.query.foo};
        else return {body: "\n"};
      }),
      stopIter: stringFun(function(head, row, req, row_info) {
        if(head) {
          return {body: "head"};
        } else if(row) {
          if(row_info.row_number > 2) return {stop: true};
          return {body: " " + row_info.row_number};
        } else {
          return {body: " tail"};
        }
      }),
      stopIter2: stringFun(function(head, row, req, row_info) {
        return respondWith(req, {
          html: function() {
            if(head) {
              return "head";
            } else if(row) {
              if(row_info.row_number > 2) return {stop: true};
              return " " + row_info.row_number;
            } else {
              return " tail";
            }
          }
        });
      }),
      emptyList: stringFun(function(head, row, req, row_info) {
        return { body: "" };
      })
    }
  };

  T(db.save(designDoc).ok);
  
  var docs = makeDocs(0, 10);
  db.bulkSave(docs);
  
  var view = db.view('lists/basicView');
  T(view.total_rows == 10);
  
  // standard get
  var xhr = CouchDB.request("GET", "/test_suite_db/_design/lists/_list/simpleForm/basicView");
  T(xhr.status == 200, "standard get should be 200");
  T(/Total Rows/.test(xhr.responseText));
  T(/Key: 1/.test(xhr.responseText));
  T(/LineNo: 0/.test(xhr.responseText));
  T(/LineNo: 5/.test(xhr.responseText));
  T(/FirstKey: 0/.test(xhr.responseText));
  T(/LastKey: 9/.test(xhr.responseText));


  var lines = xhr.responseText.split('\n');
  T(/LineNo: 5/.test(lines[6]));

  // test that etags are available
  var etag = xhr.getResponseHeader("etag");
  xhr = CouchDB.request("GET", "/test_suite_db/_design/lists/_list/simpleForm/basicView", {
    headers: {"if-none-match": etag}
  });
  T(xhr.status == 304);

  // get with query params
  var xhr = CouchDB.request("GET", "/test_suite_db/_design/lists/_list/simpleForm/basicView?startkey=3");
  T(xhr.status == 200, "with query params");
  T(/Total Rows/.test(xhr.responseText));
  T(!(/Key: 1/.test(xhr.responseText)));
  T(/FirstKey: 3/.test(xhr.responseText));
  T(/LastKey: 9/.test(xhr.responseText));

  
  // with 0 rows
  var xhr = CouchDB.request("GET", "/test_suite_db/_design/lists/_list/simpleForm/basicView?startkey=30");
  T(xhr.status == 200, "0 rows");
  T(/Total Rows/.test(xhr.responseText));
  T(/Offset: null/.test(xhr.responseText));

  // reduce with 0 rows
  var xhr = CouchDB.request("GET", "/test_suite_db/_design/lists/_list/simpleForm/withReduce?startkey=30");
  T(xhr.status == 200, "reduce 0 rows");
  T(/Total Rows/.test(xhr.responseText));
  T(/Offset: undefined/.test(xhr.responseText));

  
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
  
  // with accept headers for HTML
  xhr = CouchDB.request("GET", "/test_suite_db/_design/lists/_list/acceptSwitch/basicView", {
    headers: {
      "Accept": 'text/html'
    }
  });
  T(xhr.getResponseHeader("Content-Type") == "text/html");
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

  // now with extra qs params
  var xhr = CouchDB.request("GET", "/test_suite_db/_design/lists/_list/qsParams/basicView?foo=blam");
  T(xhr.responseText.match(/blam/));
  
  // aborting iteration
  var xhr = CouchDB.request("GET", "/test_suite_db/_design/lists/_list/stopIter/basicView");
  T(xhr.responseText.match(/^head 0 1 2 tail$/));
  xhr = CouchDB.request("GET", "/test_suite_db/_design/lists/_list/stopIter2/basicView");
  T(xhr.responseText.match(/^head 0 1 2 tail$/));

  // aborting iteration with reduce
  var xhr = CouchDB.request("GET", "/test_suite_db/_design/lists/_list/stopIter/withReduce?group=true");
  T(xhr.responseText.match(/^head 0 1 2 tail$/));
  xhr = CouchDB.request("GET", "/test_suite_db/_design/lists/_list/stopIter2/withReduce?group=true");
  T(xhr.responseText.match(/^head 0 1 2 tail$/));

  // empty list
  var xhr = CouchDB.request("GET", "/test_suite_db/_design/lists/_list/emptyList/basicView");
  T(xhr.responseText.match(/^$/));
  xhr = CouchDB.request("GET", "/test_suite_db/_design/lists/_list/emptyList/withReduce?group=true");
  T(xhr.responseText.match(/^$/));

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
};
