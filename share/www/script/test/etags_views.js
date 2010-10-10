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

couchTests.etags_views = function(debug) {
  var db = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"true"});
  db.deleteDb();
  db.createDb();
  if (debug) debugger;

  var designDoc = {
    _id: "_design/etags",
    language: "javascript",
    views : {
      fooView: {
        map: stringFun(function(doc) {
          if (doc.foo) {
            emit("bar", 1);
          }
        }),
      },
      basicView : {
        map : stringFun(function(doc) {
          if(doc.integer && doc.string) {
            emit(doc.integer, doc.string);
          }
        })
      },
      withReduce : {
        map : stringFun(function(doc) {
          if(doc.integer && doc.string) {
            emit(doc.integer, doc.string);
          }
        }),
        reduce : stringFun(function(keys, values, rereduce) {
          if (rereduce) {
            return sum(values);
          } else {
            return values.length;
          }
        })
      }
    }
  };
  T(db.save(designDoc).ok);
  db.bulkSave(makeDocs(0, 10));

  var xhr;

  // verify get w/Etag on map view
  xhr = CouchDB.request("GET", "/test_suite_db/_design/etags/_view/basicView");
  T(xhr.status == 200);
  var etag = xhr.getResponseHeader("etag");
  xhr = CouchDB.request("GET", "/test_suite_db/_design/etags/_view/basicView", {
    headers: {"if-none-match": etag}
  });
  T(xhr.status == 304);

  // verify ETag doesn't change when an update
  // doesn't change the view group's index
  T(db.save({"_id":"doc1", "foo":"bar"}).ok);
  xhr = CouchDB.request("GET", "/test_suite_db/_design/etags/_view/basicView");
  var etag1 = xhr.getResponseHeader("etag");
  T(etag1 == etag);
 
  // Verify that purges affect etags
  xhr = CouchDB.request("GET", "/test_suite_db/_design/etags/_view/fooView");
  var foo_etag = xhr.getResponseHeader("etag");
  var doc1 = db.open("doc1");
  xhr = CouchDB.request("POST", "/test_suite_db/_purge", {
    body: JSON.stringify({"doc1":[doc1._rev]})
  });
  xhr = CouchDB.request("GET", "/test_suite_db/_design/etags/_view/fooView");
  var etag1 = xhr.getResponseHeader("etag");
  T(etag1 != foo_etag);

  // Test that _purge didn't affect the other view etags.
  xhr = CouchDB.request("GET", "/test_suite_db/_design/etags/_view/basicView");
  var etag1 = xhr.getResponseHeader("etag");
  T(etag1 == etag);

  // verify different views in the same view group may have different ETags
  xhr = CouchDB.request("GET", "/test_suite_db/_design/etags/_view/fooView");
  var etag1 = xhr.getResponseHeader("etag");
  xhr = CouchDB.request("GET", "/test_suite_db/_design/etags/_view/basicView");
  var etag2 = xhr.getResponseHeader("etag");
  T(etag1 != etag2);

  // verify ETag changes when an update changes the view group's index.
  db.bulkSave(makeDocs(10, 20));
  xhr = CouchDB.request("GET", "/test_suite_db/_design/etags/_view/basicView");
  var etag1 = xhr.getResponseHeader("etag");
  T(etag1 != etag);

  // verify ETag is the same after a restart
  restartServer();
  xhr = CouchDB.request("GET", "/test_suite_db/_design/etags/_view/basicView");
  var etag2 = xhr.getResponseHeader("etag");
  T(etag1 == etag2);

  // reduce view
  xhr = CouchDB.request("GET", "/test_suite_db/_design/etags/_view/withReduce");
  T(xhr.status == 200);
  var etag = xhr.getResponseHeader("etag");
  xhr = CouchDB.request("GET", "/test_suite_db/_design/etags/_view/withReduce",{
    headers: {"if-none-match": etag}
  });
  T(xhr.status == 304);

  // verify ETag doesn't change when an update
  // doesn't change the view group's index
  T(db.save({"_id":"doc3", "foo":"bar"}).ok);
  xhr = CouchDB.request("GET", "/test_suite_db/_design/etags/_view/withReduce");
  var etag1 = xhr.getResponseHeader("etag");
  T(etag1 == etag);
  // purge
  var doc3 = db.open("doc3");
  xhr = CouchDB.request("POST", "/test_suite_db/_purge", {
    body: JSON.stringify({"doc3":[doc3._rev]})
  });
  xhr = CouchDB.request("GET", "/test_suite_db/_design/etags/_view/withReduce");
  var etag1 = xhr.getResponseHeader("etag");
  T(etag1 == etag);

  // verify different views in the same view group may have different ETags
  xhr = CouchDB.request("GET", "/test_suite_db/_design/etags/_view/fooView");
  var etag1 = xhr.getResponseHeader("etag");
  xhr = CouchDB.request("GET", "/test_suite_db/_design/etags/_view/withReduce");
  var etag2 = xhr.getResponseHeader("etag");
  T(etag1 != etag2);

  // verify ETag changes when an update changes the view group's index
  db.bulkSave(makeDocs(20, 30));
  xhr = CouchDB.request("GET", "/test_suite_db/_design/etags/_view/withReduce");
  var etag1 = xhr.getResponseHeader("etag");
  T(etag1 != etag);

  // verify ETag is the same after a restart
  restartServer();
  xhr = CouchDB.request("GET", "/test_suite_db/_design/etags/_view/withReduce");
  var etag2 = xhr.getResponseHeader("etag");
  T(etag1 == etag2);

  // confirm ETag changes with different POST bodies
  xhr = CouchDB.request("POST", "/test_suite_db/_design/etags/_view/basicView",
    {body: JSON.stringify({keys:[1]})}
  );
  var etag1 = xhr.getResponseHeader("etag");
  xhr = CouchDB.request("POST", "/test_suite_db/_design/etags/_view/basicView",
    {body: JSON.stringify({keys:[2]})}
  );
  var etag2 = xhr.getResponseHeader("etag");
  T(etag1 != etag2, "POST to map view generates key-depdendent ETags");

  xhr = CouchDB.request("POST",
    "/test_suite_db/_design/etags/_view/withReduce?group=true",
    {body: JSON.stringify({keys:[1]})}
  );
  etag1 = xhr.getResponseHeader("etag");
  xhr = CouchDB.request("POST",
    "/test_suite_db/_design/etags/_view/withReduce?group=true",
    {body: JSON.stringify({keys:[2]})}
  );
  etag2 = xhr.getResponseHeader("etag");
  T(etag1 != etag2, "POST to reduce view generates key-depdendent ETags");
  
  // all docs
  xhr = CouchDB.request("GET", "/test_suite_db/_all_docs");
  T(xhr.status == 200);
  var etag = xhr.getResponseHeader("etag");
  xhr = CouchDB.request("GET", "/test_suite_db/_all_docs", {
    headers: {"if-none-match": etag}
  });
  T(xhr.status == 304);

  // _changes
  xhr = CouchDB.request("GET", "/test_suite_db/_changes");
  T(xhr.status == 200);
  var etag = xhr.getResponseHeader("etag");
  xhr = CouchDB.request("GET", "/test_suite_db/_changes", {
    headers: {"if-none-match": etag}
  });
  T(xhr.status == 304);

  // list etag
  // in the list test for now
  
  // A new database should have unique _all_docs etags. 
  db.deleteDb(); 
  db.createDb(); 
  db.save({a: 1}); 
  xhr = CouchDB.request("GET", "/test_suite_db/_all_docs"); 
  var etag = xhr.getResponseHeader("etag"); 
  db.deleteDb(); 
  db.createDb(); 
  db.save({a: 2}); 
  xhr = CouchDB.request("GET", "/test_suite_db/_all_docs"); 
  var new_etag = xhr.getResponseHeader("etag");
  T(etag != new_etag);
  // but still be cacheable
  xhr = CouchDB.request("GET", "/test_suite_db/_all_docs"); 
  T(new_etag == xhr.getResponseHeader("etag"));
  
};
