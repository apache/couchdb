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

couchTests.stats = function(debug) {
  if (debug) debugger;

  var open_databases_tests = {
    'should increment the number of open databases when creating a db': function(name) {
       var db = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"false"});
       db.deleteDb();
       var open_databases = requestStatsTest("couchdb", "open_databases").current;
       db.createDb();

       var new_open_databases = requestStatsTest("couchdb", "open_databases").current;
       TEquals(open_databases + 1, new_open_databases, name);
     },
    'should increment the number of open databases when opening a db': function(name) {
       var db = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"false"});
       db.deleteDb();
       db.createDb();

       restartServer();

       var open_databases = requestStatsTest("couchdb", "open_databases").current;

       db.open("123");

       var new_open_databases = requestStatsTest("couchdb", "open_databases").current;
       TEquals(open_databases + 1, new_open_databases, name);
     },
       'should decrement the number of open databases when deleting': function(name) {
       var db = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"false"});
       db.deleteDb();
       db.createDb();
       var open_databases = requestStatsTest("couchdb", "open_databases").current;

       db.deleteDb();
       var new_open_databases = requestStatsTest("couchdb", "open_databases").current;
       TEquals(open_databases - 1, new_open_databases, name);
     },
    'should keep the same number of open databases when reaching the max_dbs_open limit': function(name) {
      restartServer();
      var max = 5;
      run_on_modified_server(
        [{section: "couchdb",
          key: "max_dbs_open",
          value: max.toString()}],

        function () {
          var dbs_open = requestStatsTest("couchdb", "open_databases").current;
          var files_open = requestStatsTest("couchdb", "open_os_files").current;
          for(var i=0; i<max+1; i++) {
            var db = new CouchDB("test_suite_db" + i);
            db.deleteDb();
            db.createDb();
          }

          var open_databases = requestStatsTest("couchdb", "open_databases").current;
          T(open_databases > 0 && max >= open_databases, name);

          for(var i=0; i<max+1; i++) {
            var db = new CouchDB("test_suite_db" + i);
            db.deleteDb();
          }
          T(dbs_open == requestStatsTest("couchdb", "open_databases").current);
          T(files_open == requestStatsTest("couchdb", "open_os_files").current);
        })
    },
 };

  var request_count_tests = {
   'should increase the request count for every request': function(name) {
     var requests = requestStatsTest("httpd", "requests").current + 1;

     CouchDB.request("GET", "/");

     var new_requests = requestStatsTest("httpd", "requests").current;

     TEquals(requests + 1, new_requests, name);
   }
 };

 var database_read_count_tests = {
   'should increase database reads counter when a document is read': function(name) {
     var db = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"false"});
     db.deleteDb();
     db.createDb();
     db.save({"_id":"test"});

     var reads = requestStatsTest("couchdb", "database_reads").current;
     db.open("test");
     var new_reads = requestStatsTest("couchdb", "database_reads").current;

     TEquals(reads + 1 , new_reads, name);
   },
   'should not increase database read counter when a non-document is read': function(name) {
     var db = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"false"});
     db.deleteDb();
     db.createDb();
     db.save({"_id":"test"});

     var reads = requestStatsTest("couchdb", "database_reads").current;
     CouchDB.request("GET", "/");
     var new_reads = requestStatsTest("couchdb", "database_reads").current;

     TEquals(reads, new_reads, name);
   },
   'should increase database read counter when a document\'s revisions are read': function(name) {
     var db = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"false"});
     db.deleteDb();
     db.createDb();
     db.save({"_id":"test"});

     var reads = requestStatsTest("couchdb", "database_reads").current;
     db.open("test", {"open_revs":"all"});
     var new_reads = requestStatsTest("couchdb", "database_reads").current;

     TEquals(reads + 1 , new_reads, name);
   }
 };

 var view_read_count_tests = {
   'should increase the permanent view read counter': function(name) {
     var db = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"false"});
     db.deleteDb();
     db.createDb();

     var reads = requestStatsTest("httpd", "view_reads").current;
     createAndRequestView(db);
     var new_reads = requestStatsTest("httpd", "view_reads").current;

     TEquals(reads + 1 , new_reads, name);
   },
   'should not increase the permanent view read counter when a document is read': function(name) {
     var db = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"false"});
     db.deleteDb();
     db.createDb();
     db.save({"_id":"test"});

     var reads = requestStatsTest("httpd", "view_reads").current;
     db.open("test");
     var new_reads = requestStatsTest("httpd", "view_reads").current;

     TEquals(reads, new_reads, name);
   },
   'should not increase the permanent view read counter when a temporary view is read': function(name) {
     var db = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"false"});
     db.deleteDb();
     db.createDb();

     var reads = requestStatsTest("httpd", "view_reads").current;
     db.query(function(doc) { emit(doc._id)});
     var new_reads = requestStatsTest("httpd", "view_reads").current;

     TEquals(reads, new_reads, name);
   },
   'should increase the temporary view read counter': function(name) {
     var db = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"false"});
     db.deleteDb();
     db.createDb();

     var reads = requestStatsTest("httpd", "temporary_view_reads").current;
     db.query(function(doc) { emit(doc._id)});
     var new_reads = requestStatsTest("httpd", "temporary_view_reads").current;

     TEquals(reads + 1, new_reads, name);
   },
   'should increase the temporary view read counter when querying a permanent view': function(name) {
     var db = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"false"});
     db.deleteDb();
     db.createDb();

     var reads = requestStatsTest("httpd", "view_reads").current;
     createAndRequestView(db);
     var new_reads = requestStatsTest("httpd", "view_reads").current;

     TEquals(reads + 1 , new_reads, name);
   }
 };

 var http_requests_by_method_tests = {
   'should count GET requests': function(name) {
     var requests = requestStatsTest("httpd_request_methods", "GET").current;
     var new_requests = requestStatsTest("httpd_request_methods", "GET").current;

     TEquals(requests + 1, new_requests, name);
   },
   'should not count GET requests for POST request': function(name) {
     var requests = requestStatsTest("httpd_request_methods", "GET").current;
     CouchDB.request("POST", "/");
     var new_requests = requestStatsTest("httpd_request_methods", "GET").current;

     TEquals(requests + 1, new_requests, name);
   },
   'should count POST requests': function(name) {
     var requests = requestStatsTest("httpd_request_methods", "POST").current;
     CouchDB.request("POST", "/");
     var new_requests = requestStatsTest("httpd_request_methods", "POST").current;

     TEquals(requests + 1, new_requests, name);
   }
 };

 var document_write_count_tests = {
   'should increment database changes counter for document creates': function(name) {
     var db = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"false"});
     db.deleteDb();
     db.createDb();

     var creates = requestStatsTest("couchdb", "database_writes").current;
     db.save({"a":"1"});
     var new_creates = requestStatsTest("couchdb", "database_writes").current;

     TEquals(creates + 1, new_creates, name);
   },
   'should increment database changes counter for document updates': function(name) {
     var db = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"false"});
     db.deleteDb();
     db.createDb();

     var doc = {"_id":"test"};
     db.save(doc);

     var updates = requestStatsTest("couchdb", "database_writes").current;
     db.save(doc);
     var new_updates = requestStatsTest("couchdb", "database_writes").current;

     TEquals(updates + 1, new_updates, name);
   },
   'should increment database changes counter for document deletes': function(name) {
     var db = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"false"});
     db.deleteDb();
     db.createDb();

     var doc = {"_id":"test"};
     db.save(doc);

     var deletes = requestStatsTest("couchdb", "database_writes").current;
     db.deleteDoc(doc);
     var new_deletes = requestStatsTest("couchdb", "database_writes").current;

     TEquals(deletes + 1, new_deletes, name);
   },
   'should increment database changes counter for document copies': function(name) {
     var db = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"false"});
     db.deleteDb();
     db.createDb();

     var doc = {"_id":"test"};
     db.save(doc);

     var copies = requestStatsTest("couchdb", "database_writes").current;
     CouchDB.request("COPY", "/test_suite_db/test", {
       headers: {"Destination":"copy_of_test"}
     });
     var new_copies = requestStatsTest("couchdb", "database_writes").current;

     TEquals(copies + 1, new_copies, name);
   },
   'should increase the bulk doc counter': function(name) {
     var db = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"false"});
     db.deleteDb();
     db.createDb();

     var bulks = requestStatsTest("httpd", "bulk_requests").current;

     var docs = makeDocs(5);
     db.bulkSave(docs);

     var new_bulks = requestStatsTest("httpd", "bulk_requests").current;

     TEquals(bulks + 1, new_bulks, name);
   },
   'should increment database changes counter for document creates using POST': function(name) {
     var db = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"false"});
     db.deleteDb();
     db.createDb();

     var creates = requestStatsTest("couchdb", "database_writes").current;
     CouchDB.request("POST", "/test_suite_db", {body:'{"a":"1"}'});
     var new_creates = requestStatsTest("couchdb", "database_writes").current;

     TEquals(creates + 1, new_creates, name);
   },
   'should increment database changes counter when adding attachment': function(name) {
     var db = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"false"});
     db.deleteDb();
     db.createDb();

     var creates = requestStatsTest("couchdb", "database_writes").current;
     CouchDB.request("PUT", "/test_suite_db/bin_doc2/foo2.txt", {
           body:"This is no base64 encoded text",
           headers:{"Content-Type": "text/plain;charset=utf-8"}
     });
     var new_creates = requestStatsTest("couchdb", "database_writes").current;
     TEquals(creates + 1, new_creates, name);
   },
   'should increment database changes counter when adding attachment to existing doc': function(name) {
     var db = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"false"});
     db.deleteDb();
     db.createDb();

     var doc = {_id:"test"};
     db.save(doc);

     var updates = requestStatsTest("couchdb", "database_writes").current;
     CouchDB.request("PUT", "/test_suite_db/test/foo2.txt?rev=" + doc._rev, {
           body:"This is no base64 encoded text",
           headers:{"Content-Type": "text/plain;charset=utf-8"}
     });
     var new_updates = requestStatsTest("couchdb", "database_writes").current;
     TEquals(updates + 1, new_updates, name);
   }

 };
 var response_codes_tests = {
   'should increment the response code counter': function(name) {
     var db = new CouchDB("nonexistant_db", {"X-Couch-Full-Commit":"false"});
     db.deleteDb();

     var not_founds = requestStatsTest("httpd_status_codes", "404").current;
     CouchDB.request("GET", "/nonexistant_db");
     var new_not_founds = requestStatsTest("httpd_status_codes", "404").current;

     TEquals(not_founds + 1, new_not_founds, name);
   },
   'should not increment respinse code counter for other response code': function(name) {
     var not_founds = requestStatsTest("http_status_codes", "404").current;
     CouchDB.request("GET", "/");
     var new_not_founds = requestStatsTest("http_status_codes", "404").current;

     TEquals(not_founds, new_not_founds, name);
   }
 };

 var aggregation_tests = {
   'should return the mean': function(name) {
     CouchDB.request("GET", "/");

     var mean = requestStatsTest("httpd", "requests").mean;

     T(mean >= 0, name);
   },
   'should return the maximum': function(name) {
     CouchDB.request("GET", "/");

     var maximum = requestStatsTest("httpd", "requests").max;

     T(maximum >= 0, name);
   },
   'should return the minimum': function(name) {
     CouchDB.request("GET", "/");

     var minimum = requestStatsTest("httpd", "requests", "min").min;

     T(minimum >= 0, name);
   },
   'should return the stddev': function(name) {
     CouchDB.request("GET", "/");

     var stddev = requestStatsTest("httpd", "stddev_requests").current;

     T(stddev >= 0, name);
   }
 };

 var summary_tests = {
   'should show a summary of all counters with aggregated values': function(name) {
     var options = {};
     options.headers = {"Accept": "application/json"};
     var summary = JSON.parse(CouchDB.request("GET", "/_stats", options).responseText);
     var aggregates = ["mean", "min", "max", "stddev",
       "current"];

     for(var i in aggregates) {
       T(summary.httpd.requests[aggregates[i]] >= 0, aggregates[i] + " >= 0", name);
     }
   }
 };

   var tests = [
     open_databases_tests,
     request_count_tests,
     database_read_count_tests,
     view_read_count_tests,
     http_requests_by_method_tests,
     document_write_count_tests,
     response_codes_tests,
     aggregation_tests,
     summary_tests
   ];

   for(var testGroup in tests) {
     for(var test in tests[testGroup]) {
       tests[testGroup][test](test);
     }
   };

   function createAndRequestView(db) {
     var designDoc = {
       _id:"_design/test", // turn off couch.js id escaping?
       language: "javascript",
       views: {
         all_docs_twice: {map: "function(doc) { emit(doc.integer, null); emit(doc.integer, null) }"},
       }
     };
     db.save(designDoc);

     db.view("test/all_docs_twice");
   }

   function requestStatsTest(module, key) {
     return CouchDB.requestStats(module, key, true);
   }
}
