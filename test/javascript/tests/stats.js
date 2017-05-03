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
  function newDb(doSetup) {
    var db_name = get_random_db_name();
    var db = new CouchDB(db_name, {"X-Couch-Full-Commit":"false"});
    if(doSetup) {
      db.createDb();
    }
    return db;
  };

  function getStat(path) {
    var stat = CouchDB.requestStats(path, true);
    return stat ? stat.value : null;
  };

  function doView(db) {
    var designDoc = {
      _id:"_design/test", // turn off couch.js id escaping?
      language: "javascript",
      views: {
        all_docs: {map: "function(doc) {emit(doc.integer, null);}"}
      }
    };
    db.save(designDoc);
    db.view("test/all_docs");
  };

  function runTest(path, funcs) {
    var db = newDb(true);
    if(funcs.setup) funcs.setup(db);
    var before = getStat(path);
    if(funcs.run) funcs.run(db);
    var after = getStat(path);
    if(funcs.test) funcs.test(before, after);
  }

  if (debug) debugger;

  (function() {
    var db = newDb(false);
    db.deleteDb();
  
    var before = getStat(["couchdb", "open_databases"]);
    db.createDb();
    var after = getStat(["couchdb", "open_databases"]);
    TEquals(before+8, after, "Creating a db increments open db count.");
    db.deleteDb();
  })();
  
  runTest(["couchdb", "open_databases"], {
    setup: function() {restartServer();},
    run: function(db) {db.open("123");},
    test: function(before, after) {
      T(before<after, "Opening a db increases open db count.");
    }
  });
  
  runTest(["couchdb", "open_databases"], {
    setup: function(db) {restartServer(); db.open("123");},
    run: function(db) {db.deleteDb();},
    test: function(before, after) {
      T(before>after, "Deleting a db decrements open db count.");
    }
  });
  
  /* Improvements in LRU has made this test difficult... 
  (function() {
    restartServer();
    var max = 5;
    
    var testFun = function() {
      var pre_dbs = getStat(["couchdb", "open_databases"]) || 0;
      var pre_files = getStat(["couchdb", "open_os_files"]) || 0;
     
      var triggered = false;
      var db = null;
      var dbs = [];
      for(var i = 0; i < max*2; i++) {
        while (true) {
            try {
              db = newDb(true);
              dbs.push(db);
              break;
            } catch(e) {
                // all_dbs_active error!
              triggered = true;
            }
        }

        // Trigger a delayed commit
        db.save({"a": "1"});
      }
      T(triggered, "We managed to force a all_dbs_active error.");
      
      var open_dbs = getStat(["couchdb", "open_databases"]);
      TEquals(open_dbs > 0, true, "We actually opened some dbs.");
      TEquals(max, open_dbs, "We only have max db's open.");
      
      for (var i = 0; i < dbs.length; i++) {
        dbs[i].deleteDb();
      }
      
      var post_dbs = getStat(["couchdb", "open_databases"]);
      var post_files = getStat(["couchdb", "open_os_files"]);
      TEquals(pre_dbs, post_dbs, "We have the same number of open dbs.");
      TEquals(pre_files, post_files, "We have the same number of open files.");
    };
    
    run_on_modified_server(
      [{section: "couchdb", key: "max_dbs_open", value: "40"}],
      testFun
    );
  })();
  */
  
  // Just fetching the before value is the extra +1 in test
  runTest(["couchdb", "httpd", "requests"], {
    run: function() {CouchDB.request("GET", "/");},
    test: function(before, after) {
      TEquals(before+2, after, "Request counts are incremented properly.");
    }
  });
  
  runTest(["couchdb", "database_reads"], {
    setup: function(db) {db.save({"_id": "test"});},
    run: function(db) {db.open("test");},
    test: function(before, after) {
      T(before<after, "Reading a doc increments docs reads.");
    }
  });
  
  runTest(["couchdb", "database_reads"], {
    setup: function(db) {db.save({"_id": "test"});},
    run: function(db) {db.request("GET", "/");},
    test: function(before, after) {
      TEquals(before, after, "Only doc reads increment doc reads.");
    }
  });
  
  runTest(["couchdb", "database_reads"], {
    setup: function(db) {db.save({"_id": "test"});},
    run: function(db) {db.open("test", {"open_revs": "all"});},
    test: function(before, after) {
      T(before<after, "Reading doc revs increments docs reads.");
    }
  });
  
  runTest(["couchdb", "database_writes"], {
    run: function(db) {db.save({"a": "1"});},
    test: function(before, after) {
      T(before<after, "Saving docs incrememnts doc writes.");
    }
  });
  
  runTest(["couchdb", "database_writes"], {
    run: function(db) {
      CouchDB.request("POST", "/" + db.name + "", {
        headers: {"Content-Type": "application/json"},
        body: '{"a": "1"}'
      });
    },
    test: function(before, after) {
      T(before<after, "POST'ing new docs increments doc writes.");
    }
  });
  
  runTest(["couchdb", "database_writes"], {
    setup: function(db) {db.save({"_id": "test"});},
    run: function(db) {var doc = db.open("test"); db.save(doc);},
    test: function(before, after) {
      T(before<after, "Updating docs incrememnts doc writes.");
    }
  });
  
  runTest(["couchdb", "database_writes"], {
    setup: function(db) {db.save({"_id": "test"});},
    run: function(db) {var doc = db.open("test"); db.deleteDoc(doc);},
    test: function(before, after) {
      T(before<after, "Deleting docs increments doc writes.");
    }
  });
  
  runTest(["couchdb", "database_writes"], {
    setup: function(db) {db.save({"_id": "test"});},
    run: function(db) {
      CouchDB.request("COPY", "/" + db.name + "/test", {
        headers: {"Destination": "copy_of_test"}
      });
    },
    test: function(before, after) {
      T(before<after, "Copying docs increments doc writes.");
    }
  });
  
  runTest(["couchdb", "database_writes"], {
    run: function(db) {
      CouchDB.request("PUT", "/" + db.name + "/bin_doc2/foo2.txt", {
        body: "This is no base64 encoded test",
        headers: {"Content-Type": "text/plain;charset=utf-8"}
      });
    },
    test: function(before, after) {
      T(before<after, "Create with attachment increments doc writes.");
    }
  });
  
  runTest(["couchdb", "database_writes"], {
    setup: function(db) {db.save({"_id": "test"});},
    run: function(db) {
      var doc = db.open("test");
      CouchDB.request("PUT", "/" + db.name + "/test/foo2.txt?rev=" + doc._rev, {
        body: "This is no base64 encoded text",
        headers: {"Content-Type": "text/plainn;charset=utf-8"}
      });
    },
    test: function(before, after) {
      T(before<after, "Adding attachment increments doc writes.");
    }
  });
  
  runTest(["couchdb", "httpd", "bulk_requests"], {
    run: function(db) {db.bulkSave(makeDocs(5));},
    test: function(before, after) {
      TEquals(before+1, after, "The bulk_requests counter is incremented.");
    }
  });
  
  runTest(["couchdb", "httpd", "view_reads"], {
    run: function(db) {doView(db);},
    test: function(before, after) {
      T(before<after, "Reading a view increments view reads.");
    }
  });
  
  runTest(["couchdb", "httpd", "view_reads"], {
    setup: function(db) {db.save({"_id": "test"});},
    run: function(db) {db.open("test");},
    test: function(before, after) {
      TEquals(before, after, "Reading a doc doesn't increment view reads.");
    }
  });
  
  // Relies on getting the stats values being GET requests.
  runTest(["couchdb", "httpd_request_methods", "GET"], {
    test: function(before, after) {
      TEquals(before+1, after, "Get requests are incremented properly.");
    }
  });
  
  runTest(["couchdb", "httpd_request_methods", "GET"], {
    run: function() {CouchDB.request("POST", "/");},
    test: function(before, after) {
      TEquals(before+1, after, "POST requests don't affect GET counter.");
    }
  });
  
  runTest(["couchdb", "httpd_request_methods", "POST"], {
    run: function() {CouchDB.request("POST", "/");},
    test: function(before, after) {
      TEquals(before+1, after, "POST requests are incremented properly.");
    }
  });
  
  runTest(["couchdb", "httpd_status_codes", "404"], {
    run: function() {CouchDB.request("GET", "/nonexistant_db");},
    test: function(before, after) {
      TEquals(before+1, after, "Increments 404 counter on db not found.");
    }
  });
  
  runTest(["couchdb", "httpd_status_codes", "404"], {
    run: function() {CouchDB.request("GET", "/");},
    test: function(before, after) {
      TEquals(before, after, "Getting DB info doesn't increment 404's");
    }
  });

  var test_metric = function(metric, expected_fields) {
    for (var k in metric) {
      T(expected_fields.indexOf(k) >= 0, "Unknown property name: " + k);
    }
    for (var k in expected_fields) {
      T(metric[expected_fields[k]] !== undefined, "Missing required property: " + k);
    }
  };

  var test_histogram = function(histo) {
    test_metric(histo, ["value", "type", "desc"]);
    test_metric(histo.value, ["min", "max", "arithmetic_mean",
      "geometric_mean", "harmonic_mean", "median", "variance",
       "standard_deviation", "skewness", "kurtosis", "percentile",
       "histogram", "n"]);
  };

  var test_counter = function(counter) {
    test_metric(counter, ["value", "desc", "type"]);
  };

  var test_metrics = function(metrics) {
    if (metrics.type === "counter") {
      test_counter(metrics);
    } else if (metrics.type === "gauge") {
      test_counter(metrics);
    } else if (metrics.type === "histogram") {
      test_histogram(metrics);
    } else if (metrics.type === undefined) {
      for (var k in metrics) {
        test_metrics(metrics[k]);
      }
    }
  };

  (function() {
    var summary = JSON.parse(CouchDB.request("GET", "/_node/node1@127.0.0.1/_stats", {
      headers: {"Accept": "application/json"}
    }).responseText);
    T(typeof(summary) === 'object');
    test_metrics(summary);
  })();

  // cleanup
};
