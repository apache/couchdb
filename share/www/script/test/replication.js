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

couchTests.replication = function(debug) {
  if (debug) debugger;
  var host = CouchDB.host;
  var dbPairs = [
    {source:"test_suite_db_a",
      target:"test_suite_db_b"},
    {source:"test_suite_db_a",
      target:"http://" + host + "/test_suite_db_b"},
    {source:"http://" + host + "/test_suite_db_a",
      target:"test_suite_db_b"},
    {source:"http://" + host + "/test_suite_db_a",
      target:"http://" + host + "/test_suite_db_b"}
  ]
  var dbA = new CouchDB("test_suite_db_a", {"X-Couch-Full-Commit":"false"});
  var dbB = new CouchDB("test_suite_db_b", {"X-Couch-Full-Commit":"false"});
  var numDocs = 10;
  var xhr;
  for (var testPair = 0; testPair < dbPairs.length; testPair++) {
    var A = dbPairs[testPair].source
    var B = dbPairs[testPair].target

    dbA.deleteDb();
    dbA.createDb();
    dbB.deleteDb();
    dbB.createDb();

    var repTests = {
      // copy and paste and put your code in. delete unused steps.
      test_template: new function () {
        this.init = function(dbA, dbB) {
          // before anything has happened
        }
        this.afterAB1 = function(dbA, dbB) {
          // called after replicating src=A  tgt=B first time.
        };
        this.afterBA1 = function(dbA, dbB) {
          // called after replicating src=B  tgt=A first time.
        };
        this.afterAB2 = function(dbA, dbB) {
          // called after replicating src=A  tgt=B second time.
        };
        this.afterBA2 = function(dbA, dbB) {
          // etc...
        };
      },

      simple_test: new function () {
        this.init = function(dbA, dbB) {
          var docs = makeDocs(0, numDocs);
          dbA.bulkSave(docs);
        };

        this.afterAB1 = function(dbA, dbB) {
          for (var j = 0; j < numDocs; j++) {
            var docA = dbA.open("" + j);
            var docB = dbB.open("" + j);
            T(docA._rev == docB._rev);
          }
        };
      },

     deletes_test: new function () {
        // make sure deletes are replicated
        this.init = function(dbA, dbB) {
          T(dbA.save({_id:"foo1",value:"a"}).ok);
        };

        this.afterAB1 = function(dbA, dbB) {
          var docA = dbA.open("foo1");
          var docB = dbB.open("foo1");
          T(docA._rev == docB._rev);

          dbA.deleteDoc(docA);
        };

        this.afterAB2 = function(dbA, dbB) {
          T(dbA.open("foo1") == null);
          T(dbB.open("foo1") == null);
        };
      },

      deleted_test : new function() {
        // docs created and deleted on a single node are also replicated
        this.init = function(dbA, dbB) {
          T(dbA.save({_id:"del1",value:"a"}).ok);
          var docA = dbA.open("del1");
          dbA.deleteDoc(docA);
        };

        this.afterAB1 = function(dbA, dbB) {
          var rows = dbB.allDocsBySeq().rows;
          var rowCnt = 0;
          for (var i=0; i < rows.length; i++) {
            if (rows[i].id == "del1") {
              rowCnt += 1;
              T(rows[i].value.deleted == true);
            }
          };
          T(rowCnt == 1);
        };
      },

      slashes_in_ids_test: new function () {
        // make sure docs with slashes in id replicate properly
        this.init = function(dbA, dbB) {
          dbA.save({ _id:"abc/def", val:"one" });
        };

        this.afterAB1 = function(dbA, dbB) {
          var docA = dbA.open("abc/def");
          var docB = dbB.open("abc/def");
          T(docA._rev == docB._rev);
        };
      },

      design_docs_test: new function() {
        // make sure design docs replicate properly
        this.init = function(dbA, dbB) {
          dbA.save({ _id:"_design/test" });
        };

        this.afterAB1 = function() {
          var docA = dbA.open("_design/test");
          var docB = dbB.open("_design/test");
          T(docA._rev == docB._rev);
        };
      },

      attachments_test: new function () {
        // Test attachments
        this.init = function(dbA, dbB) {
          dbA.save({
            _id:"bin_doc",
            _attachments:{
              "foo+bar.txt": {
                "type":"base64",
                "data": "VGhpcyBpcyBhIGJhc2U2NCBlbmNvZGVkIHRleHQ="
              }
            }
          });
          // make sure on design docs as well
          dbA.save({
            _id:"_design/with_bin",
            _attachments:{
              "foo+bar.txt": {
                "type":"base64",
                "data": "VGhpcyBpcyBhIGJhc2U2NCBlbmNvZGVkIHRleHQ="
              }
            }
          });
        };

        this.afterAB1 = function(dbA, dbB) {
          var xhr = CouchDB.request("GET",
            "/test_suite_db_a/bin_doc/foo%2Bbar.txt");
          T(xhr.responseText == "This is a base64 encoded text")

          xhr = CouchDB.request("GET",
            "/test_suite_db_b/bin_doc/foo%2Bbar.txt");
          T(xhr.responseText == "This is a base64 encoded text")

          // and the design-doc
          xhr = CouchDB.request("GET",
            "/test_suite_db_a/_design/with_bin/foo%2Bbar.txt");
          T(xhr.responseText == "This is a base64 encoded text")

          xhr = CouchDB.request("GET",
            "/test_suite_db_b/_design/with_bin/foo%2Bbar.txt");
          T(xhr.responseText == "This is a base64 encoded text")
        };
      },

      conflicts_test: new function () {
        // test conflicts
        this.init = function(dbA, dbB) {
          dbA.save({_id:"foo",value:"a"});
          dbB.save({_id:"foo",value:"b"});
        };

        this.afterBA1 = function(dbA, dbB) {
          var docA = dbA.open("foo", {conflicts: true});
          var docB = dbB.open("foo", {conflicts: true});

          // make sure the same rev is in each db
          T(docA._rev === docB._rev);

          // make sure the conflicts are the same in each db
          T(docA._conflicts[0] === docB._conflicts[0]);

          // delete a conflict.
          dbA.deleteDoc({_id:"foo", _rev:docA._conflicts[0]});
        };

        this.afterBA2 = function(dbA, dbB) {
          // open documents and include the conflict meta data
          var docA = dbA.open("foo", {conflicts: true, deleted_conflicts: true});
          var docB = dbB.open("foo", {conflicts: true, deleted_conflicts: true});

          // We should have no conflicts this time
          T(docA._conflicts === undefined)
          T(docB._conflicts === undefined);

          // They show up as deleted conflicts instead
          T(docA._deleted_conflicts[0] == docB._deleted_conflicts[0]);
        };
      }
    };

    var test;
    for(test in repTests) {
      if(repTests[test].init) {
        repTests[test].init(dbA, dbB);
      }
    }

    var result = CouchDB.replicate(A, B);

    var seqA = result.source_last_seq;
    T(0 == result.history[0].start_last_seq);
    T(result.history[1] === undefined)

    for(test in repTests) {
      if(repTests[test].afterAB1) repTests[test].afterAB1(dbA, dbB);
    }

    result = CouchDB.replicate(B, A);

    var seqB = result.source_last_seq;
    T(0 == result.history[0].start_last_seq);
    T(result.history[1] === undefined)

    for(test in repTests) {
      if(repTests[test].afterBA1) repTests[test].afterBA1(dbA, dbB);
    }

    var result2 = CouchDB.replicate(A, B);

    // each successful replication produces a new session id
    T(result2.session_id != result.session_id);

    T(seqA < result2.source_last_seq);
    T(seqA == result2.history[0].start_last_seq);
    T(result2.history[1].end_last_seq == seqA)

    seqA = result2.source_last_seq;

    for(test in repTests) {
      if(repTests[test].afterAB2) repTests[test].afterAB2(dbA, dbB);
    }

    result = CouchDB.replicate(B, A)

    T(seqB < result.source_last_seq);
    T(seqB == result.history[0].start_last_seq);
    T(result.history[1].end_last_seq == seqB)

    seqB = result.source_last_seq;

    for(test in repTests) {
      if(repTests[test].afterBA2) repTests[test].afterBA2(dbA, dbB);
    }

    // do an replication where nothing has changed
    result2 = CouchDB.replicate(B, A);
    T(result2.no_changes == true);
    T(result2.session_id == result.session_id);
  }
};
