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

couchTests.regression = function(debug) {
    var db = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"false"});
    db.deleteDb();
    db.createDb();
    if (debug) debugger;

    // COUCHDB-1178
    {
        var r1 = {"_id":"doc","foo":"bar"};
        var r2 = {"_id":"doc","foo":"baz","_rev":"1-4c6114c65e295552ab1019e2b046b10e"};
        var r3 = {"_id":"doc","foo":"bam","_rev":"2-cfcd6781f13994bde69a1c3320bfdadb"};
        var r4 = {"_id":"doc","foo":"bat","_rev":"3-cc2f3210d779aef595cd4738be0ef8ff"};

        T(db.save({"_id":"_design/couchdb-1178","validate_doc_update":"function(){}"}).ok);
        T(db.save(r1).ok);
        T(db.save(r2).ok);
        T(db.save(r3).ok);

        T(db.compact().ok);
        while (db.info().compact_running) {};

        TEquals({"_id":"doc",
                 "_rev":"3-cc2f3210d779aef595cd4738be0ef8ff",
                 "foo":"bam",
                 "_revisions":{"start":3,
                               "ids":["cc2f3210d779aef595cd4738be0ef8ff",
                                      "cfcd6781f13994bde69a1c3320bfdadb",
                                      "4c6114c65e295552ab1019e2b046b10e"]}},
                db.open("doc", {"revs": true}));

        TEquals([], db.bulkSave([r4, r3, r2], {"new_edits":false}), "no failures");
    }

    // cleanup
    db.deleteDb();
};