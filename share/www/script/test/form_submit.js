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

// Do some basic tests.
couchTests.form_submit = function(debug) {
    var db = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"false"});
    db.deleteDb();
    db.createDb();

    // PUT on existing DB should return 412 instead of 500
    var json = "{}";
    var xhr = CouchDB.request("POST", "/test_suite_db/baz", {body: json});
    T(xhr.status == 415);
    result = JSON.parse(xhr.responseText);
    T(result.error, "bad_content_type");
    T(result.reason, "Invalid Content-Type header for form upload");
};
