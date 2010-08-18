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
couchTests.attachment_ranges = function(debug) {
    var db = new CouchDB("test_suite_db", {
        "X-Couch-Full-Commit": "false"
    });
    db.deleteDb();
    db.createDb();

    if (debug) debugger;

    var binAttDoc = {
        _id: "bin_doc",
        _attachments: {
            "foo.txt": {
                content_type: "application/octet-stream",
                data: "VGhpcyBpcyBhIGJhc2U2NCBlbmNvZGVkIHRleHQ="
            }
        }
    }

    var save_response = db.save(binAttDoc);
    T(save_response.ok);

    // Fetching the whole entity is a 206.
    var xhr = CouchDB.request("GET", "/test_suite_db/bin_doc/foo.txt", {
        headers: {
            "Range": "bytes=0-28"
        }
    });
    TEquals(206, xhr.status);
    TEquals("This is a base64 encoded text", xhr.responseText);
    TEquals("bytes 0-28/29", xhr.getResponseHeader("Content-Range"));
    TEquals("29", xhr.getResponseHeader("Content-Length"));

    // Fetch the whole entity without an end offset is a 200.
    var xhr = CouchDB.request("GET", "/test_suite_db/bin_doc/foo.txt", {
        headers: {
            "Range": "bytes=0-"
        }
    });
    TEquals(200, xhr.status);
    TEquals("This is a base64 encoded text", xhr.responseText);
    TEquals("29", xhr.getResponseHeader("Content-Length"));

    // Fetch the end of an entity without an end offset is a 206.
    var xhr = CouchDB.request("GET", "/test_suite_db/bin_doc/foo.txt", {
        headers: {
            "Range": "bytes=2-"
        }
    });
    TEquals(206, xhr.status);
    TEquals("is is a base64 encoded text", xhr.responseText);
    TEquals("bytes 2-28/29", xhr.getResponseHeader("Content-Range"));
    TEquals("27", xhr.getResponseHeader("Content-Length"));

    // Fetch past the end of the entity is a 416
    var xhr = CouchDB.request("GET", "/test_suite_db/bin_doc/foo.txt", {
        headers: {
            "Range": "bytes=0-29"
        }
    });
    TEquals(416, xhr.status);

    // Fetch first part of entity is a 206
    var xhr = CouchDB.request("GET", "/test_suite_db/bin_doc/foo.txt", {
        headers: {
            "Range": "bytes=0-3"
        }
    });
    TEquals(206, xhr.status);
    TEquals("This", xhr.responseText);
    TEquals("4", xhr.getResponseHeader("Content-Length"));
    TEquals("bytes 0-3/29", xhr.getResponseHeader("Content-Range"));

    // Fetch middle of entity is also a 206
    var xhr = CouchDB.request("GET", "/test_suite_db/bin_doc/foo.txt", {
        headers: {
            "Range": "bytes=10-15"
        }
    });
    TEquals(206, xhr.status);
    TEquals("base64", xhr.responseText);
    TEquals("6", xhr.getResponseHeader("Content-Length"));
    TEquals("bytes 10-15/29", xhr.getResponseHeader("Content-Range"));

    // Fetch end of entity is also a 206
    var xhr = CouchDB.request("GET", "/test_suite_db/bin_doc/foo.txt", {
        headers: {
            "Range": "bytes=-3"
        }
    });
    TEquals(206, xhr.status);
    TEquals("text", xhr.responseText);
    TEquals("4", xhr.getResponseHeader("Content-Length"));
    TEquals("bytes 25-28/29", xhr.getResponseHeader("Content-Range"));


};
