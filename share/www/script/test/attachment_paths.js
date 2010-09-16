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

couchTests.attachment_paths = function(debug) {
  if (debug) debugger;
  var dbNames = ["test_suite_db", "test_suite_db/with_slashes"];
  for (var i=0; i < dbNames.length; i++) {
    var db = new CouchDB(dbNames[i]);
    var dbName = encodeURIComponent(dbNames[i]);
    db.deleteDb();
    db.createDb();

    // first just save a regular doc with an attachment that has a slash in the url.
    // (also gonna run an encoding check case)
    var binAttDoc = {
      _id: "bin_doc",
      _attachments:{
        "foo/bar.txt": {
          content_type:"text/plain",
          data: "VGhpcyBpcyBhIGJhc2U2NCBlbmNvZGVkIHRleHQ="
        },
        "foo%2Fbaz.txt": {
          content_type:"text/plain",
          data: "V2UgbGlrZSBwZXJjZW50IHR3byBGLg=="
        }
      }
    };

    T(db.save(binAttDoc).ok);

    var xhr = CouchDB.request("GET", "/"+dbName+"/bin_doc/foo/bar.txt");
    T(xhr.responseText == "This is a base64 encoded text");
    T(xhr.getResponseHeader("Content-Type") == "text/plain");

    // lets try it with an escaped attachment id...
    // weird that it's at two urls
    var xhr = CouchDB.request("GET", "/"+dbName+"/bin_doc/foo%2Fbar.txt");
    T(xhr.status == 200);
    // xhr.responseText == "This is a base64 encoded text"

    var xhr = CouchDB.request("GET", "/"+dbName+"/bin_doc/foo/baz.txt");
    T(xhr.status == 404);

    var xhr = CouchDB.request("GET", "/"+dbName+"/bin_doc/foo%252Fbaz.txt");
    T(xhr.status == 200);
    T(xhr.responseText == "We like percent two F.");

    // require a _rev to PUT
    var xhr = CouchDB.request("PUT", "/"+dbName+"/bin_doc/foo/attachment.txt", {
      headers:{"Content-Type":"text/plain;charset=utf-8"},
      body:"Just some text"
    });
    T(xhr.status == 409);

    var xhr = CouchDB.request("PUT", "/"+dbName+"/bin_doc/foo/bar2.txt?rev=" + binAttDoc._rev, {
      body:"This is no base64 encoded text",
      headers:{"Content-Type": "text/plain;charset=utf-8"}
    });
    T(xhr.status == 201);
    var rev = JSON.parse(xhr.responseText).rev;

    binAttDoc = db.open("bin_doc");

    T(binAttDoc._attachments["foo/bar.txt"] !== undefined);
    T(binAttDoc._attachments["foo%2Fbaz.txt"] !== undefined);
    T(binAttDoc._attachments["foo/bar2.txt"] !== undefined);
    TEquals("text/plain;charset=utf-8",                   // thank you Safari
      binAttDoc._attachments["foo/bar2.txt"].content_type.toLowerCase(),
      "correct content-type"
    );
    T(binAttDoc._attachments["foo/bar2.txt"].length == 30);

    //// now repeat the while thing with a design doc

    // first just save a regular doc with an attachment that has a slash in the url.
    // (also gonna run an encoding check case)
    var binAttDoc = {
      _id: "_design/bin_doc",
      _attachments:{
        "foo/bar.txt": {
          content_type:"text/plain",
          data: "VGhpcyBpcyBhIGJhc2U2NCBlbmNvZGVkIHRleHQ="
        },
        "foo%2Fbaz.txt": {
          content_type:"text/plain",
          data: "V2UgbGlrZSBwZXJjZW50IHR3byBGLg=="
        }
      }
    };

    T(db.save(binAttDoc).ok);

    var xhr = CouchDB.request("GET", "/"+dbName+"/_design%2Fbin_doc/foo/bar.txt");
    T(xhr.responseText == "This is a base64 encoded text");
    T(xhr.getResponseHeader("Content-Type") == "text/plain");

    // lets try it with an escaped attachment id...
    // weird that it's at two urls
    var xhr = CouchDB.request("GET", "/"+dbName+"/_design%2Fbin_doc/foo%2Fbar.txt");
    T(xhr.responseText == "This is a base64 encoded text");
    T(xhr.status == 200);

    // err, 3 urls
    var xhr = CouchDB.request("GET", "/"+dbName+"/_design/bin_doc/foo%2Fbar.txt");
    T(xhr.responseText == "This is a base64 encoded text");
    T(xhr.status == 200);

    // I mean um, 4 urls
    var xhr = CouchDB.request("GET", "/"+dbName+"/_design/bin_doc/foo/bar.txt");
    T(xhr.responseText == "This is a base64 encoded text");
    T(xhr.status == 200);

    var xhr = CouchDB.request("GET", "/"+dbName+"/_design%2Fbin_doc/foo/baz.txt");
    T(xhr.status == 404);

    var xhr = CouchDB.request("GET", "/"+dbName+"/_design%2Fbin_doc/foo%252Fbaz.txt");
    T(xhr.status == 200);
    T(xhr.responseText == "We like percent two F.");

    // require a _rev to PUT
    var xhr = CouchDB.request("PUT", "/"+dbName+"/_design%2Fbin_doc/foo/attachment.txt", {
      headers:{"Content-Type":"text/plain;charset=utf-8"},
      body:"Just some text"
    });
    T(xhr.status == 409);

    var xhr = CouchDB.request("PUT", "/"+dbName+"/_design%2Fbin_doc/foo/bar2.txt?rev=" + binAttDoc._rev, {
      body:"This is no base64 encoded text",
      headers:{"Content-Type": "text/plain;charset=utf-8"}
    });
    T(xhr.status == 201);
    var rev = JSON.parse(xhr.responseText).rev;

    binAttDoc = db.open("_design/bin_doc");

    T(binAttDoc._attachments["foo/bar.txt"] !== undefined);
    T(binAttDoc._attachments["foo/bar2.txt"] !== undefined);
    TEquals("text/plain;charset=utf-8",                   // thank you Safari
      binAttDoc._attachments["foo/bar2.txt"].content_type.toLowerCase(),
      "correct content-type"
    );
    T(binAttDoc._attachments["foo/bar2.txt"].length == 30);
  }
};
