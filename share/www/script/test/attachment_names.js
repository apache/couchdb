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

couchTests.attachment_names = function(debug) {
  var db = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"false"});
  db.deleteDb();
  db.createDb();
  if (debug) debugger;

  var binAttDoc = {
    _id: "bin_doc",
    _attachments:{
      "foo\x80txt": {
        content_type:"text/plain",
        data: "VGhpcyBpcyBhIGJhc2U2NCBlbmNvZGVkIHRleHQ="
      }
    }
  }

  // inline attachments
  try {
    db.save(binAttDoc);
    TEquals(1, 2, "Attachment name with non UTF-8 encoding saved. Should never show!");
  } catch (e) {
    TEquals("bad_request", e.error, "attachment_name: inline attachments");
    TEquals("Attachment name is not UTF-8 encoded", e.reason, "attachment_name: inline attachments");
  }


  // standalone docs
  var bin_data = "JHAPDO*AU£PN ){(3u[d 93DQ9¡€])}    ææøo'∂ƒæ≤çæππ•¥∫¶®#†π¶®¥π€ª®˙π8np";

  var xhr = (CouchDB.request("PUT", "/test_suite_db/bin_doc3/attachment\x80txt", {
    headers:{"Content-Type":"text/plain;charset=utf-8"},
    body:bin_data
  }));

  var resp = JSON.parse(xhr.responseText);
  TEquals(400, xhr.status, "attachment_name: standalone API");
  TEquals("bad_request", resp.error, "attachment_name: standalone API");
  TEquals("Attachment name is not UTF-8 encoded", resp.reason, "attachment_name: standalone API");


  // bulk docs
  var docs = { docs: [binAttDoc] };

  var xhr = CouchDB.request("POST", "/test_suite_db/_bulk_docs", {
    body: JSON.stringify(docs)
  });

  var resp = JSON.parse(xhr.responseText);
  TEquals(400, xhr.status, "attachment_name: bulk docs");
  TEquals("bad_request", resp.error, "attachment_name: bulk docs");
  TEquals("Attachment name is not UTF-8 encoded", resp.reason, "attachment_name: bulk docs");


  // leading underscores
  var binAttDoc = {
    _id: "bin_doc2",
    _attachments:{
      "_foo.txt": {
        content_type:"text/plain",
        data: "VGhpcyBpcyBhIGJhc2U2NCBlbmNvZGVkIHRleHQ="
      }
    }
  }

  try {
    db.save(binAttDoc);
    TEquals(1, 2, "Attachment name with leading underscore saved. Should never show!");
  } catch (e) {
    TEquals("bad_request", e.error, "attachment_name: leading underscore");
    TEquals("Attachment name can't start with '_'", e.reason, "attachment_name: leading underscore");
  }

  // todo: form uploads, waiting for cmlenz' test case for form uploads

};
