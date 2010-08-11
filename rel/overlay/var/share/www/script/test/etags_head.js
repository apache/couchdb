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

couchTests.etags_head = function(debug) {
  var db = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"false"});
  db.deleteDb();
  db.createDb();
  if (debug) debugger;

  var xhr;

  // create a new doc
  xhr = CouchDB.request("PUT", "/test_suite_db/1", {
    body: "{}"
  });
  T(xhr.status == 201);

  // extract the ETag header values
  var etag = xhr.getResponseHeader("etag");

  // get the doc and verify the headers match
  xhr = CouchDB.request("GET", "/test_suite_db/1");
  T(etag == xhr.getResponseHeader("etag"));

  // 'head' the doc and verify the headers match
  xhr = CouchDB.request("HEAD", "/test_suite_db/1", {
    headers: {"if-none-match": "s"}
  });
  T(etag == xhr.getResponseHeader("etag"));

  // replace a doc
  xhr = CouchDB.request("PUT", "/test_suite_db/1", {
    body: "{}",
    headers: {"if-match": etag}
  });
  T(xhr.status == 201);

  // extract the new ETag value
  var etagOld= etag;
  etag = xhr.getResponseHeader("etag");

  // fail to replace a doc
  xhr = CouchDB.request("PUT", "/test_suite_db/1", {
    body: "{}"
  });
  T(xhr.status == 409);

  // verify get w/Etag
  xhr = CouchDB.request("GET", "/test_suite_db/1", {
    headers: {"if-none-match": etagOld}
  });
  T(xhr.status == 200);
  xhr = CouchDB.request("GET", "/test_suite_db/1", {
    headers: {"if-none-match": etag}
  });
  T(xhr.status == 304);

  // fail to delete a doc
  xhr = CouchDB.request("DELETE", "/test_suite_db/1", {
    headers: {"if-match": etagOld}
  });
  T(xhr.status == 409);

  //now do it for real
  xhr = CouchDB.request("DELETE", "/test_suite_db/1", {
    headers: {"if-match": etag}
  });
  T(xhr.status == 200);
};
