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

couchTests.attachments= function(debug) {
  var db = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"false"});
  db.deleteDb();
  db.createDb();
  if (debug) debugger;


  // MD5 Digests of compressible attachments and therefore Etags
  // will vary depending on platform gzip implementation.
  // These MIME types are defined in [attachments] compressible_types
  var binAttDoc = {
    _id: "bin_doc",
    _attachments:{
      "foo.txt": {
        content_type:"application/octet-stream",
        data: "VGhpcyBpcyBhIGJhc2U2NCBlbmNvZGVkIHRleHQ="
      }
    }
  };

  var save_response = db.save(binAttDoc);
  T(save_response.ok);

  var xhr = CouchDB.request("GET", "/test_suite_db/bin_doc/foo.txt");
  T(xhr.responseText == "This is a base64 encoded text");
  T(xhr.getResponseHeader("Content-Type") == "application/octet-stream");
  TEquals("\"aEI7pOYCRBLTRQvvqYrrJQ==\"", xhr.getResponseHeader("Etag"));

  // empty attachment
  var binAttDoc2 = {
    _id: "bin_doc2",
    _attachments:{
      "foo.txt": {
        content_type:"text/plain",
        data: ""
      }
    }
  }

  T(db.save(binAttDoc2).ok);

  var xhr = CouchDB.request("GET", "/test_suite_db/bin_doc2/foo.txt");
  T(xhr.responseText.length == 0);
  T(xhr.getResponseHeader("Content-Type") == "text/plain");

  // test RESTful doc API

  var xhr = CouchDB.request("PUT", "/test_suite_db/bin_doc2/foo2.txt?rev=" + binAttDoc2._rev, {
    body:"This is no base64 encoded text",
    headers:{"Content-Type": "text/plain;charset=utf-8"}
  });
  T(xhr.status == 201);
  TEquals("/bin_doc2/foo2.txt",
    xhr.getResponseHeader("Location").substr(-18),
    "should return Location header to newly created or updated attachment");

  var rev = JSON.parse(xhr.responseText).rev;

  binAttDoc2 = db.open("bin_doc2");

  T(binAttDoc2._attachments["foo.txt"] !== undefined);
  T(binAttDoc2._attachments["foo2.txt"] !== undefined);
  TEqualsIgnoreCase("text/plain;charset=utf-8", binAttDoc2._attachments["foo2.txt"].content_type);
  T(binAttDoc2._attachments["foo2.txt"].length == 30);

  var xhr = CouchDB.request("GET", "/test_suite_db/bin_doc2/foo2.txt");
  T(xhr.responseText == "This is no base64 encoded text");
  TEqualsIgnoreCase("text/plain;charset=utf-8", xhr.getResponseHeader("Content-Type"));

  // test without rev, should fail
  var xhr = CouchDB.request("DELETE", "/test_suite_db/bin_doc2/foo2.txt");
  T(xhr.status == 409);

  // test with rev, should not fail
  var xhr = CouchDB.request("DELETE", "/test_suite_db/bin_doc2/foo2.txt?rev=" + rev);
  T(xhr.status == 200);
  TEquals(null, xhr.getResponseHeader("Location"),
    "should not return Location header on DELETE request");

  // test binary data
  var bin_data = "JHAPDO*AU£PN ){(3u[d 93DQ9¡€])}    ææøo'∂ƒæ≤çæππ•¥∫¶®#†π¶®¥π€ª®˙π8np";
  var xhr = CouchDB.request("PUT", "/test_suite_db/bin_doc3/attachment.txt", {
    headers:{"Content-Type":"text/plain;charset=utf-8"},
    body:bin_data
  });
  T(xhr.status == 201);
  var rev = JSON.parse(xhr.responseText).rev;
  TEquals('"' + rev + '"', xhr.getResponseHeader("Etag"));

  var xhr = CouchDB.request("GET", "/test_suite_db/bin_doc3/attachment.txt");
  T(xhr.responseText == bin_data);
  TEqualsIgnoreCase("text/plain;charset=utf-8", xhr.getResponseHeader("Content-Type"));

  // without rev
  var xhr = CouchDB.request("PUT", "/test_suite_db/bin_doc3/attachment.txt", {
    headers:{"Content-Type":"text/plain;charset=utf-8"},
    body:bin_data
  });
  T(xhr.status == 409);

  // with nonexistent rev
  var xhr = CouchDB.request("PUT", "/test_suite_db/bin_doc3/attachment.txt"  + "?rev=1-adae8575ecea588919bd08eb020c708e", {
    headers:{"Content-Type":"text/plain;charset=utf-8"},
    body:bin_data
  });
  T(xhr.status == 409);

  // with current rev
  var xhr = CouchDB.request("PUT", "/test_suite_db/bin_doc3/attachment.txt?rev=" + rev, {
    headers:{"Content-Type":"text/plain;charset=utf-8"},
    body:bin_data
  });
  T(xhr.status == 201);
  var rev = JSON.parse(xhr.responseText).rev;
  TEquals('"' + rev + '"', xhr.getResponseHeader("Etag"));

  var xhr = CouchDB.request("GET", "/test_suite_db/bin_doc3/attachment.txt");
  T(xhr.responseText == bin_data);
  TEqualsIgnoreCase("text/plain;charset=utf-8", xhr.getResponseHeader("Content-Type"));

  var xhr = CouchDB.request("GET", "/test_suite_db/bin_doc3/attachment.txt?rev=" + rev);
  T(xhr.responseText == bin_data);
  TEqualsIgnoreCase("text/plain;charset=utf-8", xhr.getResponseHeader("Content-Type"));

  var xhr = CouchDB.request("DELETE", "/test_suite_db/bin_doc3/attachment.txt?rev=" + rev);
  T(xhr.status == 200);

  var xhr = CouchDB.request("GET", "/test_suite_db/bin_doc3/attachment.txt");
  T(xhr.status == 404);

  // deleted attachment is still accessible with revision
  var xhr = CouchDB.request("GET", "/test_suite_db/bin_doc3/attachment.txt?rev=" + rev);
  T(xhr.status == 200);
  T(xhr.responseText == bin_data);
  TEqualsIgnoreCase("text/plain;charset=utf-8", xhr.getResponseHeader("Content-Type"));

  // empty attachments
  var xhr = CouchDB.request("PUT", "/test_suite_db/bin_doc4/attachment.txt", {
    headers:{"Content-Type":"text/plain;charset=utf-8"},
    body:""
  });
  T(xhr.status == 201);
  var rev = JSON.parse(xhr.responseText).rev;

  var xhr = CouchDB.request("GET", "/test_suite_db/bin_doc4/attachment.txt");
  T(xhr.status == 200);
  T(xhr.responseText.length == 0);

  // overwrite previsously empty attachment
  var xhr = CouchDB.request("PUT", "/test_suite_db/bin_doc4/attachment.txt?rev=" + rev, {
    headers:{"Content-Type":"text/plain;charset=utf-8"},
    body:"This is a string"
  });
  T(xhr.status == 201);

  var xhr = CouchDB.request("GET", "/test_suite_db/bin_doc4/attachment.txt");
  T(xhr.status == 200);
  T(xhr.responseText == "This is a string");

  // Attachment sparseness COUCHDB-220

  var docs = [];
  for (var i = 0; i < 5; i++) {
    var doc = {
      _id: (i).toString(),
      _attachments:{
        "foo.txt": {
          content_type:"text/plain",
          data: "VGhpcyBpcyBhIGJhc2U2NCBlbmNvZGVkIHRleHQ="
        }
      }
    };
    docs.push(doc);
  }

  var saved = db.bulkSave(docs);
  // now delete the docs, and while we are looping over them, remove the
  // '_rev' field so we can re-create after deletion.
  var to_up = [];
  for (i=0;i<saved.length;i++) {
    to_up.push({'_id': saved[i]['id'], '_rev': saved[i]['rev'], '_deleted': true});
    delete docs[i]._rev;
  }
  // delete them.
  var saved2 = db.bulkSave(to_up);
  // re-create them
  var saved3 = db.bulkSave(docs);

  var before = db.info().disk_size;

  // Compact it.
  T(db.compact().ok);
  T(db.last_req.status == 202);
  // compaction isn't instantaneous, loop until done
  while (db.info().compact_running) {};

  var after = db.info().disk_size;

  // Compaction should reduce the database slightly, but not
  // orders of magnitude (unless attachments introduce sparseness)
  T(after > before * 0.1, "before: " + before + " after: " + after);


  // test large attachments - COUCHDB-366
  var lorem = CouchDB.request("GET", "/_utils/script/test/lorem.txt").responseText;

  var xhr = CouchDB.request("PUT", "/test_suite_db/bin_doc5/lorem.txt", {
    headers:{"Content-Type":"text/plain;charset=utf-8"},
    body:lorem
  });
  T(xhr.status == 201);
  var rev = JSON.parse(xhr.responseText).rev;

  var xhr = CouchDB.request("GET", "/test_suite_db/bin_doc5/lorem.txt");
  T(xhr.responseText == lorem);
  TEqualsIgnoreCase("text/plain;charset=utf-8", xhr.getResponseHeader("Content-Type"));

  // test large inline attachment too
  var lorem_b64 = CouchDB.request("GET", "/_utils/script/test/lorem_b64.txt").responseText;
  var doc = db.open("bin_doc5", {attachments:true});
  T(doc._attachments["lorem.txt"].data == lorem_b64);

  // test etags for attachments.
  var xhr = CouchDB.request("GET", "/test_suite_db/bin_doc5/lorem.txt");
  T(xhr.status == 200);
  var etag = xhr.getResponseHeader("etag");
  xhr = CouchDB.request("GET", "/test_suite_db/bin_doc5/lorem.txt", {
    headers: {"if-none-match": etag}
  });
  T(xhr.status == 304);

  // test COUCHDB-497 - empty attachments
  var xhr = CouchDB.request("PUT", "/test_suite_db/bin_doc5/empty.txt?rev="+rev, {
    headers:{"Content-Type":"text/plain;charset=utf-8", "Content-Length": "0"},
    body:""
  });
  TEquals(201, xhr.status, "should send 201 Accepted");
  var rev = JSON.parse(xhr.responseText).rev;
  var xhr = CouchDB.request("PUT", "/test_suite_db/bin_doc5/empty.txt?rev="+rev, {
    headers:{"Content-Type":"text/plain;charset=utf-8"}
  });
  TEquals(201, xhr.status, "should send 201 Accepted");

  // implicit doc creation allows creating docs with a reserved id. COUCHDB-565
  var xhr = CouchDB.request("PUT", "/test_suite_db/_nonexistant/attachment.txt", {
    headers: {"Content-Type":"text/plain;charset=utf-8"},
    body: "THIS IS AN ATTACHMENT. BOOYA!"
  });
  TEquals(400, xhr.status, "should return error code 400 Bad Request");

  // test COUCHDB-809 - stubs should only require the 'stub' field
  var bin_doc6 = {
    _id: "bin_doc6",
    _attachments:{
      "foo.txt": {
        content_type:"text/plain",
        data: "VGhpcyBpcyBhIGJhc2U2NCBlbmNvZGVkIHRleHQ="
      }
    }
  };
  T(db.save(bin_doc6).ok);
  // stub out the attachment
  bin_doc6._attachments["foo.txt"] = { stub: true };
  T(db.save(bin_doc6).ok == true);

  // wrong rev pos specified
  
  // stub out the attachment with the wrong revpos
  bin_doc6._attachments["foo.txt"] = { stub: true, revpos: 10};
  try {
      T(db.save(bin_doc6).ok == true);
      T(false && "Shouldn't get here!");
  } catch (e) {
      T(e.error == "missing_stub");
  }

  // test MD5 header
  var bin_data = "foo bar"
  var xhr = CouchDB.request("PUT", "/test_suite_db/bin_doc7/attachment.txt", {
    headers:{"Content-Type":"application/octet-stream",
             "Content-MD5":"MntvB0NYESObxH4VRDUycw=="},
    body:bin_data
  });
  TEquals(201, xhr.status);

  var xhr = CouchDB.request("GET", "/test_suite_db/bin_doc7/attachment.txt");
  TEquals('MntvB0NYESObxH4VRDUycw==', xhr.getResponseHeader("Content-MD5"));

};
