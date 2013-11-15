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

couchTests.attachment_views= function(debug) {

  var db = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"false"});
  db.deleteDb();
  db.createDb();
  if (debug) debugger;

  // count attachments in a view

  var attachmentData = "VGhpcyBpcyBhIGJhc2U2NCBlbmNvZGVkIHRleHQ=";

  db.bulkSave(makeDocs(0, 10));

  db.bulkSave(makeDocs(10, 20, {
    _attachments:{
      "foo.txt": {
        content_type:"text/plain",
        data: attachmentData
      }
    }
  }));

  db.bulkSave(makeDocs(20, 30, {
    _attachments:{
      "foo.txt": {
        content_type:"text/plain",
        data: attachmentData
      },
      "bar.txt": {
        content_type:"text/plain",
        data: attachmentData
      }
    }
  }));

  db.bulkSave(makeDocs(30, 40, {
    _attachments:{
      "foo.txt": {
        content_type:"text/plain",
        data: attachmentData
      },
      "bar.txt": {
        content_type:"text/plain",
        data: attachmentData
      },
      "baz.txt": {
        content_type:"text/plain",
        data: attachmentData
      }
    }
  }));

  var mapFunction = function(doc) {
    var count = 0;

    for(var idx in doc._attachments) {
      count = count + 1;
    }

    emit(parseInt(doc._id), count);
  };

  var reduceFunction = function(key, values) {
    return sum(values);
  };

  var result = db.query(mapFunction, reduceFunction);

  T(result.rows.length == 1);
  T(result.rows[0].value == 60);

  var result = db.query(mapFunction, reduceFunction, {
    startkey:10,
    endkey:19
  });

  T(result.rows.length == 1);
  T(result.rows[0].value == 10);

  var result = db.query(mapFunction, reduceFunction, {
    startkey:20,
    endkey:29
  });

  T(result.rows.length == 1);
  T(result.rows[0].value == 20);

  var result = db.query(mapFunction, null, {
    startkey: 30,
    endkey: 39,
    include_docs: true
  });

  T(result.rows.length == 10);
  T(result.rows[0].value == 3);
  T(result.rows[0].doc._attachments['baz.txt'].stub === true);
  T(result.rows[0].doc._attachments['baz.txt'].data === undefined);
  T(result.rows[0].doc._attachments['baz.txt'].encoding === undefined);
  T(result.rows[0].doc._attachments['baz.txt'].encoded_length === undefined);

  var result = db.query(mapFunction, null, {
    startkey: 30,
    endkey: 39,
    include_docs: true,
    attachments: true
  });

  T(result.rows.length == 10);
  T(result.rows[0].value == 3);
  T(result.rows[0].doc._attachments['baz.txt'].data === attachmentData);
  T(result.rows[0].doc._attachments['baz.txt'].stub === undefined);
  T(result.rows[0].doc._attachments['baz.txt'].encoding === undefined);
  T(result.rows[0].doc._attachments['baz.txt'].encoded_length === undefined);

  var result = db.query(mapFunction, null, {
    startkey: 30,
    endkey: 39,
    include_docs: true,
    att_encoding_info: true
  });

  T(result.rows.length == 10);
  T(result.rows[0].value == 3);
  T(result.rows[0].doc._attachments['baz.txt'].data === undefined);
  T(result.rows[0].doc._attachments['baz.txt'].stub === true);
  T(result.rows[0].doc._attachments['baz.txt'].encoding === "gzip");
  T(result.rows[0].doc._attachments['baz.txt'].encoded_length === 47);
};
