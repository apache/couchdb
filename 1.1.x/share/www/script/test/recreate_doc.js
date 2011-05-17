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

couchTests.recreate_doc = function(debug) {
  var db = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"false"});
  db.deleteDb();
  db.createDb();
  if (debug) debugger;

  // First create a new document with the ID "foo", and delete it again
  var doc = {_id: "foo", a: "bar", b: 42};
  var result = db.save(doc);
  T(result.ok);
  var firstRev = result.rev;
  T(db.deleteDoc(doc).ok);

  // Now create a new document with the same ID, save it, and then modify it
  for (var i = 0; i < 10; i++) {
    doc = {_id: "foo"};
    T(db.save(doc).ok);
    doc = db.open("foo");
    doc.a = "baz";
    T(db.save(doc).ok);
    T(db.deleteDoc(doc).rev != undefined);
  }

  try {
    // COUCHDB-292 now attempt to save the document with a prev that's since
    // been deleted and this should generate a conflict exception
    db.save({_id:"foo", _rev:firstRev, bar:1});
    T("no save conflict 1" && false); // we shouldn't hit here
  } catch (e) {
    T(e.error == "conflict");
  }
  
  var binAttDoc = {
    _id: "foo",
    _rev:firstRev,
    _attachments:{
      "foo.txt": {
        content_type:"text/plain",
        data: "VGhpcyBpcyBhIGJhc2U2NCBlbmNvZGVkIHRleHQ="
      }
    }
  };
  try {
    // same as before, but with binary
    db.save(binAttDoc);
    T("no save conflict 2" && false); // we shouldn't hit here
  } catch (e) {
    T(e.error == "conflict");
  }


  try {
    // random non-existant prev rev
    db.save({_id:"foo", _rev:"1-asfafasdf", bar:1});
    T("no save conflict 3" && false); // we shouldn't hit here
  } catch (e) {
    T(e.error == "conflict");
  }
  
  try {
    // random non-existant prev rev with bin
    binAttDoc._rev = "1-aasasfasdf";
    db.save(binAttDoc);
    T("no save conflict 4" && false); // we shouldn't hit here
  } catch (e) {
    T(e.error == "conflict");
  }
};
