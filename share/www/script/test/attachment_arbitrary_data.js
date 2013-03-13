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

couchTests.attachment_arbitrary_data = function(debug) {

  var db = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"false"});
  db.deleteDb();
  db.createDb();

  // test COUCHDB-259 - allow arbitrary data to be stored along with the attachment
  var bin_doc9 = {
    _id: "bin_doc9",
    name: "Don Draper",
    _attachments:{
      "foo.txt": {
        "content_type":"text/plain",
        "data": "VGhpcyBpcyBhIGJhc2U2NCBlbmNvZGVkIHRleHQ=",
        "field1": "I am a simple string.",
        "field2": 1234,
        "field3": [1234, "string", null, {"hey":"ho", "tic":"tac"}]
      },
      "foo2.txt": {
        "content_type":"text/plain",
        "data": "SGV5IHRoZXJlCg=="
      }
    }
  };

  TEquals(true, db.save(bin_doc9).ok);
  bin_doc9 = db.open("bin_doc9");

  var stub_data9 = bin_doc9._attachments["foo.txt"];
  TEquals("I am a simple string.", stub_data9.field1);
  TEquals(1234, stub_data9.field2);

  TEquals(4, stub_data9.field3.length);
  TEquals(1234, stub_data9.field3[0]);
  TEquals("string", stub_data9.field3[1]);
  TEquals(null, stub_data9.field3[2]);
  TEquals("tac", stub_data9.field3[3].tic);


  // update a field via stub structure
  var oldRev = bin_doc9._rev;
  stub_data9.field1 = "I am a better string.";
  delete stub_data9.field2;
  bin_doc9._attachments["foo.txt"] = stub_data9;
  TEquals(true, db.save(bin_doc9).ok);
  bin_doc9 = db.open("bin_doc9");
  TEquals("I am a better string.", bin_doc9._attachments["foo.txt"].field1);
  // has the revision changed properly?
  T(bin_doc9._rev != oldRev);
  // is the deleted field2 gone?
  TEquals(undefined, bin_doc9._attachments["foo.txt"].field2);

  // remove the attachment
  delete bin_doc9._attachments["foo.txt"];
  TEquals(true, db.save(bin_doc9).ok);
  bin_doc9 = db.open("bin_doc9");
  TEquals(undefined, bin_doc9._attachments["foo.txt"]);

};
