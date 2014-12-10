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

couchTests.view_offsets = function(debug) {
  if (debug) debugger;

  var db = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"false"});
  db.deleteDb();
  db.createDb();

  var designDoc = {
    _id : "_design/test",
    views : {
      offset : {
        map : "function(doc) { emit([doc.letter, doc.number], doc); }",
      }
    }
  };
  T(db.save(designDoc).ok);

  var docs = [
    {_id : "a1", letter : "a", number : 1, foo: "bar"},
    {_id : "a2", letter : "a", number : 2, foo: "bar"},
    {_id : "a3", letter : "a", number : 3, foo: "bar"},
    {_id : "b1", letter : "b", number : 1, foo: "bar"},
    {_id : "b2", letter : "b", number : 2, foo: "bar"},
    {_id : "b3", letter : "b", number : 3, foo: "bar"},
    {_id : "b4", letter : "b", number : 4, foo: "bar"},
    {_id : "b5", letter : "b", number : 5, foo: "bar"},
    {_id : "c1", letter : "c", number : 1, foo: "bar"},
    {_id : "c2", letter : "c", number : 2, foo: "bar"},
  ];
  db.bulkSave(docs);

  var check = function(startkey, offset) {
    var opts = {startkey: startkey, descending: true};
    T(db.view("test/offset", opts).offset == offset);
  };

  [
      [["c", 2], 0],
      [["c", 1], 1],
      [["b", 5], 2],
      [["b", 4], 3],
      [["b", 3], 4],
      [["b", 2], 5],
      [["b", 1], 6],
      [["a", 3], 7],
      [["a", 2], 8],
      [["a", 1], 9]
  ].forEach(function(row){ check(row[0], row[1]);});

  var runTest = function () {
    var db = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"false"});
    db.deleteDb();
    db.createDb();

    var designDoc = {
      _id : "_design/test",
      views : {
        offset : {
          map : "function(doc) { emit([doc.letter, doc.number], doc);}",
        }
      }
    };
    T(db.save(designDoc).ok);

    var docs = [
      {_id : "a1", letter : "a", number : 1, foo : "bar"},
      {_id : "a2", letter : "a", number : 2, foo : "bar"},
      {_id : "a3", letter : "a", number : 3, foo : "bar"},
      {_id : "b1", letter : "b", number : 1, foo : "bar"},
      {_id : "b2", letter : "b", number : 2, foo : "bar"},
      {_id : "b3", letter : "b", number : 3, foo : "bar"},
      {_id : "b4", letter : "b", number : 4, foo : "bar"},
      {_id : "b5", letter : "b", number : 5, foo : "bar"},
      {_id : "c1", letter : "c", number : 1, foo : "bar"},
      {_id : "c2", letter : "c", number : 2, foo : "bar"}
    ];
    db.bulkSave(docs);

    var res1 = db.view("test/offset", {
      startkey: ["b",4], startkey_docid: "b4", endkey: ["b"],
      limit: 2, descending: true, skip: 1
    })

    var res2 = db.view("test/offset", {startkey: ["c", 3]});
    var res3 = db.view("test/offset", {
        startkey: ["b", 6],
        endkey: ["b", 7]
    });

    return res1.offset == 4 && res2.offset == docs.length && res3.offset == 8;

  };

  for(var i = 0; i < 15; i++) T(runTest());
}

