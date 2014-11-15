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

// test basic coffeescript functionality
couchTests.coffee = function(debug) {
  var db = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"false"});
  db.deleteDb();
  db.createDb();
  if (debug) debugger;

  var ddoc = {
    _id: "_design/coffee",
    language: "coffeescript",
    views: {
      myview: {
        map: '(doc) -> if doc.foo\n  emit(doc.foo, 1)',
        reduce: '(keys, values, rereduce) ->\n  sum = 0\n  for x in values\n    sum = sum + x\n  sum'
      }
    },
    shows: {
      myshow: '(doc) ->\n  "Foo #{doc.foo}"'
    },
    lists: {
      mylist: '(head, req) ->\n  while row = getRow()\n    send("Foo #{row.value}")\n  return "Foo"'
    },
    filters: {
      filter: "(doc) ->\n  doc.foo"
    }
  };

  db.save(ddoc);

  var docs = [
    {_id:"a", foo: 100},
    {foo:1},
    {foo:1},
    {foo:2},
    {foo:2},
    {bar:1},
    {bar:1},
    {bar:2},
    {bar:2}
  ];

  db.bulkSave(docs);

  var res = db.view("coffee/myview");
  TEquals(5, res.rows[0].value, "should sum up values");

  var res = CouchDB.request("GET", "/" + db.name + "/_design/coffee/_show/myshow/a");
  TEquals("Foo 100", res.responseText, "should show 100");

  var res = CouchDB.request("GET", "/" + db.name + "/_design/coffee/_list/mylist/myview");
  TEquals("Foo 5Foo", res.responseText, "should list");

  var changes = db.changes({filter: "coffee/filter"});
  TEquals(5, changes.results.length, "should have changes");
};
