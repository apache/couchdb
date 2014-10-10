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

couchTests.multiple_rows = function(debug) {
  var db = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"false"});
  db.deleteDb();
  db.createDb();
  if (debug) debugger;

  var nc = {_id:"NC", cities:["Charlotte", "Raleigh"]};
  var ma = {_id:"MA", cities:["Boston", "Lowell", "Worcester", "Cambridge", "Springfield"]};
  var fl = {_id:"FL", cities:["Miami", "Tampa", "Orlando", "Springfield"]};

  T(db.save(nc).ok);
  T(db.save(ma).ok);
  T(db.save(fl).ok);

  var generateListOfCitiesAndState = "function(doc) {" +
  " for (var i = 0; i < doc.cities.length; i++)" +
  "  emit(doc.cities[i] + \", \" + doc._id, null);" +
  "}";

  var results = db.query(generateListOfCitiesAndState);
  var rows = results.rows;

  T(rows[0].key == "Boston, MA");
  T(rows[1].key == "Cambridge, MA");
  T(rows[2].key == "Charlotte, NC");
  T(rows[3].key == "Lowell, MA");
  T(rows[4].key == "Miami, FL");
  T(rows[5].key == "Orlando, FL");
  T(rows[6].key == "Raleigh, NC");
  T(rows[7].key == "Springfield, FL");
  T(rows[8].key == "Springfield, MA");
  T(rows[9].key == "Tampa, FL");
  T(rows[10].key == "Worcester, MA");

  // add another city to NC
  nc.cities.push("Wilmington");
  T(db.save(nc).ok);

  var results = db.query(generateListOfCitiesAndState);
  var rows = results.rows;

  T(rows[0].key == "Boston, MA");
  T(rows[1].key == "Cambridge, MA");
  T(rows[2].key == "Charlotte, NC");
  T(rows[3].key == "Lowell, MA");
  T(rows[4].key == "Miami, FL");
  T(rows[5].key == "Orlando, FL");
  T(rows[6].key == "Raleigh, NC");
  T(rows[7].key == "Springfield, FL");
  T(rows[8].key == "Springfield, MA");
  T(rows[9].key == "Tampa, FL");
  T(rows[10].key == "Wilmington, NC");
  T(rows[11].key == "Worcester, MA");

  // now delete MA
  T(db.deleteDoc(ma).ok);

  var results = db.query(generateListOfCitiesAndState);
  var rows = results.rows;

  T(rows[0].key == "Charlotte, NC");
  T(rows[1].key == "Miami, FL");
  T(rows[2].key == "Orlando, FL");
  T(rows[3].key == "Raleigh, NC");
  T(rows[4].key == "Springfield, FL");
  T(rows[5].key == "Tampa, FL");
  T(rows[6].key == "Wilmington, NC");
};
