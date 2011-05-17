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

couchTests.view_xml = function(debug) {
  var db = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"false"});
  db.deleteDb();
  db.createDb();
  if (debug) debugger;

  db.save({content: "<doc><title id='xml'>Testing XML</title></doc>"});
  db.save({content: "<doc><title id='e4x'>Testing E4X</title></doc>"});

  var results = db.query(
    "function(doc) {\n" +
    "  var xml = new XML(doc.content);\n" +
    "  emit(xml.title.text().toXMLString(), null);\n" +
    "}");
  T(results.total_rows == 2);
  T(results.rows[0].key == "Testing E4X");
  T(results.rows[1].key == "Testing XML");

  var results = db.query(
    "function(doc) {\n" +
    "  var xml = new XML(doc.content);\n" +
    "  emit(xml.title.@id.toXMLString(), null);\n" +
    "}");
  T(results.total_rows == 2);
  T(results.rows[0].key == "e4x");
  T(results.rows[1].key == "xml");
};
