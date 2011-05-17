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

couchTests.utf8 = function(debug) {
  var db = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"false"});
  db.deleteDb();
  db.createDb();
  if (debug) debugger;

  var texts = [];

  texts[0] = "1. Ascii: hello"
  texts[1] = "2. Russian: На берегу пустынных волн"
  texts[2] = "3. Math: ∮ E⋅da = Q,  n → ∞, ∑ f(i) = ∏ g(i),"
  texts[3] = "4. Geek: STARGΛ̊TE SG-1"
  texts[4] = "5. Braille: ⡌⠁⠧⠑ ⠼⠁⠒  ⡍⠜⠇⠑⠹⠰⠎ ⡣⠕⠌"

  // check that we can save a reload with full fidelity
  for (var i=0; i<texts.length; i++) {
    T(db.save({_id:i.toString(), text:texts[i]}).ok);
  }

  for (var i=0; i<texts.length; i++) {
    T(db.open(i.toString()).text == texts[i]);
  }

  // check that views and key collation don't blow up
  var rows = db.query(function(doc) { emit(null, doc.text) }).rows;
  for (var i=0; i<texts.length; i++) {
    T(rows[i].value == texts[i]);
  }
};
