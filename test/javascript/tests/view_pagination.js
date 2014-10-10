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

couchTests.view_pagination = function(debug) {
    var db = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"false"});
    db.deleteDb();
    db.createDb();
    if (debug) debugger;

    var docs = makeDocs(0, 100);
    db.bulkSave(docs);

    var queryFun = function(doc) { emit(doc.integer, null); };
    var i;

    // page through the view ascending
    for (i = 0; i < docs.length; i += 10) {
      var queryResults = db.query(queryFun, null, {
        startkey: i,
        startkey_docid: i,
        limit: 10
      });
      T(queryResults.rows.length == 10);
      T(queryResults.total_rows == docs.length);
      T(queryResults.offset == i);
      var j;
      for (j = 0; j < 10;j++) {
        T(queryResults.rows[j].key == i + j);
      }

      // test aliases start_key and start_key_doc_id
      queryResults = db.query(queryFun, null, {
        start_key: i,
        start_key_doc_id: i,
        limit: 10
      });
      T(queryResults.rows.length == 10);
      T(queryResults.total_rows == docs.length);
      T(queryResults.offset == i);
      for (j = 0; j < 10;j++) {
        T(queryResults.rows[j].key == i + j);
      }
    }

    // page through the view descending
    for (i = docs.length - 1; i >= 0; i -= 10) {
      var queryResults = db.query(queryFun, null, {
        startkey: i,
        startkey_docid: i,
        descending: true,
        limit: 10
      });
      T(queryResults.rows.length == 10);
      T(queryResults.total_rows == docs.length);
      T(queryResults.offset == docs.length - i - 1);
      var j;
      for (j = 0; j < 10; j++) {
        T(queryResults.rows[j].key == i - j);
      }
    }

    // ignore decending=false. CouchDB should just ignore that.
    for (i = 0; i < docs.length; i += 10) {
      var queryResults = db.query(queryFun, null, {
        startkey: i,
        startkey_docid: i,
        descending: false,
        limit: 10
      });
      T(queryResults.rows.length == 10);
      T(queryResults.total_rows == docs.length);
      T(queryResults.offset == i);
      var j;
      for (j = 0; j < 10;j++) {
        T(queryResults.rows[j].key == i + j);
      }
    }

    function testEndkeyDocId(queryResults) {
      T(queryResults.rows.length == 35);
      T(queryResults.total_rows == docs.length);
      T(queryResults.offset == 1);
      T(queryResults.rows[0].id == "1");
      T(queryResults.rows[1].id == "10");
      T(queryResults.rows[2].id == "11");
      T(queryResults.rows[3].id == "12");
      T(queryResults.rows[4].id == "13");
      T(queryResults.rows[5].id == "14");
      T(queryResults.rows[6].id == "15");
      T(queryResults.rows[7].id == "16");
      T(queryResults.rows[8].id == "17");
      T(queryResults.rows[9].id == "18");
      T(queryResults.rows[10].id == "19");
      T(queryResults.rows[11].id == "2");
      T(queryResults.rows[12].id == "20");
      T(queryResults.rows[13].id == "21");
      T(queryResults.rows[14].id == "22");
      T(queryResults.rows[15].id == "23");
      T(queryResults.rows[16].id == "24");
      T(queryResults.rows[17].id == "25");
      T(queryResults.rows[18].id == "26");
      T(queryResults.rows[19].id == "27");
      T(queryResults.rows[20].id == "28");
      T(queryResults.rows[21].id == "29");
      T(queryResults.rows[22].id == "3");
      T(queryResults.rows[23].id == "30");
      T(queryResults.rows[24].id == "31");
      T(queryResults.rows[25].id == "32");
      T(queryResults.rows[26].id == "33");
      T(queryResults.rows[27].id == "34");
      T(queryResults.rows[28].id == "35");
      T(queryResults.rows[29].id == "36");
      T(queryResults.rows[30].id == "37");
      T(queryResults.rows[31].id == "38");
      T(queryResults.rows[32].id == "39");
      T(queryResults.rows[33].id == "4");
      T(queryResults.rows[34].id == "40");
    }

    // test endkey_docid
    var queryResults = db.query(function(doc) { emit(null, null); }, null, {
      startkey: null,
      startkey_docid: 1,
      endkey: null,
      endkey_docid: 40
    });
    testEndkeyDocId(queryResults);

    // test aliases end_key_doc_id and end_key
    queryResults = db.query(function(doc) { emit(null, null); }, null, {
      start_key: null,
      start_key_doc_id: 1,
      end_key: null,
      end_key_doc_id: 40
    });
    testEndkeyDocId(queryResults);

  };
