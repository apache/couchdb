// Licensed under the Apache License, Version 2.0 (the "License"); you may not
// use this file except in compliance with the License.  You may obtain a copy
// of the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
// WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
// License for the specific language governing permissions and limitations under
// the License.

var tests = {

  // Do some basic tests.
  basics: function(debug) {
    var db = new CouchDB("test_suite_db");
    db.deleteDb();
    db.createDb();
    if (debug) debugger;

    // Get the database info, check the doc_count
    T(db.info().doc_count == 0);

    // create a document and save it to the database
    var doc = {_id:"0",a:1,b:1};
    var result = db.save(doc);

    T(result.ok==true); // return object has an ok member with a value true
    T(result.id); // the _id of the document is set.
    T(result.rev); // the revision id of the document is set.

    // Verify the input doc is now set with the doc id and rev
    // (for caller convenience).
    T(doc._id == result.id && doc._rev == result.rev);

    var id = result.id; // save off the id for later

    // Create some more documents.
    // Notice the use of the ok member on the return result.
    T(db.save({_id:"1",a:2,b:4}).ok);
    T(db.save({_id:"2",a:3,b:9}).ok);
    T(db.save({_id:"3",a:4,b:16}).ok);

    // Check the database doc count
    T(db.info().doc_count == 4);

    // Check the all docs
    var results = db.allDocs();
    var rows = results.rows;

    for(var i=0; i < rows.length; i++) {
      T(rows[i].id >= "0" && rows[i].id <= "4");
    }

    // Test a simple map functions

    // create a map function that selects all documents whose "a" member
    // has a value of 4, and then returns the document's b value.
    var mapFunction = function(doc){
      if (doc.a==4)
        emit(null, doc.b);
    };

    results = db.query(mapFunction);

    // verify only one document found and the result value (doc.b).
    T(results.total_rows == 1 && results.rows[0].value == 16);

    // reopen document we saved earlier
    existingDoc = db.open(id);

    T(existingDoc.a==1);

    //modify and save
    existingDoc.a=4;
    db.save(existingDoc);

    // redo the map query
    results = db.query(mapFunction);

    // the modified document should now be in the results.
    T(results.total_rows == 2);

    // write 2 more documents
    T(db.save({a:3,b:9}).ok);
    T(db.save({a:4,b:16}).ok);

    results = db.query(mapFunction);

    // 1 more document should now be in the result.
    T(results.total_rows == 3);
    T(db.info().doc_count == 6);

    var reduceFunction = function(keys, values){
      return sum(values);
    };

    results = db.query(mapFunction, reduceFunction);

    T(results.rows[0].value == 33);

    // delete a document
    T(db.deleteDoc(existingDoc).ok);

    // make sure we can't open the doc
    T(db.open(existingDoc._id) == null);

    results = db.query(mapFunction);

    // 1 less document should now be in the results.
    T(results.total_rows == 2);
    T(db.info().doc_count == 5);
  },

  // Do some edit conflict detection tests
  conflicts: function(debug) {
    var db = new CouchDB("test_suite_db");
    db.deleteDb();
    db.createDb();
    if (debug) debugger;

    // create a doc and save
    var doc = {_id:"foo",a:1,b:1};
    T(db.save(doc).ok);

    // reopen
    var doc2 = db.open(doc._id);

    // ensure the revisions are the same
    T(doc._id == doc2._id && doc._rev == doc2._rev);

    // edit the documents.
    doc.a = 2;
    doc2.a = 3;

    // save one document
    T(db.save(doc).ok);

    // save the other document
    try {
      db.save(doc2);  // this should generate a conflict exception
      T("no save conflict 1" && false); // we shouldn't hit here
    } catch (e) {
      T(e.error == "conflict");
    }

    // Now clear out the _rev member and save. This indicates this document is
    // new, not based on an existing revision.
    doc2._rev = undefined;
    try {
      db.save(doc2); // this should generate a conflict exception
      T("no save conflict 2" && false); // we shouldn't hit here
    } catch (e) {
      T(e.error == "conflict");
    }

    // Now delete the document from the database
    T(db.deleteDoc(doc).ok);

    T(db.save(doc2).ok);  // we can save a new document over a deletion without
                          // knowing the deletion rev.
  },

  recreate_doc: function(debug) {
    var db = new CouchDB("test_suite_db");
    db.deleteDb();
    db.createDb();
    if (debug) debugger;

    // First create a new document with the ID "foo", and delete it again
    var doc = {_id: "foo", a: "bar", b: 42};
    T(db.save(doc).ok);
    T(db.deleteDoc(doc).ok);

    // Now create a new document with the same ID, save it, and then modify it
    // This should work fine, but currently results in a conflict error, at
    // least "sometimes"
    for (var i = 0; i < 10; i++) {
      doc = {_id: "foo"};
      T(db.save(doc).ok);
      doc = db.open("foo");
      doc.a = "baz";
      try {
        T(db.save(doc).ok);
      } finally {
        // And now, we can't even delete the document anymore :/
        T(db.deleteDoc(doc).rev != undefined);
      }
    }
  },

  bulk_docs: function(debug) {
    var db = new CouchDB("test_suite_db");
    db.deleteDb();
    db.createDb();
    if (debug) debugger;

    var docs = makeDocs(5);

    // Create the docs
    var result = db.bulkSave(docs);
    T(result.ok);
    T(result.new_revs.length == 5);
    for (var i = 0; i < 5; i++) {
      T(result.new_revs[i].id == docs[i]._id);
      T(result.new_revs[i].rev);
      docs[i].string = docs[i].string + ".00";
    }

    // Update the docs
    result = db.bulkSave(docs);
    T(result.ok);
    T(result.new_revs.length == 5);
    for (i = 0; i < 5; i++) {
      T(result.new_revs[i].id == i.toString());
      docs[i]._deleted = true;
    }

    // Delete the docs
    result = db.bulkSave(docs);
    T(result.ok);
    T(result.new_revs.length == 5);
    for (i = 0; i < 5; i++) {
      T(db.open(docs[i]._id) == null);
    }
  },

  // test saving a semi-large quanitity of documents and do some view queries.
  lots_of_docs: function(debug) {
    var db = new CouchDB("test_suite_db");
    db.deleteDb();
    db.createDb();
    if (debug) debugger;

    // keep number lowish for now to keep tests fasts. Crank up manually to
    // to really test.
    var numDocsToCreate = 500;

    for(var i=0; i < numDocsToCreate; i += 100) {
        var createNow = Math.min(numDocsToCreate - i, 100);
        var docs = makeDocs(i, i + createNow);
        T(db.bulkSave(docs).ok);
    }

    // query all documents, and return the doc.integer member as a key.
    results = db.query(function(doc){ emit(doc.integer, null) });

    T(results.total_rows == numDocsToCreate);

    // validate the keys are ordered ascending
    for(var i=0; i<numDocsToCreate; i++) {
      T(results.rows[i].key==i);
    }

    // do the query again, but with descending output
    results = db.query(function(doc){ emit(doc.integer, null) }, null, {
      descending: true
    });

    T(results.total_rows == numDocsToCreate);

    // validate the keys are ordered descending
    for(var i=0; i<numDocsToCreate; i++) {
      T(results.rows[numDocsToCreate-1-i].key==i);
    }
  },

  reduce: function(debug) {
    var db = new CouchDB("test_suite_db");
    db.deleteDb();
    db.createDb();
    if (debug) debugger;
    var numDocs = 500
    var docs = makeDocs(1,numDocs + 1);
    T(db.bulkSave(docs).ok);
    var summate = function(N) {return (N+1)*N/2;};

    var map = function (doc) {emit(doc.integer, doc.integer)};
    var reduce = function (keys, values) { return sum(values); };
    var result = db.query(map, reduce);
    T(result.rows[0].value == summate(numDocs));

    result = db.query(map, reduce, {startkey: 4, endkey: 4});
    T(result.rows[0].value == 4);

    result = db.query(map, reduce, {startkey: 4, endkey: 5});
    T(result.rows[0].value == 9);

    result = db.query(map, reduce, {startkey: 4, endkey: 6});
    T(result.rows[0].value == 15);

    result = db.query(map, reduce, {group:true, count:3});
    T(result.rows.length == 3);
    T(result.rows[0].value == 1);
    T(result.rows[1].value == 2);
    T(result.rows[2].value == 3);

    for(var i=1; i<numDocs/2; i+=30) {
      result = db.query(map, reduce, {startkey: i, endkey: numDocs - i});
      T(result.rows[0].value == summate(numDocs-i) - summate(i-1));
    }
    
    db.deleteDb();
    db.createDb();

    for(var i=1; i <= 5; i++) {
      
      for(var j=0; j < 10; j++) {
        // these docs are in the order of the keys collation, for clarity
        var docs = [];
        docs.push({keys:["a"]});
        docs.push({keys:["a"]});
        docs.push({keys:["a", "b"]});
        docs.push({keys:["a", "b"]});
        docs.push({keys:["a", "b", "c"]});
        docs.push({keys:["a", "b", "d"]});
        docs.push({keys:["a", "c", "d"]});
        docs.push({keys:["d"]});
        docs.push({keys:["d", "a"]});
        docs.push({keys:["d", "b"]});
        docs.push({keys:["d", "c"]});
        T(db.bulkSave(docs).ok);
        T(db.info().doc_count == ((i - 1) * 10 * 11) + ((j + 1) * 11));
      }
      
      map = function (doc) {emit(doc.keys, 1)};
      reduce = function (keys, values) { return sum(values); };
    
      var results = db.query(map, reduce, {group:true});
      
      //group by exact key match
      T(equals(results.rows[0], {key:["a"],value:20*i}));
      T(equals(results.rows[1], {key:["a","b"],value:20*i}));
      T(equals(results.rows[2], {key:["a", "b", "c"],value:10*i}));
      T(equals(results.rows[3], {key:["a", "b", "d"],value:10*i}));

      // test to make sure group reduce and count params provide valid json
      var results = db.query(map, reduce, {group: true, count: 2});
      T(equals(results.rows[0], {key: ["a"], value: 20*i}));
      T(equals(results.rows.length, 2));

      //group by the first element in the key array
      var results = db.query(map, reduce, {group_level:1});
      T(equals(results.rows[0], {key:["a"],value:70*i}));
      T(equals(results.rows[1], {key:["d"],value:40*i}));
      
      //group by the first 2 elements in the key array
      var results = db.query(map, reduce, {group_level:2});
      T(equals(results.rows[0], {key:["a"],value:20*i}));
      T(equals(results.rows[1], {key:["a","b"],value:40*i}));
      T(equals(results.rows[2], {key:["a","c"],value:10*i}));
      T(equals(results.rows[3], {key:["d"],value:10*i}));
      T(equals(results.rows[4], {key:["d","a"],value:10*i}));
      T(equals(results.rows[5], {key:["d","b"],value:10*i}));
      T(equals(results.rows[6], {key:["d","c"],value:10*i}));
    }
    
    // now test out more complex reductions that need to use the combine option.
    
    db.deleteDb();
    db.createDb();

      
    var map = function (doc) {emit(null, doc.val)};
    var reduceCombine = function (keys, values, rereduce) {
        // This computes the standard deviation of the mapped results
        var stdDeviation=0;
        var count=0;
        var total=0;
        var sqrTotal=0;
          
        if (!rereduce) {
          // This is the reduce phase, we are reducing over emitted values from
          // the map functions.
          for(var i in values) {
            total = total + values[i]
            sqrTotal = sqrTotal + (values[i] * values[i])
          }
          count = values.length;
        }
        else { 
          // This is the rereduce phase, we are re-reducing previosuly
          // reduced values.
          for(var i in values) {
            count = count + values[i].count;
            total = total + values[i].total;
            sqrTotal = sqrTotal + (values[i].sqrTotal * values[i].sqrTotal);
          }
        }
        
        var variance =  (sqrTotal - ((total * total)/count)) / count;
        stdDeviation = Math.sqrt(variance);
          
        // the reduce result. It contains enough information to be rereduced
        // with other reduce results.
        return {"stdDeviation":stdDeviation,"count":count,
            "total":total,"sqrTotal":sqrTotal};
      };
      
      // Save a bunch a docs.
      for(var j=0; j < 10; j++) {
        var docs = [];
        docs.push({val:10});
        docs.push({val:20});
        docs.push({val:30});
        docs.push({val:40});
        docs.push({val:50});
        docs.push({val:60});
        docs.push({val:70});
        docs.push({val:80});
        docs.push({val:90});
        docs.push({val:100});
        T(db.bulkSave(docs).ok);
      }
      
      var results = db.query(map, reduceCombine);
      
      var difference = results.rows[0].value.stdDeviation - 28.722813232690143;
      // account for floating point rounding error
      T(Math.abs(difference) < 0.0000000001);
      
  },

  multiple_rows: function(debug) {
    var db = new CouchDB("test_suite_db");
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
  },

  large_docs: function(debug) {
    var db = new CouchDB("test_suite_db");
    db.deleteDb();
    db.createDb();
    if (debug) debugger;

    var longtext = "0123456789\n";

    for (var i=0; i<10; i++) {
      longtext = longtext + longtext
    }
    T(db.save({"longtest":longtext}).ok);
    T(db.save({"longtest":longtext}).ok);
    T(db.save({"longtest":longtext}).ok);
    T(db.save({"longtest":longtext}).ok);

    // query all documents, and return the doc.foo member as a key.
    results = db.query(function(doc){
        emit(null, doc.longtest);
    });
  },

  utf8: function(debug) {
    var db = new CouchDB("test_suite_db");
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
  },

  attachments: function(debug) {
    var db = new CouchDB("test_suite_db");
    db.deleteDb();
    db.createDb();
    if (debug) debugger;

    var binAttDoc = {
      _id: "bin_doc",
      _attachments:{
        "foo.txt": {
          content_type:"text/plain",
          data: "VGhpcyBpcyBhIGJhc2U2NCBlbmNvZGVkIHRleHQ="
        }
      }
    }

    T(db.save(binAttDoc).ok);

    var xhr = CouchDB.request("GET", "/test_suite_db/bin_doc/foo.txt");
    T(xhr.responseText == "This is a base64 encoded text");
    T(xhr.getResponseHeader("Content-Type") == "text/plain");
  },

  content_negotiation: function(debug) {
    var db = new CouchDB("test_suite_db");
    db.deleteDb();
    db.createDb();
    if (debug) debugger;
    var xhr;

    xhr = CouchDB.request("GET", "/test_suite_db/");
    T(xhr.getResponseHeader("Content-Type") == "text/plain;charset=utf-8");

    xhr = CouchDB.request("GET", "/test_suite_db/", {
      headers: {"Accept": "text/html;text/plain;*/*"}
    });
    T(xhr.getResponseHeader("Content-Type") == "text/plain;charset=utf-8");

    xhr = CouchDB.request("GET", "/test_suite_db/", {
      headers: {"Accept": "application/json"}
    });
    T(xhr.getResponseHeader("Content-Type") == "application/json");
  },

  design_docs: function(debug) {
    var db = new CouchDB("test_suite_db");
    db.deleteDb();
    db.createDb();
    if (debug) debugger;

    var numDocs = 500;
    
    function makebigstring(power) {
      var str = "a";
      while(power-- > 0) {
        str = str + str;
      }
      return str;
    }

    var designDoc = {
      _id:"_design/test",
      language: "javascript",
      views: {
        all_docs: {map: "function(doc) { emit(doc.integer, null) }"},
        no_docs: {map: "function(doc) {}"},
        single_doc: {map: "function(doc) { if (doc._id == \"1\") { emit(1, null) }}"},
        summate: {map:"function (doc) {emit(doc.integer, doc.integer)};",
                  reduce:"function (keys, values) { return sum(values); };"},
        summate2: {map:"function (doc) {emit(doc.integer, doc.integer)};",
                  reduce:"function (keys, values) { return sum(values); };"},
        huge_src_and_results: {map: "function(doc) { if (doc._id == \"1\") { emit(\"" + makebigstring(16) + "\", null) }}",
                  reduce:"function (keys, values) { return \"" + makebigstring(16) + "\"; };"}
      }
    }
    T(db.save(designDoc).ok);

    T(db.bulkSave(makeDocs(1, numDocs + 1)).ok);

    for (var loop = 0; loop < 2; loop++) {
      var rows = db.view("test/all_docs").rows;
      for (var i = 1; i <= numDocs; i++) {
        T(rows[i-1].key == i);
      }
      T(db.view("test/no_docs").total_rows == 0)
      T(db.view("test/single_doc").total_rows == 1)
      restartServer();
    }


    var summate = function(N) {return (N+1)*N/2;};
    var result = db.view("test/summate");
    T(result.rows[0].value == summate(numDocs));

    result = db.view("test/summate", {startkey:4,endkey:4});
    T(result.rows[0].value == 4);

    result = db.view("test/summate", {startkey:4,endkey:5});
    T(result.rows[0].value == 9);

    result = db.view("test/summate", {startkey:4,endkey:6});
    T(result.rows[0].value == 15);

    // Verify that a shared index (view def is an exact copy of "summate")
    // does not confuse the reduce stage
    result = db.view("test/summate2", {startkey:4,endkey:6});
    T(result.rows[0].value == 15);

    for(var i=1; i<numDocs/2; i+=30) {
      result = db.view("test/summate", {startkey:i,endkey:numDocs-i});
      T(result.rows[0].value == summate(numDocs-i) - summate(i-1));
    }

    T(db.deleteDoc(designDoc).ok);
    T(db.open(designDoc._id) == null);
    T(db.view("test/no_docs") == null);

    restartServer();
    T(db.open(designDoc._id) == null);
    T(db.view("test/no_docs") == null);
  },

  view_collation: function(debug) {
    var db = new CouchDB("test_suite_db");
    db.deleteDb();
    db.createDb();
    if (debug) debugger;

    // NOTE, the values are already in their correct sort order. Consider this
    // a specification of collation of json types.

    var values = []

    // special values sort before all other types
    values.push(null)
    values.push(false)
    values.push(true)

    // then numbers
    values.push(1)
    values.push(2)
    values.push(3.0)
    values.push(4)

    // then text, case sensitive
    values.push("a")
    values.push("A")
    values.push("aa")
    values.push("b")
    values.push("B")
    values.push("ba")
    values.push("bb")

    // then arrays. compared element by element until different.
    // Longer arrays sort after their prefixes
    values.push(["a"])
    values.push(["b"])
    values.push(["b","c"])
    values.push(["b","c", "a"])
    values.push(["b","d"])
    values.push(["b","d", "e"])

    // then object, compares each key value in the list until different.
    // larger objects sort after their subset objects.
    values.push({a:1})
    values.push({a:2})
    values.push({b:1})
    values.push({b:2})
    values.push({b:2, a:1}) // Member order does matter for collation.
                            // CouchDB preserves member order
                            // but doesn't require that clients will.
                            // (this test might fail if used with a js engine
                            // that doesn't preserve order)
    values.push({b:2, c:2})

    for (var i=0; i<values.length; i++) {
      db.save({_id:(i).toString(), foo:values[i]});
    }

    var queryFun = function(doc) { emit(doc.foo, null); }
    var rows = db.query(queryFun).rows;
    for (i=0; i<values.length; i++) {
      T(equals(rows[i].key, values[i]))
    }

    // everything has collated correctly. Now to check the descending output
    rows = db.query(queryFun, null, {descending: true}).rows
    for (i=0; i<values.length; i++) {
      T(equals(rows[i].key, values[values.length - 1 -i]))
    }

    // now check the key query args
    for (i=1; i<values.length; i++) {
      var queryOptions = {key:values[i]}
      rows = db.query(queryFun, null, queryOptions).rows;
      T(rows.length == 1 && equals(rows[0].key, values[i]))
    }
  },

  view_conflicts: function(debug) {
    var dbA = new CouchDB("test_suite_db_a");
    dbA.deleteDb();
    dbA.createDb();
    var dbB = new CouchDB("test_suite_db_b");
    dbB.deleteDb();
    dbB.createDb();
    if (debug) debugger;

    var docA = {_id: "foo", bar: 42};
    T(dbA.save(docA).ok);
    CouchDB.replicate(dbA.name, dbB.name);

    var docB = dbB.open("foo");
    docB.bar = 43;
    dbB.save(docB);
    docA.bar = 41;
    dbA.save(docA);
    CouchDB.replicate(dbA.name, dbB.name);

    var doc = dbB.open("foo", {conflicts: true});
    T(doc._conflicts.length == 1);
    var conflictRev = doc._conflicts[0];
    if (doc.bar == 41) { // A won
      T(conflictRev == docB._rev);
    } else { // B won
      T(doc.bar == 43);
      T(conflictRev == docA._rev);
    }

    var results = dbB.query(function(doc) {
      if (doc._conflicts) {
        emit(doc._id, doc._conflicts);
      }
    });
    T(results.rows[0].value[0] == conflictRev);
  },

  view_errors: function(debug) {
    var db = new CouchDB("test_suite_db");
    db.deleteDb();
    db.createDb();
    if (debug) debugger;

    var doc = {integer: 1, string: "1", array: [1, 2, 3]};
    T(db.save(doc).ok);

    // emitting a key value that is undefined should result in that row not
    // being included in the view results
    var results = db.query(function(doc) {
      emit(doc.undef, null);
    });
    T(results.total_rows == 0);

    // if a view function throws an exception, its results are not included in
    // the view index, but the view does not itself raise an error
    var results = db.query(function(doc) {
      doc.undef(); // throws an error
    });
    T(results.total_rows == 0);

    // if a view function includes an undefined value in the emitted key or
    // value, an error is logged and the result is not included in the view
    // index, and the view itself does not raise an error
    var results = db.query(function(doc) {
      emit([doc._id, doc.undef], null);
    });
    T(results.total_rows == 0);
  },

  view_pagination: function(debug) {
    var db = new CouchDB("test_suite_db");
    db.deleteDb();
    db.createDb();
    if (debug) debugger;

    var docs = makeDocs(0, 100);
    T(db.bulkSave(docs).ok);

    var queryFun = function(doc) { emit(doc.integer, null) };
    var i;

    // page through the view ascending and going forward
    for (i = 0; i < docs.length; i += 10) {
      var queryResults = db.query(queryFun, null, {
        startkey: i,
        startkey_docid: i,
        count: 10
      });
      T(queryResults.rows.length == 10)
      T(queryResults.total_rows == docs.length)
      T(queryResults.offset == i)
      var j;
      for (j = 0; j < 10;j++) {
        T(queryResults.rows[j].key == i + j);
      }
    }

    // page through the view ascending and going backward
    for (i = docs.length - 1; i >= 0; i -= 10) {
      var queryResults = db.query(queryFun, null, {
        startkey: i,
        startkey_docid: i,
        count:-10
      });
      T(queryResults.rows.length == 10)
      T(queryResults.total_rows == docs.length)
      T(queryResults.offset == i - 9)
      var j;
      for (j = 0; j < 10;j++) {
        T(queryResults.rows[j].key == i - 9 + j);
      }
    }

    // page through the view descending and going forward
    for (i = docs.length - 1; i >= 0; i -= 10) {
      var queryResults = db.query(queryFun, null, {
        startkey: i,
        startkey_docid: i,
        descending: true,
        count: 10
      });
      T(queryResults.rows.length == 10)
      T(queryResults.total_rows == docs.length)
      T(queryResults.offset == docs.length - i - 1)
      var j;
      for (j = 0; j < 10; j++) {
        T(queryResults.rows[j].key == i - j);
      }
    }

    // page through the view descending and going backward
    for (i = 0; i < docs.length; i += 10) {
      var queryResults = db.query(queryFun, null, {
        startkey: i,
        startkey_docid: i,
        descending: true,
        count:-10
      });
      T(queryResults.rows.length == 10)
      T(queryResults.total_rows == docs.length)
      T(queryResults.offset == docs.length - i - 10)
      var j;
      for (j = 0; j < 10; j++) {
        T(queryResults.rows[j].key == i + 9 - j);
      }
    }

    // ignore decending=false. CouchDB should just ignore that.
    for (i = 0; i < docs.length; i += 10) {
      var queryResults = db.query(queryFun, null, {
        startkey: i,
        startkey_docid: i,
        descending: false,
        count: 10
      });
      T(queryResults.rows.length == 10)
      T(queryResults.total_rows == docs.length)
      T(queryResults.offset == i)
      var j;
      for (j = 0; j < 10;j++) {
        T(queryResults.rows[j].key == i + j);
      }
    }
  },

  view_sandboxing: function(debug) {
    var db = new CouchDB("test_suite_db");
    db.deleteDb();
    db.createDb();
    if (debug) debugger;

    var doc = {integer: 1, string: "1", array: [1, 2, 3]};
    T(db.save(doc).ok);
/*
    // make sure that attempting to change the document throws an error
    var results = db.query(function(doc) {
      doc.integer = 2;
      emit(null, doc);
    });
    T(results.total_rows == 0);

    var results = db.query(function(doc) {
      doc.array[0] = 0;
      emit(null, doc);
    });
    T(results.total_rows == 0);
*/
    // make sure that a view cannot invoke interpreter internals such as the
    // garbage collector
    var results = db.query(function(doc) {
      gc();
      emit(null, doc);
    });
    T(results.total_rows == 0);

    // make sure that a view cannot access the map_funs array defined used by
    // the view server
    var results = db.query(function(doc) { map_funs.push(1); emit(null, doc) });
    T(results.total_rows == 0);

    // make sure that a view cannot access the map_results array defined used by
    // the view server
    var results = db.query(function(doc) { map_results.push(1); emit(null, doc) });
    T(results.total_rows == 0);
  },

  view_xml: function(debug) {
    var db = new CouchDB("test_suite_db");
    db.deleteDb();
    db.createDb();
    if (debug) debugger;

    db.save({content: "<doc><title id='xml'>Testing XML</title></doc>"});
    db.save({content: "<doc><title id='e4x'>Testing E4X</title></doc>"});

    var results = db.query(
      "function(doc) {\n" +
      "  var xml = new XML(doc.content);\n" +
      "  emit(xml.title.text(), null);\n" +
      "}");
    T(results.total_rows == 2);
    T(results.rows[0].key == "Testing E4X");
    T(results.rows[1].key == "Testing XML");

    var results = db.query(
      "function(doc) {\n" +
      "  var xml = new XML(doc.content);\n" +
      "  emit(xml.title.@id, null);\n" +
      "}");
    T(results.total_rows == 2);
    T(results.rows[0].key == "e4x");
    T(results.rows[1].key == "xml");
  },

  replication: function(debug) {
    if (debug) debugger;
    var host = window.location.host;
    var dbPairs = [
      {source:"test_suite_db_a",
        target:"test_suite_db_b"},
      {source:"test_suite_db_a",
        target:"http://" + host + "/test_suite_db_b"},
      {source:"http://" + host + "/test_suite_db_a",
        target:"test_suite_db_b"},
      {source:"http://" + host + "/test_suite_db_a",
        target:"http://" + host + "/test_suite_db_b"}
    ]
    var dbA = new CouchDB("test_suite_db_a");
    var dbB = new CouchDB("test_suite_db_b");
    var numDocs = 10;
    var xhr;
    for (var testPair = 0; testPair < dbPairs.length; testPair++) {
      var A = dbPairs[testPair].source
      var B = dbPairs[testPair].target

      dbA.deleteDb();
      dbA.createDb();
      dbB.deleteDb();
      dbB.createDb();

      var docs = makeDocs(0, numDocs);
      T(dbA.bulkSave(docs).ok);

      T(CouchDB.replicate(A, B).ok);

      for (var j = 0; j < numDocs; j++) {
        docA = dbA.open("" + j);
        docB = dbB.open("" + j);
        T(docA._rev == docB._rev);
      }

      // now check binary attachments
      var binDoc = {
        _id:"bin_doc",
        _attachments:{
          "foo.txt": {
            "type":"base64",
            "data": "VGhpcyBpcyBhIGJhc2U2NCBlbmNvZGVkIHRleHQ="
          }
        }
      }

      dbA.save(binDoc);

      T(CouchDB.replicate(A, B).ok);
      T(CouchDB.replicate(B, A).ok);

      xhr = CouchDB.request("GET", "/test_suite_db_a/bin_doc/foo.txt");
      T(xhr.responseText == "This is a base64 encoded text")

      xhr = CouchDB.request("GET", "/test_suite_db_b/bin_doc/foo.txt");
      T(xhr.responseText == "This is a base64 encoded text")

      dbA.save({_id:"foo1",value:"a"});

      T(CouchDB.replicate(A, B).ok);
      T(CouchDB.replicate(B, A).ok);

      docA = dbA.open("foo1");
      docB = dbB.open("foo1");
      T(docA._rev == docB._rev);

      dbA.deleteDoc(docA);

      T(CouchDB.replicate(A, B).ok);
      T(CouchDB.replicate(B, A).ok);

      T(dbA.open("foo1") == null);
      T(dbB.open("foo1") == null);

      dbA.save({_id:"foo",value:"a"});
      dbB.save({_id:"foo",value:"b"});

      T(CouchDB.replicate(A, B).ok);
      T(CouchDB.replicate(B, A).ok);

      // open documents and include the conflict meta data
      docA = dbA.open("foo", {conflicts: true});
      docB = dbB.open("foo", {conflicts: true});

      // make sure the same rev is in each db
      T(docA._rev === docB._rev);

      // make sure the conflicts are the same in each db
      T(docA._conflicts[0] === docB._conflicts[0]);

      // delete a conflict.
      dbA.deleteDoc({_id:"foo", _rev:docA._conflicts[0]});

      // replicate the change
      T(CouchDB.replicate(A, B).ok);

      // open documents and include the conflict meta data
      docA = dbA.open("foo", {conflicts: true});
      docB = dbB.open("foo", {conflicts: true});

      // We should have no conflicts this time
      T(docA._conflicts === undefined)
      T(docB._conflicts === undefined);
    }
  },

  etags_head: function(debug) {
    var db = new CouchDB("test_suite_db");
    db.deleteDb();
    db.createDb();
    if (debug) debugger;

    var xhr;

    // create a new doc
    xhr = CouchDB.request("PUT", "/test_suite_db/1", {
      body: "{}"
    });
    T(xhr.status == 201);

    // extract the ETag header values
    var etag = xhr.getResponseHeader("etag")

    // get the doc and verify the headers match
    xhr = CouchDB.request("GET", "/test_suite_db/1");
    T(etag == xhr.getResponseHeader("etag"));

    // 'head' the doc and verify the headers match
    xhr = CouchDB.request("HEAD", "/test_suite_db/1", {
      headers: {"if-none-match": "s"}
    });
    T(etag == xhr.getResponseHeader("etag"));

    // replace a doc
    xhr = CouchDB.request("PUT", "/test_suite_db/1", {
      body: "{}",
      headers: {"if-match": etag}
    });
    T(xhr.status == 201);

    // extract the new ETag value
    var etagOld= etag;
    etag = xhr.getResponseHeader("etag")

    // fail to replace a doc
    xhr = CouchDB.request("PUT", "/test_suite_db/1", {
      body: "{}"
    });
    T(xhr.status == 412)

    // verify get w/Etag
    xhr = CouchDB.request("GET", "/test_suite_db/1", {
      headers: {"if-none-match": etagOld}
    });
    T(xhr.status == 200);
    xhr = CouchDB.request("GET", "/test_suite_db/1", {
      headers: {"if-none-match": etag}
    });
    T(xhr.status == 304);

    // fail to delete a doc
    xhr = CouchDB.request("DELETE", "/test_suite_db/1", {
      headers: {"if-match": etagOld}
    });
    T(xhr.status == 412);

    //now do it for real
    xhr = CouchDB.request("DELETE", "/test_suite_db/1", {
      headers: {"if-match": etag}
    });
    T(xhr.status == 200)
  },

  compact: function(debug) {
    var db = new CouchDB("test_suite_db");
    db.deleteDb();
    db.createDb();
    if (debug) debugger;
    var docs = makeDocs(0, 10);
    var saveResult = db.bulkSave(docs);
    T(saveResult.ok);

    var binAttDoc = {
      _id: "bin_doc",
      _attachments:{
        "foo.txt": {
          content_type:"text/plain",
          data: "VGhpcyBpcyBhIGJhc2U2NCBlbmNvZGVkIHRleHQ="
        }
      }
    }

    T(db.save(binAttDoc).ok);

    var originalsize = db.info().disk_size;

    for(var i in docs) {
        db.deleteDoc(docs[i]);
    }
    var deletesize = db.info().disk_size;
    T(deletesize > originalsize);

    var xhr = CouchDB.request("POST", "/test_suite_db/_compact");
    T(xhr.status == 202);
    // compaction isn't instantaneous, loop until done
    while (db.info().compact_running) {};
    
    restartServer();
    var xhr = CouchDB.request("GET", "/test_suite_db/bin_doc/foo.txt");
    T(xhr.responseText == "This is a base64 encoded text")
    T(xhr.getResponseHeader("Content-Type") == "text/plain")
    T(db.info().doc_count == 1);
    T(db.info().disk_size < deletesize);
  }
};

function makeDocs(start, end, templateDoc) {
  var templateDocSrc = templateDoc ? templateDoc.toSource() : "{}"
  if (end === undefined) {
    end = start;
    start = 0;
  }
  var docs = []
  for (var i = start; i < end; i++) {
    var newDoc = eval("(" + templateDocSrc + ")");
    newDoc._id = (i).toString();
    newDoc.integer = i
    newDoc.string = (i).toString();
    docs.push(newDoc)
  }
  return docs;
}

// *********************** Test Framework of Sorts ************************* //

function patchTest(fun) {
  var source = fun.toString();
  var output = "";
  var i = 0;
  var testMarker = "T("
  while (i < source.length) {
    var testStart = source.indexOf(testMarker, i);
    if (testStart == -1) {
      output = output + source.substring(i, source.length);
      break;
    }
    var testEnd = source.indexOf(");", testStart);
    var testCode = source.substring(testStart + testMarker.length, testEnd);
    output += source.substring(i, testStart) + "T(" + testCode + "," + JSON.stringify(testCode);
    i = testEnd;
  }
  try {
    return eval("(" + output + ")");
  } catch (e) {
    return null;
  }
}

function runAllTests() {
  var rows = $("#tests tbody.content tr");
  $("td", rows).html("&nbsp;");
  $("td.status", rows).removeClass("error").removeClass("failure").removeClass("success").text("not run");
  var offset = 0;
  function runNext() {
    if (offset < rows.length) {
      var row = rows.get(offset);
      runTest($("th button", row).get(0), function() {
        offset += 1;
        setTimeout(runNext, 1000);
      });
    }
  }
  runNext();
}

var numFailures = 0;
var currentRow = null;

function runTest(button, callback, debug) {
  if (currentRow != null) {
    alert("Can not run multiple tests simultaneously.");
    return;
  }
  var row = currentRow = $(button).parents("tr").get(0);
  $("td.status", row).removeClass("error").removeClass("failure").removeClass("success");
  $("td", row).html("&nbsp;");
  var testFun = tests[row.id];
  function run() {
    numFailures = 0;
    var start = new Date().getTime();
    try {
      if (debug == undefined || !debug) {
        testFun = patchTest(testFun) || testFun;
      }
      testFun(debug);
      var status = numFailures > 0 ? "failure" : "success";
    } catch (e) {
      var status = "error";
      if ($("td.details ol", row).length == 0) {
        $("<ol></ol>").appendTo($("td.details", row));
      }
      $("<li><b>Exception raised:</b> <code class='error'></code></li>")
        .find("code").text(JSON.stringify(e)).end()
        .appendTo($("td.details ol", row));
      if (debug) {
        currentRow = null;
        throw e;
      }
    }
    if ($("td.details ol", row).length) {
      $("<a href='#'>Run with debugger</a>").click(function() {
        runTest(this, undefined, true);
      }).prependTo($("td.details ol", row));
    }
    var duration = new Date().getTime() - start;
    $("td.status", row).removeClass("running").addClass(status).text(status);
    $("td.duration", row).text(duration + "ms");
    updateTestsFooter();
    currentRow = null;
    if (callback) callback();
  }
  $("td.status", row).addClass("running").text("running…");
  setTimeout(run, 100);
}

function showSource(cell) {
  var name = $(cell).text();
  var win = window.open("", name, "width=700,height=500,resizable=yes,scrollbars=yes");
  win.document.title = name;
  $("<pre></pre>").text(tests[name].toString()).appendTo(win.document.body).fadeIn();
}

function updateTestsListing() {
  for (var name in tests) {
    if (!tests.hasOwnProperty(name)) continue;
    var testFunction = tests[name];
    var row = $("<tr><th></th><td></td><td></td><td></td></tr>")
      .find("th").text(name).attr("title", "Show source").click(function() {
        showSource(this);
      }).end()
      .find("td:nth(0)").addClass("status").text("not run").end()
      .find("td:nth(1)").addClass("duration").html("&nbsp;").end()
      .find("td:nth(2)").addClass("details").html("&nbsp;").end();
    $("<button type='button' class='run' title='Run test'></button>").click(function() {
      this.blur();
      runTest(this);
      return false;
    }).prependTo(row.find("th"));
    row.attr("id", name).appendTo("#tests tbody.content");
  }
  $("#tests tr").removeClass("odd").filter(":odd").addClass("odd");
  updateTestsFooter();
}

function updateTestsFooter() {
  var tests = $("#tests tbody.content tr td.status");
  var testsRun = tests.not(":contains('not run'))");
  var testsFailed = testsRun.not(".success");
  $("#tests tbody.footer td").text(testsRun.length + " of " + tests.length +
    " test(s) run, " + testsFailed.length + " failures");
}

// Use T to perform a test that returns false on failure and if the test fails,
// display the line that failed.
// Example:
// T(MyValue==1);
function T(arg1, arg2) {
  if (!arg1) {
    if (currentRow) {
      if ($("td.details ol", currentRow).length == 0) {
        $("<ol></ol>").appendTo($("td.details", currentRow));
      }
      $("<li><b>Assertion failed:</b> <code class='failure'></code></li>")
        .find("code").text((arg2 != null ? arg2 : arg1).toString()).end()
        .appendTo($("td.details ol", currentRow));
    }
    numFailures += 1
  }
}

function equals(a,b) {
  if (a === b) return true;
  try {
    return repr(a) === repr(b);
  } catch (e) {
    return false;
  }
}

function repr(val) {
  if (val === undefined) {
    return null;
  } else if (val === null) {
    return "null";
  } else {
    return JSON.stringify(val);
  }
}

function restartServer() {
  var xhr = CouchDB.request("POST", "/_restart");
  do {
    xhr = CouchDB.request("GET", "/");
  } while(xhr.status != 200);
}
