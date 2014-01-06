/*
 * NOTE:
 * This temporarily uses the PouchDB map reduce implementation
 * These files are modified locally until we make a more general version and
 * push it back upstream.
 */

define([
  "app",

  "api",

  // Modules
  "addons/pouchdb/pouchdb.mapreduce.js"
],

function(app, FauxtonAPI, MapReduce) {
  var Pouch = FauxtonAPI.addon();
  Pouch.MapReduce = MapReduce;

  Pouch.runViewQuery = function(fun, opts) {
    /*docs = [
      {_id: 'test_doc_1', foo: 'bar-1'},
      {_id: 'test_doc_2', foo: 'bar-2'},
      {_id: 'test_doc_3', foo: 'bar-3'},
      {_id: 'test_doc_4', foo: 'bar-4'},
      {_id: 'test_doc_5', foo: 'bar-5'},
      {_id: 'test_doc_6', foo: 'bar-6'},
      {_id: 'test_doc_7', foo: 'bar-7'},
      {_id: 'test_doc_8', foo: 'bar-8'},
      {_id: 'test_doc_9', foo: 'bar-9'},
      {_id: 'test_doc_10', foo: 'bar-10'}
    ];*/

    var deferred = FauxtonAPI.Deferred();
    var complete = function(resp, rows) {
      deferred.resolve(rows);
    };

    var options = _.extend(opts, {complete: complete});

    Pouch.MapReduce.query(fun, options);
    return deferred;
  };
  //pdb.runViewQuery({map:function(doc) { emit(doc._id, doc.foo) }})
  return Pouch;
});
