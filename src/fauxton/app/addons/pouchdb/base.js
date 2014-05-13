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
  "addons/pouchdb/pouchdb.mapreduce"
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
