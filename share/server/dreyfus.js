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

var Dreyfus = (function() {

  var index_results = []; // holds temporary emitted values during index

  function handleIndexError(err, doc) {
    if (err == "fatal_error") {
      throw(["error", "map_runtime_error", "function raised 'fatal_error'"]);
    } else if (err[0] == "fatal") {
      throw(err);
    }
    var message = "function raised exception " + err.toSource();
    if (doc) message += " with doc._id " + doc._id;
    log(message);
  };

  return {
    index: function(name, value, options) {
      if (typeof name !== 'string') {
        throw({name: 'TypeError', message: 'name must be a string not ' + typeof name});
      }
      if (name.substring(0, 1) === '_') {
        throw({name: 'ReservedName', message: 'name must not start with an underscore'});
      }
      if (typeof value !== 'string' && typeof value !== 'number' && typeof value !== 'boolean') {
        throw({name: 'TypeError', message: 'value must be a string, a number or boolean not ' + typeof value});
      }
      if (options && typeof options !== 'object') {
        throw({name: 'TypeError', message: 'options must be an object not ' + typeof options});
      }
      index_results.push([name, value, options || {}]);
    },

    indexDoc: function(doc) {
      Couch.recursivelySeal(doc);
      var buf = [];
      for each (fun in State.funs) {
        index_results = [];
        try {
          fun(doc);
          buf.push(index_results);
        } catch (err) {
          handleIndexError(err, doc);
          buf.push([]);
        }
      }
      print(JSON.stringify(buf));
    }

  }
})();
