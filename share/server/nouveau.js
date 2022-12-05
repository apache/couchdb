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

var Nouveau = (function() {

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

  function assertType(name, expected, actual) {
    if (typeof actual !== expected) {
      throw({name: 'TypeError', message: 'type of ' + name + ' must be a ' + expected + ' not ' + typeof actual});
    }
  };

  return {
    index: function(name, value) {
      assertType('name', 'string', name);

      if (name.substring(0, 1) === '_') {
        throw({name: 'ReservedName', message: 'name must not start with an underscore'});
      }

      // Dreyfus compatibility.
      if (arguments.length == 2 || (arguments.length == 3 && typeof arguments[2] == 'object')) {
        options = arguments[2] || {};
        if (typeof value == 'boolean') {
          // coerce to string as handling is the same.
          value = value ? 'true' : 'false'
        }
        switch (typeof value) {
        case 'string':
          index_results.push({'@type': 'text', 'name': name, 'value': value, 'stored': options.store || false});
          index_results.push({'@type': 'sorted_dv', 'name': name, 'value': value}); // for sorting.
          if (options.facet) {
            index_results.push({'@type': 'sorted_set_dv', 'name': name, 'value': value});
          }
          break;
        case 'number':
          index_results.push({'@type': 'double_point', 'name': name, 'value': value});
          if (options.store) {
            index_results.push({'@type': 'stored_double', 'name': name, 'value': value});
          }
          if (options.facet) {
            index_results.push({'@type': 'double_dv', 'name': name, 'value': value});
          }
        default:
          throw({name: 'TypeError', message: 'value must be a string, a number or boolean not ' + typeof value});
        }
        return;
      }

      var type = arguments[2];
      assertType('type', 'string', type);

      switch (type) {
      case 'binary_dv':
      case 'stored_binary':
      case 'sorted_set_dv':
      case 'sorted_dv':
        assertType('value', 'string', value);
        index_results.push({'@type': type, 'name': name, 'value': value});
        break;
      case 'double_point':
      case 'float_dv':
      case 'float_point':
      case 'int_point':
      case 'long_point':
      case 'sorted_numeric_dv':
      case 'double_dv':
        assertType('value', 'number', value);
        index_results.push({'@type': type, 'name': name, 'value': value});
        break;
      case 'latlon_dv':
      case 'latlon_point':
        assertType('value', 'number', arguments[3]);
        assertType('value', 'number', arguments[4]);
        index_results.push({'@type': type, 'name': name, 'lat': arguments[3], 'lon': arguments[4]});
        break;
      case 'xy_dv':
      case 'xy_point':
        assertType('value', 'number', arguments[3]);
        assertType('value', 'number', arguments[4]);
        index_results.push({'@type': type, 'name': name, 'x': arguments[3], 'y': arguments[4]});
        break;
      case 'string':
      case 'text':
        assertType('value', 'string', value);
        if (arguments.length === 4) {
          assertType('boolean', arguments[3]);
        }
        index_results.push({'@type': type, 'name': name, 'value': value, 'stored': arguments[3] || false});
        break;
      case 'stored_double':
        assertType('value', 'number', value);
        index_results.push({'@type': type, 'name': name, 'value': value});
        break;
      case 'stored_string':
        assertType('value', 'string', value);
        index_results.push({'@type': type, 'name': name, 'value': value});
        break;
      default:
        throw({name: 'TypeError', message: type + ' not supported'});
      }
    },

    indexDoc: function(doc) {
      Couch.recursivelySeal(doc);
      var buf = [];
      for (var fun in State.funs) {
        index_results = [];
        try {
          State.funs[fun](doc);
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
