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

// globals used by views
var map_results = []; // holds temporary emitted values during doc map

// view helper functions
emit = function(key, value) {
  map_results.push([key, value]);
}

sum = function(values) {
  var rv = 0;
  for (var i in values) {
    rv += values[i];
  }
  return rv;
}

var Views = (function() {

  function runReduce(reduceFuns, keys, values, rereduce) {
    for (var i in reduceFuns) {
      reduceFuns[i] = compileFunction(reduceFuns[i]);
    }
    var reductions = new Array(reduceFuns.length);
    for(var i = 0; i < reduceFuns.length; i++) {
      try {
        reductions[i] = reduceFuns[i](keys, values, rereduce);
      } catch (err) {
        if (err == "fatal_error") {
          throw {
            error: "reduce_runtime_error",
            reason: "function raised fatal exception"};
        }
        log("function raised exception (" + err + ")");
        reductions[i] = null;
      }
    }
    var reduce_line = toJSON(reductions);
    var reduce_length = reduce_line.length;
    if (query_config && query_config.reduce_limit &&
          reduce_length > 200 && ((reduce_length * 2) > line.length)) {
      var reduce_preview = "Current output: '"+(reduce_line.substring(0,100) + "'... (first 100 of "+reduce_length+' bytes)');

      throw {
        error:"reduce_overflow_error",
        reason: "Reduce output must shrink more rapidly: "+reduce_preview+""
      };
    } else {
      print("[true," + reduce_line + "]");
    }
  };

  return {
    reduce : function(reduceFuns, kvs) {
      var keys = new Array(kvs.length);
      var values = new Array(kvs.length);
      for(var i = 0; i < kvs.length; i++) {
          keys[i] = kvs[i][0];
          values[i] = kvs[i][1];
      }
      runReduce(reduceFuns, keys, values, false);
    },
    rereduce : function(reduceFuns, values) {
      runReduce(reduceFuns, null, values, true);
    },
    mapDoc : function(doc) {
      // Compute all the map functions against the document.
      //
      // Each function can output multiple key/value pairs for each document.
      //
      // Example output of map_doc after three functions set by add_fun cmds:
      // [
      //  [["Key","Value"]],                    <- fun 1 returned 1 key value
      //  [],                                   <- fun 2 returned 0 key values
      //  [["Key1","Value1"],["Key2","Value2"]] <- fun 3 returned 2 key values
      // ]
      //

      /*
      Immutable document support temporarily removed.

      Removed because the seal function no longer works on JS 1.8 arrays,
      instead returning an error. The sealing is meant to prevent map
      functions from modifying the same document that is passed to other map
      functions. However, only map functions in the same design document are
      run together, so we have a reasonable expectation they can trust each
      other. Any map fun that can't be trusted can be placed in its own
      design document, and it cannot affect other map functions.

      recursivelySeal(doc); // seal to prevent map functions from changing doc
      */
      var buf = [];
      for (var i = 0; i < funs.length; i++) {
        map_results = [];
        try {
          funs[i](doc);
          buf.push(toJSON(map_results));
        } catch (err) {
          if (err == "fatal_error") {
            // Only if it's a "fatal_error" do we exit. What's a fatal error?
            // That's for the query to decide.
            //
            // This will make it possible for queries to completely error out,
            // by catching their own local exception and rethrowing a
            // fatal_error. But by default if they don't do error handling we
            // just eat the exception and carry on.
            throw {
              error: "map_runtime_error",
              reason: "function raised fatal exception"};
          }
          log("function raised exception (" + err + ") with doc._id " + doc._id);
          buf.push("[]");
        }
      }
      print("[" + buf.join(", ") + "]");
    }
  }
})();
