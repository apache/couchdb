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



var Views = (function() {

  var map_results = []; // holds temporary emitted values during doc map

  function runReduce(reduceFuns, keys, values, rereduce) {
    for (var i in reduceFuns) {
      reduceFuns[i] = Couch.compileFunction(reduceFuns[i]);
    };
    var reductions = new Array(reduceFuns.length);
    for(var i = 0; i < reduceFuns.length; i++) {
      try {
        reductions[i] = reduceFuns[i](keys, values, rereduce);
      } catch (err) {
        handleViewError(err);
        // if the error is not fatal, ignore the results and continue
        reductions[i] = null;
      }
    };
    var reduce_line = Couch.toJSON(reductions);
    var reduce_length = reduce_line.length;
    // TODO make reduce_limit config into a number
    if (State.query_config && State.query_config.reduce_limit &&
          reduce_length > 200 && ((reduce_length * 2) > State.line_length)) {
      var reduce_preview = "Current output: '"+(reduce_line.substring(0,100) + "'... (first 100 of "+reduce_length+" bytes)");
      throw(["error", 
        "reduce_overflow_error", 
        "Reduce output must shrink more rapidly: "+reduce_preview]);
    } else {
      print("[true," + reduce_line + "]");
    }
  };

  function handleViewError(err, doc) {
    if (err == "fatal_error") {
      // Only if it's a "fatal_error" do we exit. What's a fatal error?
      // That's for the query to decide.
      //
      // This will make it possible for queries to completely error out,
      // by catching their own local exception and rethrowing a
      // fatal_error. But by default if they don't do error handling we
      // just eat the exception and carry on.
      //
      // In this case we abort map processing but don't destroy the 
      // JavaScript process. If you need to destroy the JavaScript 
      // process, throw the error form matched by the block below.
      throw(["error", "map_runtime_error", "function raised 'fatal_error'"]);
    } else if (err[0] == "fatal") {
      // Throwing errors of the form ["fatal","error_key","reason"]
      // will kill the OS process. This is not normally what you want.
      throw(err);
    }
    var message = "function raised exception " + err.toSource();
    if (doc) message += " with doc._id " + doc._id;
    log(message);
  };

  return {
    // view helper functions
    emit : function(key, value) {
      map_results.push([key, value]);
    },
    sum : function(values) {
      var rv = 0;
      for (var i in values) {
        rv += values[i];
      }
      return rv;
    },
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

      Couch.recursivelySeal(doc);

      var buf = [];
      for (var i = 0; i < State.funs.length; i++) {
        map_results = [];
        try {
          State.funs[i](doc);
          buf.push(Couch.toJSON(map_results));
        } catch (err) {
          handleViewError(err, doc);
          // If the error is not fatal, we treat the doc as if it
          // did not emit anything, by buffering an empty array.
          buf.push("[]");
        }
      }
      print("[" + buf.join(", ") + "]");
    }
  };
})();
