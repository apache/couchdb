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

var cmd;
var funs = [];        // holds functions used for computation
var map_results = []; // holds temporary emitted values during doc map

var sandbox = null;

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

log = function(message) {
  print(toJSON({log: toJSON(message)}));  
}

try {
  // if possible, use evalcx (not always available)
  sandbox = evalcx('');
  sandbox.emit = emit;
  sandbox.sum = sum;
  sandbox.log = log;
} catch (e) {}

// Commands are in the form of json arrays:
// ["commandname",..optional args...]\n
//
// Responses are json values followed by a new line ("\n")

while (cmd = eval(readline())) {
  try {
    switch (cmd[0]) {
      case "reset":
        // clear the globals and run gc
        funs = [];
        gc();
        print("true"); // indicates success
        break;
      case "add_fun":
        // The second arg is a string that will compile to a function.
        // and then we add it to funs array
        funs.push(compileFunction(cmd[1]));
        print("true");
        break;
      case "map_doc":
        // The second arg is a document. We compute all the map functions against
        // it.
        //
        // Each function can output multiple keys value, pairs for each document
        //
        // Example output of map_doc after three functions set by add_fun cmds:
        // [
        //  [["Key","Value"]],                    <- fun 1 returned 1 key value
        //  [],                                   <- fun 2 returned 0 key values
        //  [["Key1","Value1"],["Key2","Value2"]] <- fun 3 returned 2 key values
        // ]
        //
        var doc = cmd[1];
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
              throw {error: "map_runtime_error",
                  reason: "function raised fatal exception"};
            }
            print(toJSON({log: "function raised exception (" + err + ")"}));
            buf.push("[]");
          }
        }
        print("[" + buf.join(", ") + "]");
        break;

      case "rereduce":
      case "reduce":
        {
        var keys = null;
        var values = null;
        var reduceFuns = cmd[1];
        var rereduce = false;
        
        if (cmd[0] == "reduce") {
          var kvs = cmd[2];
          keys = new Array(kvs.length);
          values = new Array(kvs.length);
          for(var i = 0; i < kvs.length; i++) {
              keys[i] = kvs[i][0];
              values[i] = kvs[i][1];
          }
        } else {
          values = cmd[2];
          rereduce = true;
        }

        for (var i in reduceFuns) {
          reduceFuns[i] = compileFunction(reduceFuns[i]);
        }

        var reductions = new Array(funs.length);
        for(var i = 0; i < reduceFuns.length; i++) {
          try {
            reductions[i] = reduceFuns[i](keys, values, rereduce);
          } catch (err) {
            if (err == "fatal_error") {
              throw {error: "reduce_runtime_error",
                  reason: "function raised fatal exception"};
            }
            print(toJSON({log: "function raised exception (" + err + ")"}));
            reductions[i] = null;
          }
        }
        print("[true," + toJSON(reductions) + "]");
        }
        break;

      default:
        print(toJSON({error: "query_server_error",
            reason: "unknown command '" + cmd[0] + "'"}));
        quit();
    }
  } catch (exception) {
    print(toJSON(exception));
  }
}


function compileFunction(source) {
  try {
    var functionObject = sandbox ? evalcx(source, sandbox) : eval(source);
  } catch (err) {
    throw {error: "compilation_error",
      reason: err.toString() + " (" + source + ")"};
  }
  if (typeof(functionObject) == "function") {
    return functionObject;
  } else {
    throw {error: "compilation_error",
      reason: "expression does not eval to a function. (" + source + ")"};
  }
}

function recursivelySeal(obj) {
  seal(obj);
  for (var propname in obj) {
    if (typeof doc[propname] == "object") {
      recursivelySeal(doc[propname]);
    }
  }
}

function toJSON(val) {
  if (typeof(val) == "undefined") {
    throw "Cannot encode 'undefined' value as JSON";
  }
  var subs = {'\b': '\\b', '\t': '\\t', '\n': '\\n', '\f': '\\f',
              '\r': '\\r', '"' : '\\"', '\\': '\\\\'};
  if (typeof(val) == "xml") { // E4X support
    val = val.toXMLString();
  }
  return {
    "Array": function(v) {
      var buf = [];
      for (var i = 0; i < v.length; i++) {
        buf.push(toJSON(v[i]));
      }
      return "[" + buf.join(",") + "]";
    },
    "Boolean": function(v) {
      return v.toString();
    },
    "Date": function(v) {
      var f = function(n) { return n < 10 ? '0' + n : n }
      return '"' + v.getUTCFullYear()   + '-' +
                 f(v.getUTCMonth() + 1) + '-' +
                 f(v.getUTCDate())      + 'T' +
                 f(v.getUTCHours())     + ':' +
                 f(v.getUTCMinutes())   + ':' +
                 f(v.getUTCSeconds())   + 'Z"';
    },
    "Number": function(v) {
      return isFinite(v) ? v.toString() : "null";
    },
    "Object": function(v) {
      if (v === null) return "null";
      var buf = [];
      for (var k in v) {
        if (!v.hasOwnProperty(k) || typeof(k) !== "string" || v[k] === undefined) {
          continue;
        }
        buf.push(toJSON(k, val) + ": " + toJSON(v[k]));
      }
      return "{" + buf.join(",") + "}";
    },
    "String": function(v) {
      if (/["\\\x00-\x1f]/.test(v)) {
        v = v.replace(/([\x00-\x1f\\"])/g, function(a, b) {
          var c = subs[b];
          if (c) return c;
          c = b.charCodeAt();
          return '\\u00' + Math.floor(c / 16).toString(16) + (c % 16).toString(16);
        });
      }
      return '"' + v + '"';
    }
  }[val != null ? val.constructor.name : "Object"](val);
}
