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
var map_funs = [];        // The map functions to compute against documents
var map_results = [];

try {
  var sandbox = evalcx('');
  sandbox.map = function(key, value) {
    map_results.push([key, value]);
  }
} catch (e) {
  // fallback for older versions of spidermonkey that don't have evalcx
  var sandbox = null;
  map = function(key, value) {
    map_results.push([key, value]);
  }
}

// Commands are in the form of json arrays:
// ["commandname",..optional args...]\n
//
// Responses are json values followed by a new line ("\n")

while (cmd = eval(readline())) {
  switch (cmd[0]) {
    case "reset":
      // clear the map_functions and run gc
      map_funs = [];
      gc();
      print("true"); // indicates success
      break;
    case "add_fun":
      // The second arg is a string that will compile to a function.
      // and then we add it to map_functions array
      try {
        var functionObject = sandbox ? evalcx(cmd[1], sandbox) : eval(cmd[1]);
      } catch (err) {
        print(toJSON({error: {id: "map_compilation_error",
          reason: err.toString() + " (" + toJSON(cmd[1]) + ")"}}));
        break;
      }
      if (typeof(functionObject) == "function") {
        map_funs.push(functionObject);
        print("true");
      } else {
        print(toJSON({error: "map_compilation_error",
          reason: "expression does not eval to a function. (" + cmd[1] + ")"}));
      }
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
      //  [["Key1","Value1"],["Key2","Value2"]],<- fun 3 returned 2 key values
      // ]
      //
      var doc = cmd[1];
      seal(doc); // seal to prevent map functions from changing doc
      var buf = [];
      for (var i = 0; i < map_funs.length; i++) {
        map_results = [];
        try {
          map_funs[i](doc);
          buf.push(map_results.filter(function(pair) {
            return pair[0] !== undefined && pair[1] !== undefined;
          }));
        } catch (err) {
          if (err == "fatal_error") {
            // Only if it's a "fatal_error" do we exit. What's a fatal error?
            // That's for the query to decide.
            //
            // This will make it possible for queries to completely error out,
            // by catching their own local exception and rethrowing a
            // fatal_error. But by default if they don't do error handling we
            // just eat the exception and carry on.
            print(toJSON({error: "map_runtime_error",
                reason: "function raised fatal exception"}));
            quit();
          }
          print(toJSON({log: "function raised exception (" + err + ")"}));
          buf.push([]);
        }
      }
      print(toJSON(buf));
      break;
    default:
      print(toJSON({error: "query_server_error",
          reason: "unknown command '" + cmd[0] + "'"}));
      quit();
  }
}

function toJSON(val) {
  if (typeof(val) == "undefined") {
    throw new TypeError("Cannot encode undefined value as JSON");
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
