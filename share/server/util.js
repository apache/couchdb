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
      var f = function(n) { return n < 10 ? '0' + n : n };
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

var responseSent;
// prints the object as JSON, and rescues and logs any toJSON() related errors
function respond(obj) {
  responseSent = true;
  try {
    print(toJSON(obj));  
  } catch(e) {
    log("Error converting object to JSON: " + e.toString());
  }
};

log = function(message) {
  if (typeof message == "undefined") {
    message = "Error: attempting to log message of 'undefined'.";
  } else if (typeof message != "string") {
    message = toJSON(message);
  }
  print(toJSON({log: message}));
};
