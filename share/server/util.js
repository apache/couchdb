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

var Couch = {
  // moving this away from global so we can move to json2.js later
  toJSON : function (val) {
    if (typeof(val) == "undefined") {
      throw "Cannot encode 'undefined' value as JSON";
    }
    if (typeof(val) == "xml") { // E4X support
      val = val.toXMLString();
    }
    if (val === null) { return "null"; }
    return (Couch.toJSON.dispatcher[val.constructor.name])(val);
  },
  compileFunction : function(source) {
    if (!source) throw(["error","not_found","missing function"]);
    try {
      var functionObject = sandbox ? evalcx(source, sandbox) : eval(source);
    } catch (err) {
      throw(["error", "compilation_error", err.toSource() + " (" + source + ")"]);
    };
    if (typeof(functionObject) == "function") {
      return functionObject;
    } else {
      throw(["error","compilation_error",
        "Expression does not eval to a function. (" + source.toSource() + ")"]);
    };
  },
  recursivelySeal : function(obj) {
    // seal() is broken in current Spidermonkey
    seal(obj);
    for (var propname in obj) {
      if (typeof doc[propname] == "object") {
        recursivelySeal(doc[propname]);
      }
    }
  }
}

Couch.toJSON.subs = {'\b': '\\b', '\t': '\\t', '\n': '\\n', '\f': '\\f',
              '\r': '\\r', '"' : '\\"', '\\': '\\\\'};
Couch.toJSON.dispatcher = {
    "Array": function(v) {
      var buf = [];
      for (var i = 0; i < v.length; i++) {
        buf.push(Couch.toJSON(v[i]));
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
      //if (v === null) return "null";
      var buf = [];
      for (var k in v) {
        if (!v.hasOwnProperty(k) || typeof(k) !== "string" || v[k] === undefined) {
          continue;
        }
        buf.push(Couch.toJSON(k) + ": " + Couch.toJSON(v[k]));
      }
      return "{" + buf.join(",") + "}";
    },
    "String": function(v) {
      if (/["\\\x00-\x1f]/.test(v)) {
        v = v.replace(/([\x00-\x1f\\"])/g, function(a, b) {
          var c = Couch.toJSON.subs[b];
          if (c) return c;
          c = b.charCodeAt();
          return '\\u00' + Math.floor(c / 16).toString(16) + (c % 16).toString(16);
        });
      }
      return '"' + v + '"';
    }
};

// prints the object as JSON, and rescues and logs any toJSON() related errors
function respond(obj) {
  try {
    print(Couch.toJSON(obj));
  } catch(e) {
    log("Error converting object to JSON: " + e.toString());
    log("error on obj: "+ obj.toSource());
  }
};

function log(message) {
  // return; // idea: query_server_config option for log level
  if (typeof message == "undefined") {
    message = "Error: attempting to log message of 'undefined'.";
  } else if (typeof message != "string") {
    message = Couch.toJSON(message);
  }
  respond(["log", message]);
};
