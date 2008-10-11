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

// couch.js, with modifications

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

// some monkeypatches
var JSON = {
  parse : function(string) {
    return eval('('+string+')');
  },
  stringify : function(obj) {
    return toJSON(obj||null);
  }
};

RegExp.escape = function(text) {
  if (!arguments.callee.sRE) {
    var specials = [
      '/', '.', '*', '+', '?', '|',
      '(', ')', '[', ']', '{', '}', '\\'
    ];
    arguments.callee.sRE = new RegExp(
      '(\\' + specials.join('|\\') + ')', 'g'
    );
  }
  return text.replace(arguments.callee.sRE, '\\$1');
}

// This is a JS wrapper for the curl function made available in couch_js.c,
// it should be used in other JavaScripts that would like to make HTTP calls.

var HTTP = (function() {
  function parseCurl(string) {
    var parts = string.split(/\r\n\r\n/);
    var body = parts.pop();
    var header = parts.pop();
    var headers = header.split(/\n/);
    
    var status = /HTTP\/1.\d (\d*)/.exec(header)[1];
    return {
      responseText: body,
      status: parseInt(status),
      getResponseHeader: function(key) {
        var keymatcher = new RegExp(RegExp.escape(key), "i");
        for (var i in headers) {
          var h = headers[i];
          if (keymatcher.test(h)) {
            var value = h.substr(key.length+2);
            value = value.slice(0, value.length-1);
            return value;
          }
        }
        return "";
      }
    }
  };
  return {
    GET : function(url, body, headers) {
      var st, urx = url, hx = (headers || null);
      st = gethttp(urx, hx);
      return parseCurl(st);
    },
    HEAD : function(url, body, headers) {
      var st, urx = url, hx = (headers || null);
      st = headhttp(urx, hx);
      return parseCurl(st);      
    },
    DELETE : function(url, body, headers) {
      var st, urx = url, hx = (headers || null);
      st = delhttp(urx, hx);
      return parseCurl(st);
    },
    MOVE : function(url, body, headers) {
      var st, urx = url, hx = (headers || null);
      st = movehttp(urx, hx);
      return parseCurl(st);
    },
    COPY : function(url, body, headers) {
      var st, urx = url, hx = (headers || null);
      st = copyhttp(urx, hx);
      return parseCurl(st);
    },
    POST : function(url, body, headers) {
      var st, urx = url, bx = (body || ""), hx = (headers || {});
      hx['Content-Type'] = hx['Content-Type'] || "application/json";
      st = posthttp(urx, bx, hx);
      return parseCurl(st);
    },
    PUT : function(url, body, headers) {
      var st, urx = url, bx = (body || ""), hx = (headers || {});
      hx['Content-Type'] = hx['Content-Type'] || "application/json";
      st = puthttp(urx, bx, hx);
      return parseCurl(st);
    }
  };
})();

// Monkeypatches to CouchDB client for use of curl.

CouchDB.host = (typeof window == 'undefined' || !window) ? "127.0.0.1:5984" : window;

CouchDB.request = function(method, uri, options) {
  var full_uri = "http://" + CouchDB.host + uri;
  options = options || {};
  var response = HTTP[method](full_uri, options.body, options.headers);
  return response;
}


function toJSON(val) {
  if (typeof(val) == "undefined") {
    throw {error:"bad_value", reason:"Cannot encode 'undefined' value as JSON"};
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



// *************** Test Framework Console Adapter ****************** //

var p = print;
var numFailures = 0;

function runAllTestsConsole() {
  var numTests = 0;
  var debug = false;
  for (var t in tests) {
    p(t);
    if (t == "utf8") {
      p("We skip the utf8 test because it fails due to problems in couch_js.c");
      p("Run the in-browser tests to verify utf8.\n");
    } else {
      numTests += 1;
      var testFun = tests[t];
      runTestConsole(testFun, debug);      
    }
  }
  p("Results: "+numFailures.toString() + " failures in "+numTests+" tests.")
};

function runTestConsole(testFun, debug) {
  var start = new Date().getTime();
  try {
    if (!debug) testFun = patchTest(testFun) || testFun;
    testFun();
    p("PASS");
  } catch(e) {
    p("ERROR");
    p("Exception raised: "+e.toString());
    p("Backtrace: "+e.stack);
  }
  var duration = new Date().getTime() - start;
  p(duration+"ms\n");
};


// Use T to perform a test that returns false on failure and if the test fails,
// display the line that failed.
// Example:
// T(MyValue==1);
function T(arg1, arg2) {
  if (!arg1) {
    p("Assertion failed: "+(arg2 != null ? arg2 : arg1).toString());
    numFailures += 1
  }
}

p("Running CouchDB Test Suite\n");
p("Host: "+CouchDB.host);

try {
  p("Version: "+CouchDB.getVersion()+"\n");
  runAllTestsConsole();
  // runTestConsole(tests.attachments);
} catch (e) {
  p(e.toString());
}

p("\nFinished");
