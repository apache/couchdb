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

var resolveModule = function(names, parent, current, path) {
  if (names.length == 0) {
    if (typeof current != "string") {
      throw ["error","invalid_require_path",
        'Must require a JavaScript string, not: '+(typeof current)];
    }
    return [current, parent, path];
  }
  // we need to traverse the path
  var n = names.shift();
  if (n == '..') {
    if (!(parent && parent.parent)) {
      throw ["error", "invalid_require_path", 'Object has no parent '+JSON.stringify(current)];
    }
    path = path.slice(0, path.lastIndexOf('/'));
    return resolveModule(names, parent.parent.parent, parent.parent, path);
  } else if (n == '.') {
    if (!parent) {
      throw ["error", "invalid_require_path", 'Object has no parent '+JSON.stringify(current)];
    }
    return resolveModule(names, parent.parent, parent, path);
  }
  if (!current[n]) {
    throw ["error", "invalid_require_path", 'Object has no property "'+n+'". '+JSON.stringify(current)];
  }
  var p = current;
  current = current[n];
  current.parent = p;
  path = path ? path + '/' + n : n;
  return resolveModule(names, p, current, path);
};

var Couch = {
  // moving this away from global so we can move to json2.js later
  toJSON : function (val) {
    return JSON.stringify(val);
  },
  compileFunction : function(source, ddoc) {    
    if (!source) throw(["error","not_found","missing function"]);
    try {
      if (sandbox) {
        if (ddoc) {
          var require = function(name, parent) {
            if (!parent) {parent = {}};
            var resolved = resolveModule(name.split('/'), parent.actual, ddoc, parent.id);
            var s = "function (module, exports, require) { " + resolved[0] + " }";
            var module = {id:resolved[2], actual:resolved[1]};
            module.exports = {};
            try {
              var func = sandbox ? evalcx(s, sandbox) : eval(s);
              func.apply(sandbox, [module, module.exports, function(name) {return require(name, module)}]);
            } catch(e) { 
              throw ["error","compilation_error","Module require('"+name+"') raised error "+e.toSource()]; 
            }
            return module.exports;
          }
          sandbox.require = require;
        }
        var functionObject = evalcx(source, sandbox);
      } else {
        var functionObject = eval(source);
      }
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
  // idea: query_server_config option for log level
  if (typeof message != "string") {
    message = Couch.toJSON(message);
  }
  respond(["log", message]);
};
