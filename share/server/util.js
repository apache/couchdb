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

var resolveModule = function(names, mod, root) {
  if (names.length == 0) {
    if (typeof mod.current != "string") {
      throw ["error","invalid_require_path",
        'Must require a JavaScript string, not: '+(typeof mod.current)];
    }
    return {
      current : mod.current,
      parent : mod.parent,
      id : mod.id,
      exports : {}
    };
  }
  // we need to traverse the path
  var n = names.shift();
  if (n == '..') {
    if (!(mod.parent && mod.parent.parent)) {
      throw ["error", "invalid_require_path", 'Object has no parent '+JSON.stringify(mod.current)];
    }
    return resolveModule(names, {
      id : mod.id.slice(0, mod.id.lastIndexOf('/')),
      parent : mod.parent.parent.parent,
      current : mod.parent.parent.current
    });
  } else if (n == '.') {
    if (!mod.parent) {
      throw ["error", "invalid_require_path", 'Object has no parent '+JSON.stringify(mod.current)];
    }
    return resolveModule(names, {
      parent : mod.parent.parent,
      current : mod.parent.current,
      id : mod.id
    });
  } else if (root) {
    mod = {current : root};
  }
  if (!mod.current[n]) {
    throw ["error", "invalid_require_path", 'Object has no property "'+n+'". '+JSON.stringify(mod.current)];
  }
  return resolveModule(names, {
    current : mod.current[n],
    parent : mod,
    id : mod.id ? mod.id + '/' + n : n
  });
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
          var require = function(name, module) {
            module = module || {};
            var newModule = resolveModule(name.split('/'), module, ddoc);
            var s = "function (module, exports, require) { " + newModule.current + " }";
            try {
              var func = sandbox ? evalcx(s, sandbox) : eval(s);
              func.apply(sandbox, [newModule, newModule.exports, function(name) {
                return require(name, newModule);
              }]);
            } catch(e) { 
              throw ["error","compilation_error","Module require('"+name+"') raised error "+e.toSource()]; 
            }
            return newModule.exports;
          };
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
    try {
      seal(obj);
    } catch (x) {
      // Sealing of arrays broken in some SpiderMonkey versions.
      // https://bugzilla.mozilla.org/show_bug.cgi?id=449657
    }
    for (var propname in obj) {
      if (typeof obj[propname] == "object") {
        arguments.callee(obj[propname]);
      }
    }
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
  // idea: query_server_config option for log level
  if (typeof message == "xml") {
    message = message.toXMLString();
  } else if (typeof message != "string") {
    message = Couch.toJSON(message);
  }
  respond(["log", String(message)]);
};

function isArray(obj) {
  return toString.call(obj) === "[object Array]";
}
