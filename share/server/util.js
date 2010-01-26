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
    return JSON.stringify(val);
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
