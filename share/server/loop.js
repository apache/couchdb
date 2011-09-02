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

var sandbox = null;
var filter_sandbox = null;

function init_sandbox() {
  try {
    // if possible, use evalcx (not always available)
    sandbox = evalcx('');
    sandbox.emit = Views.emit;
    sandbox.sum = Views.sum;
    sandbox.log = log;
    sandbox.toJSON = Couch.toJSON;
    sandbox.JSON = JSON;
    sandbox.provides = Mime.provides;
    sandbox.registerType = Mime.registerType;
    sandbox.start = Render.start;
    sandbox.send = Render.send;
    sandbox.getRow = Render.getRow;
    sandbox.isArray = isArray;
  } catch (e) {
    //log(e.toSource());
  }
};
init_sandbox();

function init_filter_sandbox() {
  try {
    filter_sandbox = evalcx('');
    for (var p in sandbox) {
      if (sandbox.hasOwnProperty(p)) {
        filter_sandbox[p] = sandbox[p];
      }
    }
    filter_sandbox.emit = Filter.emit;
  } catch(e) {
    log(e.toSource());
  }
};

init_filter_sandbox();

// Commands are in the form of json arrays:
// ["commandname",..optional args...]\n
//
// Responses are json values followed by a new line ("\n")

var DDoc = (function() {
  var ddoc_dispatch = {
    "lists"     : Render.list,
    "shows"    : Render.show,
    "filters"   : Filter.filter,
    "views"     : Filter.filter_view, 
    "updates"  : Render.update,
    "validate_doc_update" : Validate.validate
  };
  var ddocs = {};
  return {
    ddoc : function() {
      var args = [];
      for (var i=0; i < arguments.length; i++) {
        args.push(arguments[i]);
      };
      var ddocId = args.shift();
      if (ddocId == "new") {
        // get the real ddocId.
        ddocId = args.shift();
        // store the ddoc, functions are lazily compiled.
        ddocs[ddocId] = args.shift();
        print("true");
      } else {
        // Couch makes sure we know this ddoc already.
        var ddoc = ddocs[ddocId];
        if (!ddoc) throw(["fatal", "query_protocol_error", "uncached design doc: "+ddocId]);
        var funPath = args.shift();
        var cmd = funPath[0];
        // the first member of the fun path determines the type of operation
        var funArgs = args.shift();
        if (ddoc_dispatch[cmd]) {
          // get the function, call the command with it
          var point = ddoc;
          for (var i=0; i < funPath.length; i++) {
            if (i+1 == funPath.length) {
              var fun = point[funPath[i]];
              if (!fun) {
                throw(["error","not_found",
                       "missing " + funPath[0] + " function " + funPath[i] +
                       " on design doc " + ddocId]);
              }
              if (typeof fun != "function") {
                fun = Couch.compileFunction(fun, ddoc);
                // cache the compiled fun on the ddoc
                point[funPath[i]] = fun;
              };
            } else {
              point = point[funPath[i]];
            }
          };

          // run the correct responder with the cmd body
          ddoc_dispatch[cmd].apply(null, [fun, ddoc, funArgs]);
        } else {
          // unknown command, quit and hope the restarted version is better
          throw(["fatal", "unknown_command", "unknown ddoc command '" + cmd + "'"]);
        }
      }
    }
  };
})();

var Loop = function() {
  var line, cmd, cmdkey, dispatch = {
    "ddoc"     : DDoc.ddoc,
    // "view"    : Views.handler,
    "reset"    : State.reset,
    "add_fun"  : State.addFun,
    "add_lib"  : State.addLib,
    "map_doc"  : Views.mapDoc,
    "reduce"   : Views.reduce,
    "rereduce" : Views.rereduce
  };
  function handleError(e) {
    var type = e[0];
    if (type == "fatal") {
      e[0] = "error"; // we tell the client it was a fatal error by dying
      respond(e);
      quit(-1);
    } else if (type == "error") {
      respond(e);
    } else if (e.error && e.reason) {
      // compatibility with old error format
      respond(["error", e.error, e.reason]);
    } else {
      respond(["error","unnamed_error",e.toSource()]);
    }
  };
  while (line = readline()) {
    cmd = JSON.parse(line);
    State.line_length = line.length;
    try {
      cmdkey = cmd.shift();
      if (dispatch[cmdkey]) {
        // run the correct responder with the cmd body
        dispatch[cmdkey].apply(null, cmd);
      } else {
        // unknown command, quit and hope the restarted version is better
        throw(["fatal", "unknown_command", "unknown command '" + cmdkey + "'"]);
      }
    } catch(e) {
      handleError(e);
    }
  };
};

Loop();
