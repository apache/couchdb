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

function create_sandbox() {
  var sandbox = {};
  sandbox.emit = Views.emit;
  sandbox.sum = Views.sum;
  sandbox.log = log;
  sandbox.toJSON = JSON.stringify;
  sandbox.JSON = JSON;
  sandbox.provides = Mime.provides;
  sandbox.registerType = Mime.registerType;
  sandbox.start = Render.start;
  sandbox.send = Render.send;
  sandbox.getRow = Render.getRow;
  sandbox.isArray = isArray;
  return sandbox;
};

function create_filter_sandbox() {
  var sandbox = create_sandbox();
  sandbox.emit = Filter.emit;
  return sandbox;
};

function create_dreyfus_sandbox() {
  var sandbox = create_sandbox();
  sandbox.index = Dreyfus.index;
  return sandbox;
};

function create_nouveau_sandbox() {
  var sandbox = create_sandbox();
  sandbox.index = Nouveau.index;
  return sandbox;
};

// This is a copy from loop.js
var DDoc = (function() {
  var ddoc_dispatch = {
    "lists"     : Render.list,
    "shows"    : Render.show,
    "filters"   : Filter.filter,
    "views"     : Filter.filter_view,
    "updates"  : Render.update,
    "validate_doc_update" : Validate.validate,
    "rewrites"  : Render.rewrite
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
                // For filter_view we want the emit() function
                // to be overridden and just toggle a flag instead of
                // accumulating rows
                var sandbox = (cmd === "views") ? create_filter_sandbox() : create_sandbox();
                fun = Couch.compileFunction(fun, ddoc, funPath.join('.'), sandbox);
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

// This mostly a copy from loop.js handleError
function handleError(e) {
    if (e === null) {
      // internal error, another possibility when out of memory
      // we tell the client it was a fatal error by dying
      respond(["error", "internal_error", null]);
      return false;
    }
    const type = e[0];
    if (type == "fatal") {
      e[0] = "error"; // we tell the client it was a fatal error by dying
      respond(e);
      return false;
    } else if (type == "error") {
      respond(e);
      return true;
    } else if (e.name == "InternalError") {
      // If the internal error is caught by handleViewError it will be
      // re-thrown as a ["fatal", ...] error, and we already handle that above.
      // Here we handle the case when the error is thrown outside of
      // handleViewError, for instance when serializing the rows to be sent
      // back to the user
      respond(["error", e.name, e.message]);
      return false;
    } else if (e.error && e.reason) {
      // compatibility with old error format
      respond(["error", e.error, e.reason]);
      return true;
    } else if (e.name) {
      respond(["error", e.name, e]);
      return true;
    } else {
      respond(["error","unnamed_error", e.stack]);
      return true;
    }
  };

globalThis.dispatch = function(line) {
  const cmd = JSON.parse(line);
  State.line_length = line.length;
  try {
    switch (cmd.shift()) {
    case "ddoc":
      DDoc.ddoc.apply(null, cmd);
      break;
    case "reset":
      State.reset.apply(null, cmd);
      break;
    case "add_fun":
      State.addFun.apply(null, cmd);
      break;
    case "add_lib":
      State.addLib.apply(null, cmd);
      break;
    case "map_doc":
      Views.mapDoc.apply(null, cmd);
      break;
    case "index_doc":
      Dreyfus.indexDoc.apply(null, cmd);
      break;
    case "nouveau_index_doc":
      Nouveau.indexDoc.apply(null, cmd);
      break;
    case "reduce":
      Views.reduce.apply(null, cmd);
      break;
    case "rereduce":
      Views.rereduce.apply(null, cmd);
      break;
    default:
      // unknown command, quit and hope the restarted version is better
      throw(["fatal", "unknown_command", "unknown command '" + cmdkey + "'"]);
    }
  } catch(e) {
      return handleError(e);
  };
  return true;
};
