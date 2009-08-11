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

try {
  // if possible, use evalcx (not always available)
  sandbox = evalcx('');
  sandbox.emit = emit;
  sandbox.sum = sum;
  sandbox.log = log;
  sandbox.toJSON = toJSON;
  sandbox.provides = provides;
  sandbox.registerType = registerType;
  sandbox.start = start;
  sandbox.send = send;
  sandbox.getRow = getRow;
} catch (e) {}

// Commands are in the form of json arrays:
// ["commandname",..optional args...]\n
//
// Responses are json values followed by a new line ("\n")

var line, cmd, cmdkey;

var dispatch = {
  "reset"    : State.reset,
  "add_fun"  : State.addFun,
  "map_doc"  : Views.mapDoc,
  "reduce"   : Views.reduce,
  "rereduce" : Views.rereduce,
  "validate" : Validate.validate,
  "show"     : Render.show,
  "update"   : Render.update,
  "list"     : Render.list,
  "filter"   : Filter.filter
};

while (line = eval(readline())) {
  cmd = eval(line);
  line_length = line.length;
  try {
    cmdkey = cmd.shift();
    if (dispatch[cmdkey]) {
      // run the correct responder with the cmd body
      dispatch[cmdkey].apply(this, cmd);
    } else {
      // unknown command, quit and hope the restarted version is better
      respond({
        error: "query_server_error",
        reason: "unknown command '" + cmdkey + "'"});
      quit();
    }
  } catch(e) {
    respond(e);
  }
};
