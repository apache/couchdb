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

/*
 * Add global couchTests object required for existing tests.
 */
var couchTests = {}; 

var console = { 
  log: function(arg) {
    var msg = (arg.toString()).replace(/\n/g, "\n    ");
    print(msg, true);
  }
};

var fmtStack = function(stack) {
  if(!stack) {
    console.log("No stack information");
    return;
  }
  console.log("Trace back (most recent call first):\n");
  var re = new RegExp("(.*?)@([^:]*):(.*)$");
  var lines = stack.split("\n");
  for(var i = 0; i < lines.length; i++) {
    var line = lines[i];
    if(!line.length) continue;
    var match = re.exec(line);
    if(!match) continue
    var match = re.exec(line);
    if(!match) continue
    var source = match[1].substr(0, 70);
    var file = match[2];
    var lnum = match[3];
    while(lnum.length < 3) lnum = " " + lnum;
    console.log(" " + lnum + ": " + file);
    console.log("      " + source);
  }
} 

function T(arg1, arg2) {
  if(!arg1) {
    var result = (arg2 ? arg2 : arg1);
    throw((result instanceof Error ? result : Error(result)));
  }
} 

function waitForSuccess(fun, tag) {
  var start = new Date().getTime();
  var complete = false;
  
  while (!complete) {
    var now = new Date().getTime();
    if (now > start + 5000) {
      complete = true;
      print('\nFAIL ' + tag);
      quit(1);
    }
    try {
      while (new Date().getTime() < now + 500);
      complete = fun();
    } catch (e) {}
  }
}

function restartServer() {
  print('restart');
  var start = new Date().getTime();
  while (new Date().getTime() < start + 1000);
  waitForSuccess(CouchDB.isRunning, 'restart');
}

/*
 * If last_req is an object, we got something back. This might be an error, but
 * CouchDB is up and running!
 */
CouchDB.isRunning = function() {
  CouchDB.last_req = CouchDB.request("GET", "/");
  return typeof CouchDB.last_req == 'object';
};
