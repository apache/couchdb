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

function runTestConsole(num, name, func) {
  var passed = false;
  try {
    func();
    passed = true;
    print("ok " + num + " " + name);
  } catch(e) {
    print("not ok " + num + " " + name);
    console.log("Reason: " + e.message);
    fmtStack(e.stack);
  }
  return passed;
}

function runAllTestsConsole() {
  var numTests = 0;
  var numPassed = 0;
  for(var t in couchTests) { numTests += 1; }
  print("1.." + numTests);
  var testId = 0;
  for(var t in couchTests) {
    testId += 1;
    if(runTestConsole(testId, t, couchTests[t])) {
      numPassed++;
    }
  }
  if(numPassed != numTests) {
    console.log("Test failures: " + (numTests - numPassed));
    quit(1);
  } else {
    console.log("All tests passed");
  }
};

waitForSuccess(CouchDB.getVersion);
runAllTestsConsole();
