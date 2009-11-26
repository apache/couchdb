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
    print("# " + msg);
  }
};

function T(arg1, arg2) {
  if(!arg1) {
    throw((arg2 ? arg2 : arg1).toString());
  }
}

function runTestConsole(num, name, func) {
  try {
    func();
    print("ok " + num + " " + name);
  } catch(e) {
    msg = e.toString();
    msg = msg.replace(/\n/g, "\n    ");
    print("not ok " + num + " " + name + " " + msg);
  }
}

function runAllTestsConsole() {
  var numTests = 0;
  for(var t in couchTests) { numTests += 1; }
  print("1.." + numTests);
  var testId = 0;
  for(var t in couchTests) {
    testId += 1;
    runTestConsole(testId, t, couchTests[t]);
  }
};

try {
  runAllTestsConsole();
} catch (e) {
  p("# " + e.toString());
}
