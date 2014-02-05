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
//

/*
 * Futon test suite was designed to be able to run all tests populated into
 * couchTests. Here we should only be loading one test, so we'll pop the first
 * test off the list and run the test. If more than one item is loaded in the
 * test object, return an error.
 */
function runTest() {
  var count = 0;
  var start = new Date().getTime();

  for(var name in couchTests) {
      count++;
  }

  if (count !== 1) {
      console.log('Only one test per file is allowed.');
      quit(1);
  }

  try {
    // Add artificial wait for each test of 1 sec
    while (new Date().getTime() < start + 1200);
    couchTests[name]();
    print('OK');
  } catch(e) {
    console.log("FAIL\nReason: " + e.message);
    fmtStack(e.stack);
    quit(1);
  }
}

waitForSuccess(CouchDB.isRunning, 'isRunning');

runTest();
