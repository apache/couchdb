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
 * Quit current test execution if it is tagged as skipped or ported to elixir
 */
function quitIfSkippedOrPorted() {
  if(couchTests.skip) {
    quit(2);
  }

  if(couchTests.elixir) {
    quit(3);
  }
}

/*
 * Futon test suite was designed to be able to run all tests populated into
 * couchTests. Here we should only be loading one test, so we'll pop the first
 * test off the list and run the test. If more than one item is loaded in the
 * test object, return an error.
 */
function runTest() {
  CouchDB.reloadConfig();
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
    quit(0);
  } catch(e) {
    console.log("\nError: " + e.message);
    fmtStack(e.stack);
    quit(1)
  }
}

quitIfSkippedOrPorted();

waitForSuccess(CouchDB.isRunning, 'isRunning');

runTest();
