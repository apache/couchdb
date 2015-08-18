// Licensed under the Apache License, Version 2.0 (the "License"); you may not
// use this file except in compliance with the License.  You may obtain a copy
// of the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
// WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
// License for the specific language governing permissions and limitations under
// the License.

couchTests.couch_auth_lockout = function(debug) {
  // This tests auth lockout.

  var timeLogin = function (user, pass) {
    var start = new Date().getTime();
    CouchDB.login(user, pass);
    var end = new Date().getTime();
    return end - start;
  };

  var testFun = function () {
    // fails quickly
    T(timeLogin("foo", "bar") < 10);
    T(timeLogin("foo", "bar") < 10);

    // fails slowly
    T(timeLogin("foo", "bar") >= 2000);
  };

  run_on_modified_server(
    [
      {section: "couch_auth_lockout",
       key: "lockout_secs", value: "2"},
      {section: "couch_auth_lockout",
       key: "max_attempts", value: "2"}
    ],
    testFun
  );
}
