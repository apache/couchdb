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

couchTests.config = function(debug) {
  var db = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"false"});
  db.deleteDb();
  db.createDb();
  if (debug) debugger;

  // test that /_config returns all the settings
  var xhr = CouchDB.request("GET", "/_config");
  var config = JSON.parse(xhr.responseText);

  /*
    if we run on standard ports, we can't extract
    the number from the URL. Instead we try to guess
    from the protocol what port we are running on.
    If we can't guess, we don't test for the port.
    Overengineering FTW.
  */
  var server_port = CouchDB.host.split(':');
  if(server_port.length == 1 && CouchDB.inBrowser) {
    var proto = window.location.protocol;
    if(proto == "http:") {
      port = 80;
    }
    if(proto == "https:") {
      port = 443;
    }
  } else {
    port = server_port.pop();
  }

  if(port) {
    T(config.httpd.port == port);
  }

  T(config.couchdb.database_dir);
  T(config.daemons.httpd);
  T(config.httpd_global_handlers._config);
  T(config.log.level);
  T(config.query_servers.javascript);

  // test that settings can be altered
  xhr = CouchDB.request("PUT", "/_config/test/foo",{
    body : JSON.stringify("bar"),
    headers: {"X-Couch-Persist": "false"}
  });
  T(xhr.status == 200);
  xhr = CouchDB.request("GET", "/_config/test");
  config = JSON.parse(xhr.responseText);
  T(config.foo == "bar");

  // you can get a single key
  xhr = CouchDB.request("GET", "/_config/test/foo");
  config = JSON.parse(xhr.responseText);
  T(config == "bar");
};
