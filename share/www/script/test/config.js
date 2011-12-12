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
    if(CouchDB.protocol == "http://") {
      port = "80";
    }
    if(CouchDB.protocol == "https://") {
      port = "443";
    }
  } else {
    port = server_port.pop();
  }

  if(CouchDB.protocol == "http://") {
    config_port = config.httpd.port;
  }
  if(CouchDB.protocol == "https://") {
    config_port = config.ssl.port;
  }

  if(port) {
    TEquals(config_port, port, "ports should match");
  }

  T(config.couchdb.database_dir);
  T(config.daemons.httpd);
  T(config.httpd_global_handlers._config);
  T(config.log.level);
  T(config.query_servers.javascript);

  // test that settings can be altered, and that an undefined whitelist allows any change
  TEquals(undefined, config.httpd.config_whitelist, "Default whitelist is empty");
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

  // Non-term whitelist values allow further modification of the whitelist.
  xhr = CouchDB.request("PUT", "/_config/httpd/config_whitelist",{
    body : JSON.stringify("!This is an invalid Erlang term!"),
    headers: {"X-Couch-Persist": "false"}
  });
  TEquals(200, xhr.status, "Set config whitelist to an invalid Erlang term");
  xhr = CouchDB.request("DELETE", "/_config/httpd/config_whitelist",{
    headers: {"X-Couch-Persist": "false"}
  });
  TEquals(200, xhr.status, "Modify whitelist despite it being invalid syntax");

  // Non-list whitelist values allow further modification of the whitelist.
  xhr = CouchDB.request("PUT", "/_config/httpd/config_whitelist",{
    body : JSON.stringify("{[yes, a_valid_erlang_term, but_unfortunately, not_a_list]}"),
    headers: {"X-Couch-Persist": "false"}
  });
  TEquals(200, xhr.status, "Set config whitelist to an non-list term");
  xhr = CouchDB.request("DELETE", "/_config/httpd/config_whitelist",{
    headers: {"X-Couch-Persist": "false"}
  });
  TEquals(200, xhr.status, "Modify whitelist despite it not being a list");

  // Keys not in the whitelist may not be modified.
  xhr = CouchDB.request("PUT", "/_config/httpd/config_whitelist",{
    body : JSON.stringify("[{httpd,config_whitelist}, {test,foo}]"),
    headers: {"X-Couch-Persist": "false"}
  });
  TEquals(200, xhr.status, "Set config whitelist to something valid");

  ["PUT", "DELETE"].forEach(function(method) {
    ["test/not_foo", "not_test/foo", "neither_test/nor_foo"].forEach(function(pair) {
      var path = "/_config/" + pair;
      var test_name = method + " to " + path + " disallowed: not whitelisted";

      xhr = CouchDB.request(method, path, {
        body : JSON.stringify("Bummer! " + test_name),
        headers: {"X-Couch-Persist": "false"}
      });
      TEquals(400, xhr.status, test_name);
    });
  });

  // Keys in the whitelist may be modified.
  ["PUT", "DELETE"].forEach(function(method) {
    xhr = CouchDB.request(method, "/_config/test/foo",{
      body : JSON.stringify(method + " to whitelisted config variable"),
      headers: {"X-Couch-Persist": "false"}
    });
    TEquals(200, xhr.status, "Keys in the whitelist may be modified");
  });

  // Non-2-tuples in the whitelist are ignored
  xhr = CouchDB.request("PUT", "/_config/httpd/config_whitelist",{
    body : JSON.stringify("[{httpd,config_whitelist}, these, {are}, {nOt, 2, tuples}," +
                          " [so], [they, will], [all, become, noops], {test,foo}]"),
    headers: {"X-Couch-Persist": "false"}
  });
  TEquals(200, xhr.status, "Set config whitelist with some inert values");
  ["PUT", "DELETE"].forEach(function(method) {
    xhr = CouchDB.request(method, "/_config/test/foo",{
      body : JSON.stringify(method + " to whitelisted config variable"),
      headers: {"X-Couch-Persist": "false"}
    });
    TEquals(200, xhr.status, "Update whitelisted variable despite invalid entries");
  });

  // Atoms, binaries, and strings suffice as whitelist sections and keys.
  ["{test,foo}", '{"test","foo"}', '{<<"test">>,<<"foo">>}'].forEach(function(pair) {
    xhr = CouchDB.request("PUT", "/_config/httpd/config_whitelist",{
      body : JSON.stringify("[{httpd,config_whitelist}, " + pair + "]"),
      headers: {"X-Couch-Persist": "false"}
    });
    TEquals(200, xhr.status, "Set config whitelist to include " + pair);

    var pair_format = {"t":"tuple", '"':"string", "<":"binary"}[pair[1]];
    ["PUT", "DELETE"].forEach(function(method) {
      xhr = CouchDB.request(method, "/_config/test/foo",{
        body : JSON.stringify(method + " with " + pair_format),
        headers: {"X-Couch-Persist": "false"}
      });
      TEquals(200, xhr.status, "Whitelist works with " + pair_format);
    });
  });

  xhr = CouchDB.request("DELETE", "/_config/httpd/config_whitelist",{
    headers: {"X-Couch-Persist": "false"}
  });
  TEquals(200, xhr.status, "Reset config whitelist to undefined");
};
