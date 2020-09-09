// Licensed under the Apache License, Version 2.0 (the "License"); you may not
// use this file except in compliance with the License. You may obtain a copy of
// the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
// WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
// License for the specific language governing permissions and limitations under
// the License.

couchTests.elixir = true;
couchTests.proxyauth = function(debug) {
  // this test proxy authentification handler
  return console.log('done in test/elixir/test/proxyauth_test.exs');
  var users_db_name = get_random_db_name();
  var usersDb = new CouchDB(users_db_name, {"X-Couch-Full-Commit":"false"});
  usersDb.createDb();

  var db_name = get_random_db_name();
  var db = new CouchDB(db_name, {"X-Couch-Full-Commit":"false"});
  db.createDb();

  if (debug) debugger;

  // Simple secret key generator
  function generateSecret(length) {
    var tab = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
    var secret = '';
    for (var i=0; i<length; i++) {
      secret += tab.charAt(Math.floor(Math.random() * 64));
    }
    return secret;
  }

  var secret = generateSecret(64);

  function TestFun() {

    var benoitcUserDoc = CouchDB.prepareUserDoc({
      name: "benoitc@apache.org"
    }, "test");
    T(usersDb.save(benoitcUserDoc).ok);

    T(CouchDB.session().userCtx.name == null);

    // test that you can use basic auth aginst the users db
    var s = CouchDB.session({
      headers : {
        "Authorization" : "Basic YmVub2l0Y0BhcGFjaGUub3JnOnRlc3Q="
      }
    });
    T(s.userCtx.name == "benoitc@apache.org");
    T(s.info.authenticated == "default");

    CouchDB.logout();

/*  XXX: None of the rest of this is supported yet in 2.0
    var headers = {
      "X-Auth-CouchDB-UserName": "benoitc@apache.org",
      "X-Auth-CouchDB-Roles": "test",
      "X-Auth-CouchDB-Token": hex_hmac_sha1(secret, "benoitc@apache.org")
    };

    var designDoc = {
      _id:"_design/test",
      language: "javascript",

      shows: {
        "welcome": stringFun(function(doc,req) {
          return "Welcome " + req.userCtx["name"];
        }),
        "role": stringFun(function(doc, req) {
          return req.userCtx['roles'][0];
        })
      }
    };

    db.save(designDoc);

    var req = CouchDB.request("GET", "/" + db_name + "/_design/test/_show/welcome",
                        {headers: headers});
    T(req.responseText == "Welcome benoitc@apache.org", req.responseText);

    req = CouchDB.request("GET", "/" + db_name + "/_design/test/_show/role",
                        {headers: headers});
    T(req.responseText == "test");

    var xhr = CouchDB.request("PUT", "/_node/node1@127.0.0.1/_config/couch_httpd_auth/proxy_use_secret",{
      body : JSON.stringify("true"),
      headers: {"X-Couch-Persist": "false"}
    });
    T(xhr.status == 200);

    req = CouchDB.request("GET", "/" + db_name + "/_design/test/_show/welcome",
                        {headers: headers});
    T(req.responseText == "Welcome benoitc@apache.org");

    req = CouchDB.request("GET", "/" + db_name + "/_design/test/_show/role",
                        {headers: headers});
    T(req.responseText == "test");
*/

  }

  run_on_modified_server(
    [{section: "httpd",
      key: "authentication_handlers",
      value:"{chttpd_auth, proxy_authentication_handler}, {chttpd_auth, default_authentication_handler}"},
      {section: "chttpd_auth",
        key: "authentication_db",
        value: users_db_name},
      {section: "chttpd_auth",
        key: "secret",
        value: secret},
      {section: "chttpd_auth",
        key: "x_auth_username",
        value: "X-Auth-CouchDB-UserName"},
      {section: "chttpd_auth",
        key: "x_auth_roles",
        value: "X-Auth-CouchDB-Roles"},
      {section: "chttpd_auth",
        key: "x_auth_token",
        value: "X-Auth-CouchDB-Token"},
      {section: "chttpd_auth",
        key: "proxy_use_secret",
        value: "false"}],
    TestFun
  );

  // cleanup
  db.deleteDb();
  usersDb.deleteDb();

};
