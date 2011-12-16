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
 
 
 
couchTests.proxyauth = function(debug) {
  // this test proxy authentification handler
  
  var usersDb = new CouchDB("test_suite_users", {"X-Couch-Full-Commit":"false"});
  var db = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"false"});
  
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
    usersDb.deleteDb();
    db.deleteDb();
    db.createDb();
    
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
    
    var req = CouchDB.request("GET", "/test_suite_db/_design/test/_show/welcome",
                        {headers: headers});
    T(req.responseText == "Welcome benoitc@apache.org");
    
    req = CouchDB.request("GET", "/test_suite_db/_design/test/_show/role",
                        {headers: headers});
    T(req.responseText == "test");
    
    var xhr = CouchDB.request("PUT", "/_config/couch_httpd_auth/proxy_use_secret",{
      body : JSON.stringify("true"),
      headers: {"X-Couch-Persist": "false"}
    });
    T(xhr.status == 200);
    
    req = CouchDB.request("GET", "/test_suite_db/_design/test/_show/welcome",
                        {headers: headers});
    T(req.responseText == "Welcome benoitc@apache.org");
    
    req = CouchDB.request("GET", "/test_suite_db/_design/test/_show/role",
                        {headers: headers});
    T(req.responseText == "test");
    
  }
  
  run_on_modified_server(
    [{section: "httpd",
      key: "authentication_handlers",
      value:"{couch_httpd_auth, proxy_authentification_handler}, {couch_httpd_auth, default_authentication_handler}"},
      {section: "couch_httpd_auth",
        key: "authentication_db", 
        value: "test_suite_users"},
      {section: "couch_httpd_auth",
        key: "secret", 
        value: secret},
      {section: "couch_httpd_auth",
        key: "x_auth_username", 
        value: "X-Auth-CouchDB-UserName"},
      {section: "couch_httpd_auth",
        key: "x_auth_roles", 
        value: "X-Auth-CouchDB-Roles"},
      {section: "couch_httpd_auth",
        key: "x_auth_token", 
        value: "X-Auth-CouchDB-Token"},
      {section: "couch_httpd_auth",
        key: "proxy_use_secret", 
        value: "false"}],
    TestFun
  );
  
};