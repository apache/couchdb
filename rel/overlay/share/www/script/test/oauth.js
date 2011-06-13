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

couchTests.oauth = function(debug) {
  // This tests OAuth authentication.

  var authorization_url = "/_oauth/authorize";

  var db = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"false"});
  db.deleteDb();
  db.createDb();
  if (debug) debugger;

  var dbA = new CouchDB("test_suite_db_a", {"X-Couch-Full-Commit":"false"});
  var dbB = new CouchDB("test_suite_db_b", {"X-Couch-Full-Commit":"false"});
  dbA.deleteDb();
  dbA.createDb();
  dbB.deleteDb();
  dbB.createDb();

  // Simple secret key generator
  function generateSecret(length) {
    var tab = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
    var secret = '';
    for (var i=0; i<length; i++) {
      secret += tab.charAt(Math.floor(Math.random() * 64));
    }
    return secret;
  }

  function oauthRequest(method, path, message, accessor) {
    message.action = path;
    message.method = method || 'GET';
    OAuth.SignatureMethod.sign(message, accessor);
    var parameters = message.parameters;
    if (method == "POST" || method == "GET") {
      if (method == "GET") {
        return CouchDB.request("GET", OAuth.addToURL(path, parameters));
      } else {
        return CouchDB.request("POST", path, {
          headers: {"Content-Type": "application/x-www-form-urlencoded"},
          body: OAuth.formEncode(parameters)
        });
      }
    } else {
      return CouchDB.request(method, path, {
        headers: {Authorization: OAuth.getAuthorizationHeader('', parameters)}
      });
    }
  }

  var consumerSecret = generateSecret(64);
  var tokenSecret = generateSecret(64);
  var admintokenSecret = generateSecret(64);
  var testadminPassword = "ohsosecret";

  var adminBasicAuthHeaderValue = function() {
    var retval = 'Basic ' + binb2b64(str2binb("testadmin:" + testadminPassword));
    return retval;
  }

  var host = CouchDB.host;
  var dbPair = {
    source: {
      url: CouchDB.protocol + host + "/test_suite_db_a",
      auth: {
        oauth: {
          consumer_key: "key",
          consumer_secret: consumerSecret,
          token_secret: tokenSecret,
          token: "foo"
        }
      }
    },
    target: {
      url: CouchDB.protocol + host + "/test_suite_db_b",
      headers: {"Authorization": adminBasicAuthHeaderValue()}
    }
  };

  // this function will be called on the modified server
  var testFun = function () {
    try {
      CouchDB.request("PUT", CouchDB.protocol + host + "/_config/admins/testadmin", {
        headers: {"X-Couch-Persist": "false"},
        body: JSON.stringify(testadminPassword)
      });
      var i = 0;
      waitForSuccess(function() {
        //loop until the couch server has processed the password
        i += 1;
        var xhr = CouchDB.request("GET", CouchDB.protocol + host + "/_config/admins/testadmin?foo="+i,{
            headers: {
              "Authorization": adminBasicAuthHeaderValue()
            }});
        if (xhr.responseText.indexOf("\"-hashed-") != 0) {
            throw("still waiting");
        }
      }, "wait-for-admin");

      CouchDB.newUuids(2); // so we have one to make the salt

      CouchDB.request("PUT", CouchDB.protocol + host + "/_config/couch_httpd_auth/require_valid_user", {
        headers: {
          "X-Couch-Persist": "false",
          "Authorization": adminBasicAuthHeaderValue()
        },
        body: JSON.stringify("true")
      });

      var usersDb = new CouchDB("test_suite_users", {
        "X-Couch-Full-Commit":"false",
        "Authorization": adminBasicAuthHeaderValue()
      });
      usersDb.deleteDb();
      usersDb.createDb();
        
      // Create a user
      var jasonUserDoc = CouchDB.prepareUserDoc({
        name: "jason",
        roles: ["test"]
      }, "testpassword");
      T(usersDb.save(jasonUserDoc).ok);


      var accessor = {
        consumerSecret: consumerSecret,
        tokenSecret: tokenSecret
      };
      var adminAccessor = {
        consumerSecret: consumerSecret,
        tokenSecret: admintokenSecret
      };

      var signatureMethods = ["PLAINTEXT", "HMAC-SHA1"];
      var consumerKeys = {key: 200, nonexistent_key: 400};
      for (var i=0; i<signatureMethods.length; i++) {
        for (var consumerKey in consumerKeys) {
          var expectedCode = consumerKeys[consumerKey];
          var message = {
            parameters: {
              oauth_signature_method: signatureMethods[i],
              oauth_consumer_key: consumerKey,
              oauth_token: "foo",
              oauth_token_secret: tokenSecret,
              oauth_version: "1.0"
            }
          };

          // Get request token via Authorization header
          xhr = oauthRequest("GET", CouchDB.protocol + host + "/_oauth/request_token", message, accessor);
          T(xhr.status == expectedCode);

          // GET request token via query parameters
          xhr = oauthRequest("GET", CouchDB.protocol + host + "/_oauth/request_token", message, accessor);
          T(xhr.status == expectedCode);

          responseMessage = OAuth.decodeForm(xhr.responseText);

          // Obtaining User Authorization
          //Only needed for 3-legged OAuth
          //xhr = CouchDB.request("GET", authorization_url + '?oauth_token=' + responseMessage.oauth_token);
          //T(xhr.status == expectedCode);

          xhr = oauthRequest("GET", CouchDB.protocol + host + "/_session", message, accessor);
          T(xhr.status == expectedCode);
          if (xhr.status == expectedCode == 200) {
            data = JSON.parse(xhr.responseText);
            T(data.name == "jason");
            T(data.roles[0] == "test");
          }

          xhr = oauthRequest("GET", CouchDB.protocol + host + "/_session?foo=bar", message, accessor);
          T(xhr.status == expectedCode);

          // Test HEAD method
          xhr = oauthRequest("HEAD", CouchDB.protocol + host + "/_session?foo=bar", message, accessor);
          T(xhr.status == expectedCode);

          // Replication
          var dbA = new CouchDB("test_suite_db_a", {
            "X-Couch-Full-Commit":"false",
            "Authorization": adminBasicAuthHeaderValue()
          });
          T(dbA.save({_id:"_design/"+i+consumerKey}).ok);
          var result = CouchDB.replicate(dbPair.source, dbPair.target, {
            headers: {"Authorization": adminBasicAuthHeaderValue()}
          });
          T(result.ok);

          // Test auth via admin user defined in .ini
          var message = {
            parameters: {
              oauth_signature_method: signatureMethods[i],
              oauth_consumer_key: consumerKey,
              oauth_token: "bar",
              oauth_token_secret: admintokenSecret,
              oauth_version: "1.0"
            }
          };
          xhr = oauthRequest("GET", CouchDB.protocol + host + "/_session?foo=bar", message, adminAccessor);
          if (xhr.status == expectedCode == 200) {
            data = JSON.parse(xhr.responseText);
            T(data.name == "testadmin");
            T(data.roles[0] == "_admin");
          }

          // Test when the user's token doesn't exist.
          message.parameters.oauth_token = "not a token!";
          xhr = oauthRequest("GET", CouchDB.protocol + host + "/_session?foo=bar",
                  message, adminAccessor);
          T(xhr.status == 400, "Request should be invalid.");
        }
      }
    } finally {
      var xhr = CouchDB.request("PUT", CouchDB.protocol + host + "/_config/couch_httpd_auth/require_valid_user", {
        headers: {
          "Authorization": adminBasicAuthHeaderValue(),
          "X-Couch-Persist": "false"
        },
        body: JSON.stringify("false")
      });
      T(xhr.status == 200);

      var xhr = CouchDB.request("DELETE", CouchDB.protocol + host + "/_config/admins/testadmin", {
        headers: {
          "Authorization": adminBasicAuthHeaderValue(),
          "X-Couch-Persist": "false"
        }
      });
      T(xhr.status == 200);
    }
  };

  run_on_modified_server(
    [
     {section: "httpd",
      key: "WWW-Authenticate", value: 'OAuth'},
     {section: "couch_httpd_auth",
      key: "secret", value: generateSecret(64)},
     {section: "couch_httpd_auth",
      key: "authentication_db", value: "test_suite_users"},
     {section: "oauth_consumer_secrets",
      key: "key", value: consumerSecret},
     {section: "oauth_token_users",
      key: "foo", value: "jason"},
     {section: "oauth_token_users",
      key: "bar", value: "testadmin"},
     {section: "oauth_token_secrets",
      key: "foo", value: tokenSecret},
     {section: "oauth_token_secrets",
      key: "bar", value: admintokenSecret},
     {section: "couch_httpd_oauth",
      key: "authorization_url", value: authorization_url}
    ],
    testFun
  );
};
