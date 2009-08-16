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
    var secret = '';
    for (var i=0; i<length; i++) {
      secret += String.fromCharCode(Math.floor(Math.random() * 256));
    }
    return secret;
  }

  function oauthRequest(path, message, accessor, method) {
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
      return CouchDB.request("GET", path, {
        headers: {Authorization: OAuth.getAuthorizationHeader('', parameters)}
      });
    }
  }

  var consumerSecret = generateSecret(64);
  var tokenSecret = generateSecret(64);

  var host = CouchDB.host;
  var dbPair = {
    source: {
      url: "http://" + host + "/test_suite_db_a",
      auth: {
        oauth: {
          consumer_key: "key",
          consumer_secret: consumerSecret,
          token_secret: tokenSecret,
          token: "foo"
        }
      }
    },
    target: "http://" + host + "/test_suite_db_b"
  };

  // this function will be called on the modified server
  var testFun = function () {
    try {
      var usersDb = new CouchDB("test_suite_users", {"X-Couch-Full-Commit":"false"});
      usersDb.deleteDb();
      usersDb.createDb();
      
      // Create a user
      T(CouchDB.createUser("jason", "testpassword", "test@somemail.com", ['test'], true).ok);

      var accessor = {
        consumerSecret: consumerSecret,
        tokenSecret: tokenSecret
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
          xhr = oauthRequest("http://" + host + "/_oauth/request_token", message, accessor);
          T(xhr.status == expectedCode);

          // GET request token via query parameters
          xhr = oauthRequest("http://" + host + "/_oauth/request_token", message, accessor, "GET");
          T(xhr.status == expectedCode);

          responseMessage = OAuth.decodeForm(xhr.responseText);

          // Obtaining User Authorization
          //Only needed for 3-legged OAuth
          //xhr = CouchDB.request("GET", authorization_url + '?oauth_token=' + responseMessage.oauth_token);
          //T(xhr.status == expectedCode);

          xhr = oauthRequest("http://" + host + "/_session", message, accessor);
          T(xhr.status == expectedCode);
          if (xhr.status == expectedCode == 200) {
            data = JSON.parse(xhr.responseText);
            T(data.name == "jason");
            T(data.roles[0] == "test");
          }

          xhr = oauthRequest("http://" + host + "/_session?foo=bar", message, accessor);
          T(xhr.status == expectedCode);

          // Replication
          var result = CouchDB.replicate(dbPair.source, dbPair.target);
          T(result.ok);
        }
      }

    } finally {
    }
  };

  run_on_modified_server(
    [{section: "httpd",
      key: "WWW-Authenticate", value: 'Basic realm="administrator",OAuth'},
     {section: "couch_httpd_auth",
      key: "secret", value: generateSecret(64)},
     {section: "couch_httpd_auth",
      key: "authentication_db", value: "test_suite_users"},
     {section: "oauth_consumer_secrets",
      key: "key", value: consumerSecret},
     {section: "oauth_token_users",
      key: "foo", value: "jason"},
     {section: "oauth_token_secrets",
      key: "foo", value: tokenSecret},
     {section: "couch_httpd_oauth",
      key: "authorization_url", value: authorization_url},
     {section: "httpd",
      key: "authentication_handlers",
      value: "{couch_httpd_oauth, oauth_authentication_handler}, {couch_httpd_auth, default_authentication_handler}"}],
    testFun
  );
};
