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

couchTests.oauth_users_db = function(debug) {
  // This tests OAuth authentication using the _users DB instead of the ini
  // configuration for storing OAuth tokens and secrets.

  if (debug) debugger;

  var usersDb = new CouchDB("test_suite_users",{"X-Couch-Full-Commit":"false"});
  var db = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"false"});
  var host = CouchDB.host;
  var authorization_url = "/_oauth/authorize";


  // Simple secret key generator
  function generateSecret(length) {
    var tab = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
    var secret = '';
    for (var i = 0; i < length; i++) {
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


  // this function will be called on the modified server
  var testFun = function () {
    var fdmanana = CouchDB.prepareUserDoc({
      name: "fdmanana",
      roles: ["dev"],
      oauth: {
        consumer_keys: {
          "key_foo": "bar",
          "key_xpto": "mars"
        },
        tokens: {
          "salut": "ola",
          "tok1": "123"
        }
      }
    }, "qwerty");
    TEquals(true, usersDb.save(fdmanana).ok);

    var signatureMethods = ["PLAINTEXT", "HMAC-SHA1"];
    var message, xhr, responseMessage, accessor, data;

    for (var i = 0; i < signatureMethods.length; i++) {
      message = {
        parameters: {
          oauth_signature_method: signatureMethods[i],
          oauth_consumer_key: "key_foo",
          oauth_token: "tok1",
          oauth_version: "1.0"
        }
      };
      accessor = {
        consumerSecret: "bar",
        tokenSecret: "123"
      };

      xhr = oauthRequest("GET", CouchDB.protocol + host + "/_oauth/request_token",
        message, accessor
      );
      TEquals(200, xhr.status);

      responseMessage = OAuth.decodeForm(xhr.responseText);

      // Obtaining User Authorization
      // Only needed for 3-legged OAuth
      //xhr = CouchDB.request(
      //  "GET", authorization_url + '?oauth_token=' + responseMessage.oauth_token);
      //TEquals(200, xhr.status);

      xhr = oauthRequest(
        "GET", CouchDB.protocol + host + "/_session", message, accessor);
      TEquals(200, xhr.status);
      data = JSON.parse(xhr.responseText);
      TEquals(true, data.ok);
      TEquals("object", typeof data.userCtx);
      TEquals("fdmanana", data.userCtx.name);
      TEquals("dev", data.userCtx.roles[0]);
      TEquals("oauth", data.info.authenticated);

      // test invalid token
      message.parameters.oauth_token = "not a token!";
      xhr = oauthRequest("GET", CouchDB.protocol + host + "/_session",
        message, accessor
      );
      TEquals(400, xhr.status, "Request should be invalid.");

      // test invalid secret
      message.parameters.oauth_token = "tok1";
      accessor.tokenSecret = "badone";
      xhr = oauthRequest("GET", CouchDB.protocol + host + "/_session",
        message, accessor
      );
      data = JSON.parse(xhr.responseText);
      TEquals(null, data.userCtx.name);
      TEquals(1, data.userCtx.roles.length);
      TEquals("_admin", data.userCtx.roles[0]);
      TEquals(true, data.info.authentication_handlers.indexOf("default") >= 0);
      TEquals("default", data.info.authenticated);
    }
  };


  usersDb.deleteDb();

  run_on_modified_server(
    [
     {section: "httpd",
      key: "WWW-Authenticate", value: 'OAuth'},
     {section: "couch_httpd_auth",
      key: "secret", value: generateSecret(64)},
     {section: "couch_httpd_auth",
      key: "authentication_db", value: usersDb.name},
     {section: "couch_httpd_oauth",
      key: "use_users_db", value: "true"},
     {section: "httpd", key: "authentication_handlers",
      value: "{couch_httpd_oauth, oauth_authentication_handler}, " +
        "{couch_httpd_auth, default_authentication_handler}"}
    ],
    testFun
  );

  // cleanup
  usersDb.deleteDb();
  db.deleteDb();
};
