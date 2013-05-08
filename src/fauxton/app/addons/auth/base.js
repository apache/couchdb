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

define([
       "app",
       "api",
       "addons/auth/routes"
],

function(app, FauxtonAPI, Auth) {

  Auth.initialize = function() {
    var session = Auth.session = new Auth.Session();
    Auth.navLink = new Auth.NavLink({model: Auth.session});

    FauxtonAPI.addHeaderLink({
      title: "Auth", 
      href: "#_auth",
      view: Auth.navLink,
      establish: [Auth.session.fetchOnce()]
    });

    var auth = function (roles, layout) {
      var deferred = $.Deferred();

      var sessionDeferred = session.fetchOnce().then(function () {
        console.log(session);

        if (session.isAdminParty()) {
          deferred.resolve();
        } else if(session.matchesRoles(roles)) {
          deferred.resolve();
        } else {
          deferred.reject();
        }
      });

      return [sessionDeferred, deferred];
    };

    var authDenied = function () {
      app.masterLayout.setView('#dashboard', new Auth.NoAccessView());
      app.masterLayout.renderView('#dashboard');
    };

    FauxtonAPI.auth.registerAuth(auth);
    FauxtonAPI.auth.registerAuthDenied(authDenied);
  };

  return Auth;
});
