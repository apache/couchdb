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

  "api"
],

function(app, FauxtonAPI) {
  // This is an example module of using the new auth module.

  var noAccessView = FauxtonAPI.View.extend({
    template: "addons/exampleAuth/templates/noAccess"

  });

  // To utilise the authentication - all that is required, is one callback
  // that is registered with the auth api. This function can return an array
  // of deferred objects.
  // The roles argument that is passed in is the required roles for the current user
  // to be allowed to access the current page.
  // The layout is the main layout for use when you want to render a view onto the page
  var auth = function (roles) {
    var deferred = $.Deferred();

    if (roles.indexOf('_admin') > -1) {
      deferred.reject();
    } else {
      deferred.resolve();
    }

    return [deferred];
  };

  // If you would like to do something with when access is denied you can register this callback.
  // It will be called is access has been denied on the previous page.
  var authFail = function () {
    app.masterLayout.setView('#dashboard', new noAccessView());
    app.masterLayout.renderView('#dashboard');
  };

  // Register the auth call back. This will be called before new route rendered
  FauxtonAPI.auth.registerAuth(auth);
  // Register a failed route request callback. This is called if access is denied.
  FauxtonAPI.auth.registerAuthDenied(authFail);



});
