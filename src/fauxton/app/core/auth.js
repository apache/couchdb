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
  "core/base",
  "backbone"
],
function(FauxtonAPI, Backbone) {

  // This is not exposed externally as it should not need to be accessed or overridden
  var Auth = function (options) {
    this._options = options;
    this.initialize.apply(this, arguments);
  };

  // Piggy-back on Backbone's self-propagating extend function,
  Auth.extend = Backbone.Model.extend;

  _.extend(Auth.prototype, Backbone.Events, {
    authDeniedCb: function() {},

    initialize: function() {
      var that = this;
    },

    authHandlerCb : function (roles) {
      var deferred = $.Deferred();
      deferred.resolve();
      return deferred;
    },

    registerAuth: function (authHandlerCb) {
      this.authHandlerCb = authHandlerCb;
    },

    registerAuthDenied: function (authDeniedCb) {
      this.authDeniedCb = authDeniedCb;
    },

    checkAccess: function (roles) {
      var requiredRoles = roles || [],
      that = this;

      if (!FauxtonAPI.session) {
        throw new Error("Fauxton.session is not configured.");
      }

      return FauxtonAPI.session.fetchUser().then(function (user) {
        return FauxtonAPI.when(that.authHandlerCb(FauxtonAPI.session, requiredRoles));
      });
    }
  });

//  FauxtonAPI.auth = new Auth();

  return Auth;
});

