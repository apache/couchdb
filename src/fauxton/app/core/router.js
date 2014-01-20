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
       "core/auth",
       "backbone"
],

function(FauxtonAPI, Auth, Backbone) {

  var beforeUnloads = {};

  var Router = Backbone.Router.extend({
    routes: {},

    beforeUnload: function (name, fn) {
      beforeUnloads[name] = fn;
    },

    removeBeforeUnload: function (name) {
      delete beforeUnloads[name];
    },

    navigate: function (fragment, trigger) {
      var continueNav  = true,
          msg = _.find(_.map(beforeUnloads, function (fn) { return fn(); }), function (beforeReturn) {
            if (beforeReturn) { return true; }
          });

      if (msg) {
        continueNav = window.confirm(msg);
      }

      if (continueNav) {
        Backbone.Router.prototype.navigate(fragment, trigger);
      }
    },

    addModuleRouteObject: function(RouteObject) {
      var that = this;
      var masterLayout = FauxtonAPI.masterLayout,
      routeUrls = RouteObject.prototype.getRouteUrls();

      _.each(routeUrls, function(route) {
        this.route(route, route.toString(), function() {
          var args = Array.prototype.slice.call(arguments),
          roles = RouteObject.prototype.getRouteRoles(route),
          authPromise = FauxtonAPI.auth.checkAccess(roles);

          authPromise.then(function () {
            if (!that.activeRouteObject || !that.activeRouteObject.hasRoute(route)) {
              if (that.activeRouteObject) {
                that.activeRouteObject.removeViews();
              }
              that.activeRouteObject = new RouteObject(route, masterLayout, args);
            }

            var routeObject = that.activeRouteObject;
            routeObject.routeCallback(route, args);
            routeObject.renderWith(route, masterLayout, args);
          }, function () {
            FauxtonAPI.auth.authDeniedCb();
          });

        }); 
      }, this);
    },

    setModuleRoutes: function(addons) {
      _.each(addons, function(module) {
        if (module){
          module.initialize();
          // This is pure routes the addon provides
          if (module.RouteObjects) {
            _.each(module.RouteObjects, this.addModuleRouteObject, this);
          }
        }
      }, this);
    },

    initialize: function(addons) {
      this.addons = addons;
      this.auth = FauxtonAPI.auth = new Auth();
      // NOTE: This must be below creation of the layout
      // FauxtonAPI header links and others depend on existence of the layout
      this.setModuleRoutes(addons);

      $(FauxtonAPI.el).html(FauxtonAPI.masterLayout.el);
      FauxtonAPI.masterLayout.render();
    },

    triggerRouteEvent: function(event, args) {
      if (this.activeRouteObject) {
        var eventArgs = [event].concat(args);
        this.activeRouteObject.trigger.apply(this.activeRouteObject, eventArgs );
        this.activeRouteObject.renderWith(eventArgs, FauxtonAPI.masterLayout, args);
      }
    }
  });

  return Router;
});

