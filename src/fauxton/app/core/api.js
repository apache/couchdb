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
       "core/resources",
       // Modules
],

function(FauxtonAPI, Fauxton) {
    
  FauxtonAPI.navigate = function(url, _opts) {
    var options = _.extend({trigger: true}, _opts );
    FauxtonAPI.router.navigate(url,options);
  };

  FauxtonAPI.beforeUnload = function () {
    FauxtonAPI.router.beforeUnload.apply(FauxtonAPI.router, arguments);
  };

  FauxtonAPI.removeBeforeUnload = function () {
    FauxtonAPI.router.removeBeforeUnload.apply(FauxtonAPI.router, arguments);
  };

  FauxtonAPI.addHeaderLink = function(link) {
    FauxtonAPI.masterLayout.navBar.addLink(link);
  };

  FauxtonAPI.removeHeaderLink = function(link) {
    FauxtonAPI.masterLayout.navBar.removeLink(link);
  };

  FauxtonAPI.addRoute = function(route) {
    FauxtonAPI.router.route(route.route, route.name, route.callback);
  };

  FauxtonAPI.triggerRouteEvent = function (routeEvent, args) {
    FauxtonAPI.router.triggerRouteEvent("route:"+routeEvent, args);
  };

  var beforeUnloads = {};

  FauxtonAPI.Router = Backbone.Router.extend({
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
          authPromise = app.auth.checkAccess(roles);

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

    setModuleRoutes: function() {
      _.each(LoadAddons.addons, function(module) {
        if (module){
          module.initialize();
          // This is pure routes the addon provides
          if (module.RouteObjects) {
            _.each(module.RouteObjects, this.addModuleRouteObject, this);
          }
        }
      }, this);
    },

    initialize: function() {
      //TODO: It would be nice to handle this with a router
      this.auth = app.auth = FauxtonAPI.auth;
      // NOTE: This must be below creation of the layout
      // FauxtonAPI header links and others depend on existence of the layout
      this.setModuleRoutes();

      $("#app-container").html(FauxtonAPI.masterLayout.el);
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


  return FauxtonAPI;
});

