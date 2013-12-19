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
       // Load require for use in nested requiring
       // as per the note in: http://requirejs.org/docs/api.html#multiversion
       "require",

       // Application.
       "app",

       // Initialize application
       "initialize",

       // Load Fauxton API
       "api",

       // Modules
       "addons/fauxton/base",
       // Layout
       "addons/fauxton/layout",

       "load_addons"
],

function(req, app, Initialize, FauxtonAPI, Fauxton, Layout, LoadAddons) {

  var beforeUnloads = {};

  var Router = app.router = Backbone.Router.extend({
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
      var masterLayout = this.masterLayout,
      routeUrls = RouteObject.prototype.getRouteUrls();

      _.each(routeUrls, function(route) {
        this.route(route, route.toString(), function() {
          var args = Array.prototype.slice.call(arguments),
          roles = RouteObject.prototype.getRouteRoles(route),
          authPromise = app.auth.checkAccess(roles);

          authPromise.then(function () {
            if (!that.activeRouteObject || !that.activeRouteObject.hasRoute(route)) {
              if (that.activeRouteObject) {
                that.activeRouteObject.cleanup();
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
      this.navBar = app.navBar = new Fauxton.NavBar();
      this.apiBar = app.apiBar = new Fauxton.ApiBar();
      this.auth = app.auth = FauxtonAPI.auth;
      app.session = FauxtonAPI.session;

      app.masterLayout = this.masterLayout = new Layout(this.navBar, this.apiBar);
      app.footer = new Fauxton.Footer({el: "#footer-content"});

      // NOTE: This must be below creation of the layout
      // FauxtonAPI header links and others depend on existence of the layout
      //this.setAddonHooks();
      this.setModuleRoutes();

      $("#app-container").html(this.masterLayout.el);
      this.masterLayout.render();

      // TODO: move this to a proper Fauxton.View
      $.when.apply(null, app.footer.establish()).done(function() {
        app.footer.render();
      });
    },

    triggerRouteEvent: function(event, args) {
      if (this.activeRouteObject) {
        var eventArgs = [event].concat(args);
        this.activeRouteObject.trigger.apply(this.activeRouteObject, eventArgs );
        this.activeRouteObject.renderWith(eventArgs, this.masterLayout, args);
      }
    }
  });

  return Router;

});
