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
  "modules/fauxton/base",
  // Layout
  "modules/fauxton/layout",

  // Routes return the module that they define routes for
  "modules/databases/base",
  "modules/documents/base",
  "modules/pouchdb/base",


  // this needs to be added as a plugin later
  // "modules/logs/base",
  // "modules/config/base",

  "load_addons"
],

function(req, app, Initialize, FauxtonAPI, Fauxton, Layout, Databases, Documents, Pouch, LoadAddons) {

  var defaultLayout = 'with_sidebar';
  // TODO: auto generate this list if possible
  var modules = [Databases, Documents];

  var generateRoute = function(settingsGenerator, route) {
    return function() {
      var boundRoute = route;
      var settings = settingsGenerator.apply(null, arguments);
      var layout = settings.layout || defaultLayout;
      var establish = settings.establish || function() { return null; };
      var masterLayout = this.masterLayout;

      console.log("Settings generator for: ", layout, settings);

      masterLayout.setTemplate(layout);
      masterLayout.clearBreadcrumbs();

      if (settings.crumbs) {
        masterLayout.setBreadcrumbs(new Fauxton.Breadcrumbs({
          crumbs: settings.crumbs
        }));
      }

      $.when.apply(null, establish()).done(function(resp) {
        _.each(settings.views, function(view, selector) {
          masterLayout.setView(selector, view);

          $.when.apply(null, view.establish()).then(function(resp) {
            masterLayout.renderView(selector);
          }, function(resp) {
            view.establishError = {
              error: true,
              reason: resp
            };
            masterLayout.renderView(selector);
          });

          var hooks = masterLayout.hooks[selector];

          if(hooks){
            _.each(hooks, function(hook){
              if (_.any(hook.routes, function(route){return route == boundRoute;})){
                hook.callback(view);
              }
            });
          }
        });
      });

      if (settings.apiUrl) this.apiBar.update(settings.apiUrl);
    };
  };

  var Router = app.router = Backbone.Router.extend({
    routes: {},

    addModuleRouteObject: function(RouteObject) {
      var self = this;
      var masterLayout = this.masterLayout;

      _.each(RouteObject.prototype.get('routes'), function(route) {
        this.route(route, route.toString(), function() {
          var args = Array.prototype.slice.call(arguments);

          if (self.activeRouteObject && self.activeRouteObject.get('routes').indexOf(route) > -1) {
            //Don't need to do anything here as this route has been initialised
            self.activeRouteObject.route.call(self.activeRouteObject, route, args);
            console.log('Avoiding Route creation');
            return;
          }

          self.activeRouteObject = new RouteObject(args);
          self.activeRouteObject[self.activeRouteObject.defaultRoute].apply(self.activeRouteObject, args);
          self.activeRouteObject.render(route, masterLayout, args);
        });
      }, this);
    },

    setModuleRoutes: function() {
      _.each(modules, function(module) {
        if (module){
          _.each(module.RouteObjects, this.addModuleRouteObject, this);
        }
      }, this);
      _.each(LoadAddons.addons, function(module) {
        if (module){
          module.initialize();
          // This is pure routes the addon provides
          if (module.Routes) {
            _.each(module.RouteObjects, this.addModuleRouteObject, this);
          }
        }
      }, this);
    },

    setAddonHooks: function() {
      _.each(LoadAddons.addons, function(module) {
        // This is updates to views by the addon
        if (module && module.hooks){
          _.each(module.hooks, function(callback, route){
            if (this.masterLayout.hooks[route]) {
              this.masterLayout.hooks[route].push(callback);
            } else {
              this.masterLayout.hooks[route] = [callback];
            }
          }, this);
        }
      }, this);
    },

    initialize: function() {
      //TODO: It would be nice to handle this with a router
      this.navBar = app.navBar = new Fauxton.NavBar();
      this.apiBar = app.apiBar = new Fauxton.ApiBar();

      app.masterLayout = this.masterLayout = new Layout(this.navBar, this.apiBar);
      app.footer = new Fauxton.Footer({el: "#footer-content"});

      // NOTE: This must be below creation of the layout
      // FauxtonAPI header links and others depend on existence of the layout
      this.setAddonHooks();
      this.setModuleRoutes();

      $("#app-container").html(this.masterLayout.el);
      this.masterLayout.render();

      app.footer.render();
    },

    triggerRouteEvent: function(event, args) {
      if (this.activeRouteObject) {
        var eventArgs = [event].concat(args);
        console.log("CALLING ROUTE EVENT ON", this.activeRouteObject, arguments);
        this.activeRouteObject.trigger.apply(this.activeRouteObject, eventArgs );
        this.activeRouteObject.renderWith(eventArgs, this.masterLayout, args);
      }
    }
  });

  return Router;

});
