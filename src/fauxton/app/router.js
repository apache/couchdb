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


  // this needs to be added as a plugin later
  // "modules/logs/base",
  // "modules/config/base",

  "load_addons"
],

function(req, app, Initialize, FauxtonAPI, Fauxton, Layout, Databases, Documents, LoadAddons) {

  var defaultLayout = 'with_sidebar';
  // TODO: auto generate this list if possible
  var modules = [Databases, Documents];

  var generateRoute = function(settingsGenerator) {
    return function() {
      var settings = settingsGenerator.apply(null, arguments);
      var layoutName = settings.layout || defaultLayout;
      var establish = settings.establish || function() { return null; };
      var masterLayout = this.masterLayout;

      console.log("Settings generator for: "+layoutName, settings);

      masterLayout.setTemplate(layoutName);
      masterLayout.clearBreadcrumbs();

      if (settings.crumbs) {
        masterLayout.setBreadcrumbs(new Fauxton.Breadcrumbs({
          crumbs: settings.crumbs
        }));
      }

      $.when.apply(null, establish()).done(function(resp) {
        _.each(settings.views, function(view, selector) {
          masterLayout.setView(selector, view);

          $.when.apply(null, view.establish()).done(function(resp) {
            masterLayout.renderView(selector);
          });
        });
      });

      if (settings.apiUrl) this.apiBar.update(settings.apiUrl);
    };
  };

  var Router = app.router = Backbone.Router.extend({
    routes: {
    },

    // These moduleRoutes functions are aguably better outside but
    // need access to the Router instance which is not created in this
    // module
    addModuleRoute: function(generator, route) {
      this.route(route, route.toString(), generateRoute(generator));
    },

    setModuleRoutes: function() {
      var addModuleRoute = this.addModuleRoute;
      var that = this;

      _.each(modules, function(module) {
        if (module){
          _.each(module.Routes, addModuleRoute, this);
        }
      }, this);

      req(LoadAddons.addons, function() {
        var modules = arguments;
        _.each(modules, function(module) {
          module.initialize();
          if (module.Routes) {
            _.each(module.Routes, addModuleRoute, that);
          }
        });
      });
    },

    initialize: function() {
      this.setModuleRoutes();

      this.navBar = app.navBar = new Fauxton.NavBar();
      this.apiBar = app.apiBar = new Fauxton.ApiBar();

      app.masterLayout = this.masterLayout = new Layout(this.navBar, this.apiBar);

      $("#app-container").html(this.masterLayout.el);
      this.masterLayout.render();
    },

    log: function() {
      var masterLayout = this.masterLayout;
      masterLayout.setTemplate('with_sidebar');

      var logs = new Log.Collection();

      var crumbs = [
        {"name": "Dashboard", "link": app.root},
        {"name": "Logs","link": app.root}
      ];

      masterLayout.setBreadcrumbs(new Fauxton.Breadcrumbs({
        crumbs: crumbs
      }));

      masterLayout.setContent(new Log.View({
        collection: logs
      }));

      masterLayout.setSidebarContent(new Log.FilterView({}));

      logs.fetch().done(function (resp) {
        masterLayout.content.render();
      });

      this.apiBar.update(logs.url());
    },

    config: function () {
      var masterLayout = this.masterLayout;
      masterLayout.setTemplate('one_pane');

      var configs = new Config.Collection();

      var crumbs = [
        {"name": "Home","link": app.root},
        {"name": "Config","link": app.root}
      ];

      this.masterLayout.setContent(new Config.View({
        collection: configs
      }));

      this.masterLayout.setBreadcrumbs(new Fauxton.Breadcrumbs({
        crumbs: crumbs
      }));

      configs.fetch().done(function (resp) {
        masterLayout.render();
      });

      this.apiBar.update(configs.url());
    }
  });

  return Router;

});
