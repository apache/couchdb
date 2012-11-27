define([
  // Application.
  "app",

  // Initialize application
  "initialize",

  // Load Fauxton API
  "fauxton_api",

  // Modules
  "modules/fauxton",
  "modules/dashboard",
  // Routes return the module that they define routes for
  "modules/databases/routes",
  "modules/documents/routes",

  "modules/api",

  // this needs to be added as a plugin later
  "modules/log",
  "modules/config"
],

function(app, Initialize, FauxtonAPI, Fauxton, Dashboard, Databases, Documents, API, Log, Config) {

  var defaultTemplate = 'with_sidebar';
  // TODO: auto generate this list if possible
  var modules = [Databases, Documents];

  var generateRoute = function(settingsGenerator) {
    return function() {
      var settings = settingsGenerator.apply(null, arguments);
      var templateName = settings.template || defaultTemplate;
      var establish = settings.establish || function() { return null; };
      var dashboard = this.dashboard;

      dashboard.setTemplate(templateName);
      dashboard.clearBreadcrumbs();

      if (settings.crumbs) {
        dashboard.setBreadcrumbs(new Fauxton.Breadcrumbs({
          crumbs: settings.crumbs
        }));
      }

      $.when.apply(null, establish()).done(function(resp) {
        _.each(settings.views, function(view, selector) {
          dashboard.setView(selector, view);

          $.when(null, view.establish()).done(function(resp) {
            // HACK: need to find a permanent solution to this
            setTimeout(function() {
              dashboard.renderView(selector);
            }, 0);
          });
        });
      });

      if (settings.apiUrl) this.apiBar.update(settings.apiUrl);
    };
  };

  var Router = app.router = Backbone.Router.extend({
    routes: {
      "_log": "log",
      "_config": "config"
    },

    // These moduleRoutes functions are aguably better outside but
    // need access to the Router instance which is not created in this
    // module
    addModuleRoute: function(generator, route) {
      //var name = settings.name || route;
       this.route(route, route.toString(), generateRoute(generator));
    },

    setModuleRoutes: function() {
      var addModuleRoute = this.addModuleRoute;
      _.each(modules, function(module) {
        _.each(module.Routes, addModuleRoute, this);
      }, this);
    },

    initialize: function() {
      this.setModuleRoutes();

      this.navBar = app.navBar = new Fauxton.NavBar();
      this.apiBar = app.apiBar = new Fauxton.ApiBar();

      app.dashboard = this.dashboard = new Dashboard(this.navBar, this.apiBar);

      $("#app-container").html(this.dashboard.el);
      this.dashboard.render();
    },

    log: function() {
      var dashboard = this.dashboard;
      dashboard.setTemplate('with_sidebar');

      var logs = new Log.Collection();

      var crumbs = [
        {"name": "Dashboard", "link": app.root},
        {"name": "Logs","link": app.root}
      ];

      dashboard.setBreadcrumbs(new Fauxton.Breadcrumbs({
        crumbs: crumbs
      }));

      dashboard.setDashboardContent(new Log.View({
        collection: logs
      }));

      dashboard.setSidebarContent(new Log.FilterView({}));

      logs.fetch().done(function (resp) {
        dashboard.dashboardContent.render();
      });

      this.apiBar.update(logs.url());
    },

    config: function () {
      var dashboard = this.dashboard;
      dashboard.setTemplate('one_pane');

      var configs = new Config.Collection();

      var crumbs = [
        {"name": "Home","link": app.root},
        {"name": "Config","link": app.root}
      ];

      this.dashboard.setDashboardContent(new Config.View({
        collection: configs
      }));

      this.dashboard.setBreadcrumbs(new Fauxton.Breadcrumbs({
        crumbs: crumbs
      }));

      configs.fetch().done(function (resp) {
        dashboard.render();
      });

      this.apiBar.update(configs.url());
    }
  });

  return Router;

});
