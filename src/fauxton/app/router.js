define([
  // Application.
  "app",

  // Initialize application
  "initialize",

  // Modules
  "modules/fauxton",
  "modules/dashboard",
  "modules/databases",
  "modules/documents",
  "modules/api",
  "modules/fauxton_plugin",

  // this needs to be added as a plugin later
  "modules/log",
  "modules/config"
],

function(app, Initialize, Fauxton, Dashboard, Databases, Documents, API, Plugin, Log, Config) {

  // Defining the application router, you can attach sub routers here.
  var Router = app.router = Backbone.Router.extend({
    routes: {
      "": "index",
      "index.html": "index",
      "_all_dbs": "index",
      "database/:database/_:handler": "database_handler",
      "database/:database/:doc": "database_doc",
      "_log": "log",
      "_config": "config"
    },

    initialize: function() {
      this.navBar = app.navBar = new Fauxton.NavBar();
      this.apiBar = app.apiBar = new Fauxton.ApiBar();

      app.dashboard = this.dashboard = new Dashboard(this.navBar, this.apiBar);

      $("#app-container").html(this.dashboard.el);
      this.dashboard.render();
    },

    database_doc: function(databaseName, docID) {
      var dashboard = this.dashboard;
      var database = new Databases.Model({id:databaseName});
      var doc = new Documents.Doc({
        "_id": docID
      });
      doc.collection = database;

      var crumbs = [
        {"name": "Dashboard", "link": app.root},
        {"name": "Databases", "link": app.root},
        {"name": database.id, "link": Databases.databaseUrl(database)},
        {"name": docID, "link": "#"}
      ];

      dashboard.setBreadcrumbs(new Fauxton.Breadcrumbs({
        crumbs: crumbs
      }));

      dashboard.setDashboardContent(new Documents.Views.Doc({
        model: doc
      }));

      dashboard.setSidebarContent(new Documents.Views.Sidebar({
        collection: database
      }));

      doc.fetch().done(function(resp) {
        // Render only the part of the dashboard that needs to be re-rendered
        dashboard.dashboardContent.render();
      });

      this.apiBar.update(doc.url());
    },

    database_handler: function(databaseName, page) {
      var dashboard = this.dashboard;
      var database = new Databases.Model({id:databaseName});

      var designDocs = new Documents.AllDocs({
        database: database,
        params: {startkey: '"_design"',
                 endkey: '"_design1"',
                 include_docs: true}
      });

      var options = app.getParams();
      database.buildAllDocs(options);

      var crumbs = [
        {"name": "Dashboard", "link": app.root},
        {"name": "Databases", "link": app.root},
        {"name": database.id, "link": Databases.databaseUrl(database)}
      ];

      dashboard.setBreadcrumbs(new Fauxton.Breadcrumbs({
        crumbs: crumbs
      }));

      dashboard.setDashboardContent(new Documents.Views.AllDocsList({
        model: database
      }));

      dashboard.setSidebarContent(new Documents.Views.Sidebar({
        collection: designDocs
      }));

      database.allDocs.fetch().done(function(resp) {
        dashboard.dashboardContent.render();
      });

      designDocs.fetch().done(function(resp) {
        dashboard.sidebarContent.render();
      });

      this.apiBar.update(database.allDocs.url());
    },

    log: function() {
      var dashboard = this.dashboard;
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
      var dashboard = this.dashboard,
          configs = new Config.Collection();

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
    },

    // TODO: This should be renamed when we have a real dashboar
    index: function() {
      var dashboard = this.dashboard;
      var databases = app.databases = new Databases.List();

      this.dashboard.clearBreadcrumbs();

      var crumbs = [
        {"name": "Dashboard", "link": app.root}
      ];

      dashboard.setBreadcrumbs(new Fauxton.Breadcrumbs({
        crumbs: crumbs
      }));

      dashboard.setDashboardContent(new Databases.Views.List({
        collection: databases
      }));

      dashboard.setSidebarContent(new Databases.Views.Sidebar({
        collection: databases
      }));

      databases.fetch().done(function(resp) {
        $.when.apply(null, databases.map(function(database) {
          return database.status.fetch();
        })).done(function(resp) {
          dashboard.dashboardContent.render();
        });
      });

      this.apiBar.update(databases.url());
    }
  });

  return Router;

});
