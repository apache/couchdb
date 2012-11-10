define([
  // Application.
  "app",

  // Initialize application
  "initialize",

  // Modules
  "modules/fauxton",
  "modules/databases",
  "modules/api",
  "modules/fauxton_plugin",

  // this needs to be added as a plugin later
  "modules/log"
],

function(app, Initialize, Fauxton, Databases, API, Plugin, Log) {

  // Defining the application router, you can attach sub routers here.
  var Router = app.router = Backbone.Router.extend({
    routes: {
      "": "index",
      "index.html": "index",
      "_all_dbs": "index",
      "database/:database/_:handler": "database_handler",
      "database/:database/:doc": "database_doc",
      "_log": "log"
    },

    initialize: function() {
      this.navBar = app.navBar = new Fauxton.NavBar();

      window.dashboard = this.dashboard = new Backbone.Layout({
        template: "layouts/dashboard"
      });
      this.initializeDashboardDom();
    },

    // I'm not sure if this method is required. We can just set the dashboard in the router method.
    // I also dont think we need to keep the dashboardContent any more either.
    setDashboardContent: function(view) {
      this.dashboardContent = this.dashboard.setView("#dashboard-content", view).render();
    },

    setBreadcrumbs: function(view) {
      //if (this.breadcrumbs) this.breadcrumbs.remove();

      this.breadcrumbs = this.dashboard.setView("#breadcrumbs", view).render();
      //this.setDashboardDom();
    },

    // TODO:: this function seems hacky
    // Garren - FIXED!!!
    initializeDashboardDom: function() {
      $("#app-container").html(this.dashboard.el);
      this.dashboard.setView("#primary-navbar", this.navBar);
      // this should be called once and the rest of the time only update the views inside the manager that require refreshing
      this.dashboard.render();
    },

    database_doc: function(databaseName, docID) {
      var dashboard = this.dashboard;
      var database = new Databases.Model({id:databaseName});
      var doc = new Databases.Doc({
        "_id": docID
      });
      doc.collection = database;

      var crumbs = [
        {"name": "Home","link": app.root},
        {"name": database.id, "link": Databases.databaseUrl(database)},
        {"name": docID, "link": "#"}
      ];

      this.setDashboardContent(new Databases.Views.Doc({
        model: doc
      }));
      this.setBreadcrumbs(new Fauxton.Breadcrumbs({
        crumbs: crumbs
      }));

      doc.fetch().done(function(resp) {
        // Instead of re-rendering the whole dashboard, we should rather 
        // wire up the view to the model.on('change') and render just the
        // required view. This should stop the whole page from flickering
        dashboard.render();
      });
    },

    database_handler: function(databaseName, page) {
      var dashbaard = this.dashboard;
      var database = new Databases.Model({id:databaseName});
      var options = app.getParams();
      database.buildAllDocs(options);

      var crumbs = [
        {"name": "Home","link": app.root},
        {"name": database.id, "link": Databases.databaseUrl(database)}
      ];

      this.setDashboardContent(new Databases.Views.AllDocsList({
        model: database
      }));
      this.setBreadcrumbs(new Fauxton.Breadcrumbs({
        crumbs: crumbs
      }));

      database.allDocs.fetch().done(function(resp) {
        dashboard.render();
      });
    },

    log: function() {
      var dashboard = this.dashboard;
      var logs = new Log.Collection();
      var crumbs = [
        {"name": "Home","link": app.root},
        {"name": "Logs","link": app.root}
      ];

      this.setDashboardContent(new Log.View({
        collection: logs
      }));
      this.setBreadcrumbs(new Fauxton.Breadcrumbs({
        crumbs: crumbs
      }));

      logs.fetch().done(function (resp) {
        dashboard.render();
      });
    },

    index: function() {
      var dashbard = this.dashboard;
      var databases = app.databases = new Databases.List();

      this.setDashboardContent(new Databases.Views.List({
        collection: databases
      }));

      databases.fetch().done(function(resp) {
        $.when.apply(null, databases.map(function(database) {
          return database.status.fetch();
        })).done(function(resp) {
          dashboard.render();
        });
      });
    }
  });

  return Router;

});
