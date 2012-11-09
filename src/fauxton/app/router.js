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
      this.setDashboardDom();
    },

    setDashboardContent: function(view) {
      if (this.dashboardContent) this.dashboardContent.remove();

      this.dashboardContent = this.dashboard.insertView("#dashboard-content", view);
      this.setDashboardDom();
    },

    setBreadcrumbs: function(view) {
      if (this.breadcrumbs) this.breadcrumbs.remove();

      this.breadcrumbs = this.dashboard.insertView("#breadcrumbs", view);
      this.setDashboardDom();
    },

    // TODO:: this function seems hacky
    // Should layout manager auto update the html node? do we need to
    // specify the destination element in the layout?
    // Re-setting the navbar seems wrong, isn't the whole point of
    // insertView as opposed to setView to keep the existing reference
    // nodes in place? Unfortunately insertView won't persist through
    // new pages. Need to look into this more
    setDashboardDom: function() {
      this.dashboard.setView("#primary-navbar", this.navBar);
      $("#app-container").html(this.dashboard.$el);
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
