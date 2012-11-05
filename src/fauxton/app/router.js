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
    },

    database_doc: function(databaseName, docID) {
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

      var dashboard = new Backbone.Layout({
        template: "dashboard",

        views: {
          "#dashboard-content": new Databases.Views.Doc({
            model: doc
          }),

          "#breadcrumbs": new Fauxton.Breadcrumbs({
            crumbs: crumbs
          }),

          "#primary-navbar": this.navBar
        }
      });

      $("#app-container").html(dashboard.$el);

      doc.fetch().done(function(resp) {
        dashboard.render();
      });
    },

    database_handler: function(databaseName, page) {
      //var database = app.databases.get(databaseName);
      var database = new Databases.Model({id:databaseName});
      var options = app.getParams();
      database.buildAllDocs(options);

      var crumbs = [
        {"name": "Home","link": app.root},
        {"name": database.id, "link": Databases.databaseUrl(database)}
      ];

      var dashboard = new Backbone.Layout({
        template: "dashboard",

        views: {
          "#dashboard-content": new Databases.Views.AllDocsList({
            model: database
          }),

          "#breadcrumbs": new Fauxton.Breadcrumbs({
            crumbs: crumbs
          }),

          "#primary-navbar": this.navBar
        }
      });

      $("#app-container").html(dashboard.$el);

      database.allDocs.fetch().done(function(resp) {
        dashboard.render();
      });
    },

    log: function() {
      var logs = new Log.Collection();

      var crumbs = [
        {"name": "Home","link": app.root},
        {"name": "Logs","link": app.root}
      ];

      var dashboard = new Backbone.Layout({
        template: "dashboard",

        views: {
          "#dashboard-content": new Log.View({
            collection: logs
          }),

          "#breadcrumbs": new Fauxton.Breadcrumbs({
            crumbs: crumbs
          }),

          "#primary-navbar": this.navBar
        }
      });


      $("#app-container").html(dashboard.$el);

      logs.fetch().done(function (resp) {
        dashboard.render();
      });

    },

    index: function() {
      console.log('index router.js ftw');
      var databases = app.databases = new Databases.List();

      var dashboard = new Backbone.Layout({
        template: "dashboard",

        views: {
          "#dashboard-content": new Databases.Views.List({
            collection: databases
          }),

          "#primary-navbar": this.navBar
        }
      });

      window.dashboard = dashboard;

      $("#app-container").html(dashboard.$el);

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
