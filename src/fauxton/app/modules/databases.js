define([
  "app",

  "fauxton_api",

  // Modules
  "modules/documents",

  // Views
  "modules/databases/views"

  // Plugins
],

function(app, FauxtonAPI, Documents, Views) {
  var Databases = FauxtonAPI.module();

  // Utility functions
  Databases.databaseUrl = function(database) {
    var name = _.isObject(database) ? database.id : database;

    return ["/database/", name, "/_all_docs?limit=10"].join('');
  };

  Databases.Model = Backbone.Model.extend({
    initialize: function(options) {
      this.status = new Databases.Status({
        database: this
      });
    },

    buildAllDocs: function(params) {
      this.allDocs = new Documents.AllDocs(null, {
        database: this,
        params: params
      });

      return this.allDocs;
    },

    isNew: function(){
      // Databases are never new, to make Backbone do a PUT
      return false;
    },

    url: function() {
      return app.host + "/" + this.id;
    },

    pageUrl: function() {
      return "/database/" + this.id;
    }
  });

  Databases.Status = Backbone.Model.extend({
    url: function() {
      return app.host + "/" + this.database.id;
    },

    initialize: function(options) {
      this.database = options.database;
    }
  });

  // TODO: shared databases - read from the user doc
  Databases.List = Backbone.Collection.extend({
    model: Databases.Model,

    url: function() {
      return app.host + "/_all_dbs";
    },

    parse: function(resp) {
      // TODO: pagination!
      return _.map(_.first(resp, 10), function(database) {
        return {
          id: encodeURIComponent(database),
          name: database
        };
      });
    }
  });

  Databases.Views = Views;

  var allDbsCallback = function() {
    var data = {
      databases: new Databases.List()
    };
    var deferred = FauxtonAPI.Deferred();

    return {
      layout: "with_sidebar",

      data: data,

      crumbs: [
        {"name": "Dashboard", "link": app.root}
      ],

      views: {
        "#dashboard-content": new Databases.Views.List({
          collection: data.databases
        }),

        "#sidebar-content": new Databases.Views.Sidebar({
          collection: data.databases
        })
      },

      apiUrl: data.databases.url(),

      establish: function() {
        data.databases.fetch().done(function(resp) {
          $.when.apply(null, data.databases.map(function(database) {
            return database.status.fetch();
          })).done(function(resp) {
            deferred.resolve();
          });
        });
        return [deferred];
      }
    };
  };

  Databases.Routes = {
    "": allDbsCallback,
    "index.html": allDbsCallback,
    "_all_dbs": allDbsCallback,

    // HACK
    // The ordering of routes is different in this object that the
    // routes object in the Backbone.Router. As a result, the
    // declaration order of show doc and _handler methods has been
    // switched. This is a brittle solution that needs to be fixed.
    // Conflicts with route: "database/:database/_:handler"
    //
    // TODO: add support for regex based rotues
    // Javascript does not handle a regex as an object key very well,
    // and it turns it into its string representation when you use in
    // non object literal form, which does get recast back as a regex
    // when we need it.
    // The inability to use regex based routes here is a design flaw
    // and should be rectified.
    "database/:database/:doc": function(databaseName, docID) {
      var data = {
        database: new Databases.Model({id:databaseName}),
        doc: new Documents.Doc({
          "_id": docID
        })
      };
      data.doc.collection = data.database;
      data.designDocs = new Documents.AllDocs(null, {
        database: data.database,
        params: {startkey: '"_design"',
                 endkey: '"_design1"',
                 include_docs: true}
      });

      var options = app.getParams();
      options.include_docs = true;
      data.database.buildAllDocs(options);

      return {
        layout: "with_sidebar",

        data: data,

        crumbs: [
          {"name": "Dashboard", "link": app.root},
          {"name": "Databases", "link": app.root},
          {"name": data.database.id, "link": Databases.databaseUrl(data.database)},
          {"name": docID, "link": "#"}
        ],

        views: {
          "#dashboard-content": new Documents.Views.Doc({
            model: data.doc
          }),

          "#sidebar-content": new Documents.Views.Sidebar({
            collection: data.designDocs
          })
        },

        apiUrl: data.doc.url()
      };
    },

    "database/:database/_:handler": function(databaseName, page) {
      var data = {
        database: new Databases.Model({id:databaseName})
      };
      data.designDocs = new Documents.AllDocs(null, {
        database: data.database,
        params: {startkey: '"_design"',
                 endkey: '"_design1"',
                 include_docs: true}
      });

      var options = app.getParams();
      options.include_docs = true;
      data.database.buildAllDocs(options);

      return {
        layout: "with_tabs_sidebar",

        data: data,

        crumbs: [
          {"name": "Dashboard", "link": app.root},
          {"name": "Databases", "link": app.root},
          {"name": data.database.id, "link": Databases.databaseUrl(data.database)}
        ],

        views: {
          "#dashboard-content": new Documents.Views.AllDocsList({
            model: data.database
          }),

          "#sidebar-content": new Documents.Views.Sidebar({
            collection: data.designDocs
          }),

          "#tabs": new Documents.Views.Tabs({})
        },

        apiUrl: data.database.allDocs.url()
      };
    }
  };

  return Databases;
});
