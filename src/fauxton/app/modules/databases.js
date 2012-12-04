define([
  "app",

  "fauxton_api",

  // Modules
  "modules/documents/models_collections",

  // Views
  "modules/databases/views"

  // Plugins
],

function(app, FauxtonAPI, Documents, Views, Routes) {
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

    url: function(context) {
      if (context === "index") {
        return "/database/" + this.id + "/_all_docs";
      } else if (context === "app") {
        return "/database/" + this.id;
      } else {
        return app.host + "/" + this.id;
      }
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

  return Databases;
});
