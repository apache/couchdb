define([
  "app",

  // Libs
  "backbone",

  // Views
  "modules/databases/views"

  // Plugins
],

function(app, Backbone, Views) {
  var Databases = app.module();

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
      this.allDocs = new Databases.AllDocs({
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

  Databases.Doc = Backbone.Model.extend({
    idAttribute: "_id",

    url: function() {
      return app.host + "/" + this.collection.id + "/" + this.id;
    }
  });

  Databases.AllDocs = Backbone.Collection.extend({
    model: Databases.Doc,

    initialize: function(options) {
      this.database = options.database;
      this.params = options.params;
    },

    url: function() {
      var query = "";
      if (this.params) {
        query = "?" + $.param(this.params);
      }
      return app.host + "/" + this.database.id + "/_all_docs" + query;
    },

    parse: function(resp) {
      return _.map(resp.rows, function(row) {
        return {
          "_id": row.id,
          rev: row.value.rev,
          value: row.value,
          key: row.key
        };
      });
    }
  });



  Databases.Views = Views;

  return Databases;
});
