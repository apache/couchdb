define([
  "app",

  // Libs
  "backbone",

  // Views
  "modules/documents/views"

  // Plugins
],

function(app, Backbone, Views) {
  var Documents = app.module();

  Documents.Doc = Backbone.Model.extend({
    idAttribute: "_id",

    url: function() {
      return app.host + "/" + this.collection.id + "/" + this.id;
    }
  });

  Documents.AllDocs = Backbone.Collection.extend({
    model: Documents.Doc,

    initialize: function(_models, options) {
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
      that = this;
      return _.map(resp.rows, function(row) {
        return {
          "_id": row.id,
          rev: row.value.rev,
          value: row.value,
          key: row.key,
          doc: row.doc || undefined
        };
      });
    }
  });

  Documents.Views = Views;

  return Documents;
});
