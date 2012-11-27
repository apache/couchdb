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
    },

    docType: function() {
      return this.id.match(/^_design/) ? "design doc" : "doc";
    },

    // Need this to work around backbone router thinking _design/foo
    // is a separate route. Alternatively, maybe these should be
    // treated separately. For instance, we could default into the
    // json editor for docs, or into a ddoc specific page.
    safeID: function() {
      return this.id.replace('/', '%2F');
    },

    pageUrl: function() {
      return this.collection.database.pageUrl() + "/" + this.safeID();
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
