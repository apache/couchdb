// Licensed under the Apache License, Version 2.0 (the "License"); you may not
// use this file except in compliance with the License. You may obtain a copy of
// the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
// WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
// License for the specific language governing permissions and limitations under
// the License.

define([
  "app",

  "api",

  // Views
  "modules/documents/views"

  // Plugins
],

function(app, FauxtonAPI, Views) {
  var Documents = app.module();

  Documents.Doc = Backbone.Model.extend({
    idAttribute: "_id",

    url: function(context) {
      if (context === "app") {
        return this.getDatabase().url("app") + "/" + this.safeID();
      } else {
        return app.host + "/" + this.getDatabase().id + "/" + this.id;
      }
    },

    initialize: function(_attrs, options) {
      if (this.collection && this.collection.database) {
        this.database = this.collection.database;
      } else if (options.database) {
        this.database = options.database;
      }
    },

    // HACK: the doc needs to know about the database, but it may be
    // set directly or indirectly in all docs
    getDatabase: function() {
      return this.database ? this.database : this.collection.database;
    },

    docType: function() {
      return this.id.match(/^_design/) ? "design doc" : "doc";
    },

    isEditable: function() {
      return this.docType() != "reduction";
    },

    isDdoc: function() {
      return this.docType() === "design doc";
    },

    hasViews: function() {
      if (!this.isDdoc()) return false;
      var doc = this.get('doc');
      return doc && doc.views && _.keys(doc.views).length > 0;
    },

    getDdocView: function(view) {
      if (!this.isDdoc() || !this.hasViews()) return false;

      var doc = this.get('doc');
      return doc.views[view];
    },

    viewHasReduce: function(viewName) {
      var view = this.getDdocView(viewName);

      return view && view.reduce;
    },

    // Need this to work around backbone router thinking _design/foo
    // is a separate route. Alternatively, maybe these should be
    // treated separately. For instance, we could default into the
    // json editor for docs, or into a ddoc specific page.
    safeID: function() {
      return this.id.replace('/', '%2F');
    },

    destroy: function() {
      var url = this.url() + "?rev=" + this.get('_rev');
      return $.ajax({
        url: url,
        dataType: 'json',
        type: 'DELETE'
      });
    },

    parse: function(resp) {
      if (resp.rev) {
        resp._rev = resp.rev;
        delete resp.rev;
      }
      if (resp.id) {
        if (typeof(this.id) === "undefined") {
          resp._id = resp.id;
        }
        delete resp.id;
      }
      if (resp.ok) {
        delete resp.ok;
      }

      return resp;
    },

    prettyJSON: function() {
      var data = this.get("doc") ? this.get("doc") : this;

      return JSON.stringify(data, null, "  ");
    }
  });

  Documents.ViewRow = Backbone.Model.extend({
    docType: function() {
      if (!this.id) return "reduction";

      return this.id.match(/^_design/) ? "design doc" : "doc";
    },

    url: function(context) {
      if (!this.isEditable()) return false;

      return this.collection.database.url(context) + "/" + this.id;
    },

    isEditable: function() {
      return this.docType() != "reduction";
    },

    prettyJSON: function() {
      //var data = this.get("doc") ? this.get("doc") : this;
      return JSON.stringify(this, null, "  ");
    }
  });

  Documents.NewDoc = Documents.Doc.extend({
    fetch: function() {
      var uuid = new FauxtonAPI.UUID();
      var deferred = this.deferred = $.Deferred();
      var that = this;

      uuid.fetch().done(function() {
        that.set("_id", uuid.next());
        deferred.resolve();
      });

      return deferred.promise();
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

    totalRows: function() {
      return this.viewMeta.total_rows || "unknown";
    },

    updateSeq: function() {
      return this.viewMeta.update_seq || false;
    },

    parse: function(resp) {
      that = this;
      this.viewMeta = {
        total_rows: resp.total_rows,
        offest: resp.offest,
        update_seq: resp.update_seq
      };
      return _.map(resp.rows, function(row) {
        return {
          _id: row.id,
          _rev: row.value.rev,
          value: row.value,
          key: row.key,
          doc: row.doc || undefined
        };
      });
    }
  });

  Documents.IndexCollection = Backbone.Collection.extend({
    model: Documents.ViewRow,

    initialize: function(_models, options) {
      this.database = options.database;
      this.view = options.view;
      this.design = options.design;
      this.params = _.extend({limit: 10, reduce: false}, options.params);
      this.idxType = "_view";
    },

    url: function() {
      var query = "";
      if (this.params) {
        query = "?" + $.param(this.params);
      }
      var url = [app.host, this.database.id, "_design", this.design, this.idxType, this.view];
      return url.join("/") + query;
    },

    totalRows: function() {
      return this.viewMeta.total_rows || "unknown";
    },

    updateSeq: function() {
      return this.viewMeta.update_seq || false;
    },

    parse: function(resp) {
      that = this;
      this.viewMeta = {
        total_rows: resp.total_rows,
        offest: resp.offest,
        update_seq: resp.update_seq
      };
      return _.map(resp.rows, function(row) {
        return {
          value: row.value,
          key: row.key,
          doc: row.doc,
          id: row.id
        };
      });
    },

    buildAllDocs: function(){
      this.fetch();
    },

    allDocs: function(){
      return this.models;
    }
  });

  Documents.Views = Views;

  return Documents;
});
