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

  "api"
],

function(app, FauxtonAPI) {
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
      if (doc) {
        return doc && doc.views && _.keys(doc.views).length > 0;
      }

      var views = this.get('views');
      return views && _.keys(views).length > 0;
    },

    hasAttachments: function () {
      return !!this.get('_attachments');
    },

    getDdocView: function(view) {
      if (!this.isDdoc() || !this.hasViews()) return false;

      var doc = this.get('doc');
      if (doc) {
        return doc.views[view];
      }

      return this.get('views')[view];
    },

    setDdocView: function (view, map, reduce) {
      if (!this.isDdoc()) return false;
      var views = this.get('views');

      if (reduce) {
        views[view] = {
          map: map,
          reduce: reduce
        }; 
      } else {
        views[view].map = map;
      }

      this.set({views: views});

      return true;
    },

    removeDdocView: function (viewName) {
      if (!this.isDdoc()) return false;
      var views = this.get('views');

      delete views[viewName];
      this.set({views: views});
    },

    dDocModel: function () {
      if (!this.isDdoc()) return false;
      var doc = this.get('doc');

      if (doc) {
        return new Documents.Doc(doc, {database: this.database});
      } 

      return this;
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
    },

    copy: function (copyId) {
      return $.ajax({
        type: 'COPY',
        url: '/' + this.database.id + '/' + this.id,
        headers: {Destination: copyId}
      });
    },

    isNewDoc: function () {
      return this.get('_rev') ? false : true;
    }
  });

  Documents.DdocInfo = Backbone.Model.extend({
    idAttribute: "_id",

    initialize: function (_attrs, options) {
      this.database = options.database;
    },

    url: function(context) {
      if (context === "app") {
        return this.database.url("app") + "/" + this.safeID() + '/_info';
      } else {
        return app.host + "/" + this.database.id + "/" + this.id + '/_info';
      }
    },

    // Need this to work around backbone router thinking _design/foo
    // is a separate route. Alternatively, maybe these should be
    // treated separately. For instance, we could default into the
    // json editor for docs, or into a ddoc specific page.
    safeID: function() {
      return this.id.replace('/', '%2F');
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
    },

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
        offest: resp.offset,
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
      this.params = _.extend({limit: 10, reduce: false}, options.params);
      this.idxType = "_view";
      this.view = options.view;
      this.design = options.design.replace('_design/','');
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
      this.endTime = new Date().getTime();
      this.requestDuration = (this.endTime - this.startTime);

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

    // We implement our own fetch to store the starttime so we that
    // we can get the request duration
    fetch: function () {
      this.startTime = new Date().getTime();
      return Backbone.Collection.prototype.fetch.call(this);
    },

    allDocs: function(){
      return this.models;
    },

    // This is taken from futon.browse.js $.timeString
    requestDurationInString: function () {
      var ms, sec, min, h, timeString, milliseconds = this.requestDuration;

      sec = Math.floor(milliseconds / 1000.0);
      min = Math.floor(sec / 60.0);
      sec = (sec % 60.0).toString();
      if (sec.length < 2) {
         sec = "0" + sec;
      }

      h = (Math.floor(min / 60.0)).toString();
      if (h.length < 2) {
        h = "0" + h;
      }

      min = (min % 60.0).toString();
      if (min.length < 2) {
        min = "0" + min;
      }

      timeString = h + ":" + min + ":" + sec;

      ms = (milliseconds % 1000.0).toString();
      while (ms.length < 3) {
        ms = "0" + ms;
      }
      timeString += "." + ms;

      return timeString;
    }
  });

  
  Documents.PouchIndexCollection = Backbone.Collection.extend({
    model: Documents.ViewRow,

    initialize: function(_models, options) {
      this.database = options.database;
      this.rows = options.rows;
      this.view = options.view;
      this.design = options.design.replace('_design/','');
      this.params = _.extend({limit: 10, reduce: false}, options.params);
      this.idxType = "_view";
    },

    url: function () {
      return '';
    },

    fetch: function() {
      var deferred = FauxtonAPI.Deferred();
      this.reset(this.rows, {silent: true});

      this.viewMeta = {
        total_rows: this.rows.length,
        offest: 0,
        update_seq: false
      };

      deferred.resolve();
      return deferred;
    },

    totalRows: function() {
      return this.viewMeta.total_rows || "unknown";
    },

    updateSeq: function() {
      return this.viewMeta.update_seq || false;
    },

    buildAllDocs: function(){
      this.fetch();
    },

    allDocs: function(){
      return this.models;
    }
  });



  return Documents;
});
