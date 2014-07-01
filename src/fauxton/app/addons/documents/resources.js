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
  "cloudant.pagingcollection"
],

function(app, FauxtonAPI, PagingCollection) {
  var Documents = FauxtonAPI.addon();

  Documents.QueryParams = (function () {
    var _eachParams = function (params, action) {
      // clone to avoid in-place modification
      var result = _.clone(params);

      _.each(['startkey', 'endkey', 'key'], function (key) {
        if (_.has(result, key)) {
          result[key] = action(result[key]);
        }
      });

      return result;
    };

    return {
      parse: function (params) {
        return _eachParams(params, JSON.parse);
      },

      stringify: function (params) {
        return _eachParams(params, JSON.stringify);
      }
    };
  })();


  Documents.Doc = FauxtonAPI.Model.extend({
    idAttribute: "_id",
    documentation: function(){
      return "docs";
    },
    url: function(context) {
      if (context === "app") {
        return this.getDatabase().url("app") + "/" + this.safeID();
      } else if (context === "web-index") {
        return this.getDatabase().url("app") + "/" + app.utils.safeURLName(this.id);
      } else if (context === "apiurl"){
        return window.location.origin + "/" + this.getDatabase().safeID() + "/" + this.safeID();
      } else {
        return app.host + "/" + this.getDatabase().safeID() + "/" + this.safeID();
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

    validate: function(attrs, options) {
      if (this.id && this.id !== attrs._id && this.get('_rev') ) {
        return "Cannot change a documents id.";
      }
    },

    docType: function() {
      return this.id && this.id.match(/^_design\//) ? "design doc" : "doc";
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
      var views = this.get('views'),
          tempView = views[view] || {};

      if (reduce) {
        tempView.reduce=reduce;
      } else {
        delete tempView.reduce;
      }
      tempView.map= map;

      views[view] = tempView;
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
      if (this.isDdoc()){
        var ddoc = this.id.replace(/^_design\//,"");
        return "_design/"+app.utils.safeURLName(ddoc);
      }else{
        return app.utils.safeURLName(this.id);
      }
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
      var data = this.get("doc") ? this.get("doc") : this.attributes;

      return JSON.stringify(data, null, "  ");
    },

    copy: function (copyId) {
      return $.ajax({
        type: 'COPY',
        url: '/' + this.database.safeID() + '/' + this.safeID(),
        headers: {Destination: copyId}
      });
    },

    isNewDoc: function () {
      return this.get('_rev') ? false : true;
    }
  });

  Documents.DdocInfo = FauxtonAPI.Model.extend({
    idAttribute: "_id",
    documentation: function(){
      return "docs";
    },
    initialize: function (_attrs, options) {
      this.database = options.database;
    },

    url: function(context) {
      if (context === "app") {
        return this.database.url("app") + "/" + this.safeID() + '/_info';
      } else if (context === "apiurl"){
        return window.location.origin + "/" + this.database.safeID() + "/" + this.safeID() + '/_info';
      } else {
        return app.host + "/" + this.database.safeID() + "/" + this.safeID() + '/_info';
      }
    },

    // Need this to work around backbone router thinking _design/foo
    // is a separate route. Alternatively, maybe these should be
    // treated separately. For instance, we could default into the
    // json editor for docs, or into a ddoc specific page.
    safeID: function() {
      var ddoc = this.id.replace(/^_design\//,"");
      return "_design/"+app.utils.safeURLName(ddoc);
    }

  });

  Documents.ViewRow = FauxtonAPI.Model.extend({
    // this is a hack so that backbone.collections doesn't group
    // these by id and reduce the number of items returned.
    idAttribute: "_id",

    docType: function() {
      if (!this.id) return "reduction";

      return this.id.match(/^_design/) ? "design doc" : "doc";
    },
    documentation: function(){
      return "docs";
    },
    url: function(context) {
      return this.collection.database.url(context) + "/" + this.safeID();
    },

    isEditable: function() {
      return this.docType() != "reduction";
    },
    safeID: function() {
      var id = this.id || this.get("id");

      return app.utils.safeURLName(id);
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

  Documents.BulkDeleteDoc = FauxtonAPI.Model.extend({
    idAttribute: "_id"
  });

  Documents.BulkDeleteDocCollection = FauxtonAPI.Collection.extend({

    model: Documents.BulkDeleteDoc,

    sync: function () {

    },

    initialize: function (models, options) {
      this.databaseId = options.databaseId;
    },

    bulkDelete: function () {
      var payload = this.createPayload(this.toJSON()),
          that = this;

      $.ajax({
        type: 'POST',
        url: app.host + '/' + this.databaseId + '/_bulk_docs',
        contentType: 'application/json',
        dataType: 'json',
        data: JSON.stringify(payload),
      })
      .then(function (res) {
        that.handleResponse(res);
      })
      .fail(function () {
        var ids = _.reduce(that.toArray(), function (acc, doc) {
          acc.push(doc.id);
          return acc;
        }, []);
        that.trigger('error', ids);
      });
    },

    handleResponse: function (res) {
      var ids = _.reduce(res, function (ids, doc) {
        if (doc.error) {
          ids.errorIds.push(doc.id);
        }

        if (doc.ok === true) {
          ids.successIds.push(doc.id);
        }

        return ids;
      }, {errorIds: [], successIds: []});

      this.removeDocuments(ids.successIds);

      if (ids.errorIds.length) {
        this.trigger('error', ids.errorIds);
      }

      this.trigger('updated');
    },

    removeDocuments: function (ids) {
      var reloadDesignDocs = false;
      _.each(ids, function (id) {
        if (/_design/.test(id)) {
          reloadDesignDocs = true;
        }

        this.remove(this.get(id));
      }, this);

      if (reloadDesignDocs) {
        FauxtonAPI.triggerRouteEvent('reloadDesignDocs');
      }

      this.trigger('removed', ids);
    },

    createPayload: function (documents) {
      var documentList = documents;

      return {
        docs: documentList
      };
    }
  });


  Documents.AllDocs = PagingCollection.extend({
    model: Documents.Doc,
    documentation: function(){
      return "docs";
    },
    initialize: function(_models, options) {
      this.database = options.database;
      this.params = _.clone(options.params);

      this.on("remove",this.decrementTotalRows , this);
      this.perPageLimit = options.perPageLimit || 20;

      if (!this.params.limit) {
        this.params.limit = this.perPageLimit;
      }
    },

    urlRef: function(context, params) {
      var query = "";

      if (params) {
        if (!_.isEmpty(params)) {
          query = "?" + $.param(params);
        } else {
          query = '';
        }
      } else if (this.params) {
        query = "?" + $.param(this.params);
      }

      if (context === 'app') {
        return 'database/' + this.database.safeID() + "/_all_docs" + query;
      } else if (context === "apiurl"){
        return window.location.origin + "/" + this.database.safeID() + "/_all_docs" + query;
      } else {
        return app.host + "/" + this.database.safeID() + "/_all_docs" + query;
      }
    },

    url: function () {
      return this.urlRef.apply(this, arguments);
    },

    simple: function () {
      var docs = this.map(function (item) {
        return {
          _id: item.id,
          _rev: item.get('_rev'),
        };
      });

      return new Documents.AllDocs(docs, {
        database: this.database,
        params: this.params
      });
    },

    totalRows: function() {
      return this.viewMeta.total_rows || "unknown";
    },

    decrementTotalRows: function () {
      if (this.viewMeta.total_rows) {
        this.viewMeta.total_rows = this.viewMeta.total_rows -1;
        this.trigger('totalRows:decrement');
      }
    },

    updateSeq: function() {
      return this.viewMeta.update_seq || false;
    },

    parse: function(resp) {
      var rows = resp.rows;

      // remove any query errors that may return without doc info
      // important for when querying keys on all docs
      var cleanRows = _.filter(rows, function(row){
        return row.value;
      });

      resp.rows = _.map(cleanRows, function(row){
        return {
          _id: row.id,
          _rev: row.value.rev,
          value: row.value,
          key: row.key,
          doc: row.doc || undefined
        };
      });

      return PagingCollection.prototype.parse.call(this, resp);
    },

    clone: function () {
      return new this.constructor(this.models, {
        database: this.database,
        params: this.params,
        paging: this.paging
      });
    }
  });

  Documents.IndexCollection = PagingCollection.extend({
    model: Documents.ViewRow,
    documentation: function(){
      return "docs";
    },
    initialize: function(_models, options) {
      this.database = options.database;
      this.params = _.extend({limit: 20, reduce: false}, options.params);

      this.idxType = "_view";
      this.view = options.view;
      this.design = options.design.replace('_design/','');
      this.perPageLimit = options.perPageLimit || 20;

      if (!this.params.limit) {
        this.params.limit = this.perPageLimit;
      }
    },

    urlRef: function(context, params) {
      var query = "";
      
      if (params) {
        if (!_.isEmpty(params)) {
          query = "?" + $.param(params);
        } else {
          query = '';
        }
      } else if (this.params) {
        var parsedParam = Documents.QueryParams.stringify(this.params);
        query = "?" + $.param(parsedParam);
      }

      var startOfUrl = app.host;
      if (context === 'app') {
        startOfUrl = 'database';
      } else if (context === "apiurl"){
        startOfUrl = window.location.origin;
      }
      var design = app.utils.safeURLName(this.design),
          view = app.utils.safeURLName(this.view);

      var url = [startOfUrl, this.database.safeID(), "_design", design, this.idxType, view];
      return url.join("/") + query;
    },

    url: function () {
      return this.urlRef.apply(this, arguments);
    },

    totalRows: function() {
      if (this.params.reduce) { return "unknown_reduce";}

      return this.viewMeta.total_rows || "unknown";
    },

    updateSeq: function() {
      return this.viewMeta.update_seq || false;
    },

    simple: function () {
      var docs = this.map(function (item) {
        return {
          _id: item.id,
          key: item.get('key'),
          value: item.get('value')
        };
      });

      return new Documents.IndexCollection(docs, {
        database: this.database,
        params: this.params,
        view: this.view,
        design: this.design
      });
    },

    parse: function(resp) {
      var rows = resp.rows;
      this.endTime = new Date().getTime();
      this.requestDuration = (this.endTime - this.startTime);

      return PagingCollection.prototype.parse.apply(this, arguments);
    },

    buildAllDocs: function(){
      this.fetch();
    },

    // We implement our own fetch to store the starttime so we that
    // we can get the request duration
    fetch: function () {
      this.startTime = new Date().getTime();
      return PagingCollection.prototype.fetch.call(this);
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


  Documents.PouchIndexCollection = PagingCollection.extend({
    model: Documents.ViewRow,
    documentation: function(){
      return "docs";
    },
    initialize: function(_models, options) {
      this.database = options.database;
      this.rows = options.rows;
      this.view = options.view;
      this.design = options.design.replace('_design/','');
      this.params = _.extend({limit: 20, reduce: false}, options.params);

      this.idxType = "_view";
    },

    url: function () {
      return '';
    },

    simple: function () {
      var docs = this.map(function (item) {
        return {
          _id: item.id,
          key: item.get('key'),
          value: item.get('value')
        };
      });

      return new Documents.PouchIndexCollection(docs, {
        database: this.database,
        params: this.params,
        view: this.view,
        design: this.design,
        rows: this.rows
      });

    },

    fetch: function() {
      var deferred = FauxtonAPI.Deferred();
      this.reset(this.rows, {silent: true});

      this.viewMeta = {
        total_rows: this.rows.length,
        offset: 0,
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
