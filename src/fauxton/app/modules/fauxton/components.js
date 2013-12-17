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

define('ace_configuration', ["app", "ace/ace"], function (app, ace) {
  var path = app.host + app.root + 'js/ace';
  var config = require("ace/config");
  config.set("packaged", true);
  config.set("workerPath",path);
  config.set("modePath",path);
  config.set("themePath", path);
  return ace;
});

define([
  "app",
  // Libs
  "api",
  "ace_configuration",
],

function(app, FauxtonAPI, ace) {
  var Components = app.module();

  Components.Pagination = FauxtonAPI.View.extend({
    template: "templates/fauxton/pagination",

    initialize: function(options) {
      this.page = parseInt(options.page, 10);
      this.perPage = options.perPage;
      this.total = options.total;
      this.totalPages = Math.ceil(this.total / this.perPage);
      this.urlFun = options.urlFun;
    },

    serialize: function() {
      return {
        page: this.page,
        perPage: this.perPage,
        total: this.total,
        totalPages: this.totalPages,
        urlFun: this.urlFun
      };
    }
  });

  Components.IndexPagination = FauxtonAPI.View.extend({
    template: "templates/fauxton/index_pagination",
    events: {
      "click a": 'scrollTo',
      "click a#next": 'nextClicked',
      "click a#previous": 'previousClicked'
    },

    previousIds: [],

    scrollTo: function () {
      if (!this.scrollToSelector) { return; }
      $(this.scrollToSelector).animate({ scrollTop: 0 }, 'slow');
    },

    initialize: function (options) {
      this.previousUrlfn = options.previousUrlfn;
      this.nextUrlfn = options.nextUrlfn;
      this.canShowPreviousfn = options.canShowPreviousfn;
      this.canShowNextfn = options.canShowNextfn;
      this.scrollToSelector = options.scrollToSelector;
      _.bindAll(this);
    },

    previousClicked: function (event) {
      event.preventDefault();
      event.stopPropagation();
      if (!this.canShowPreviousfn()) { return; }
      FauxtonAPI.navigate(this.previousUrlfn(), {trigger: false});
      FauxtonAPI.triggerRouteEvent('paginate', 'previous');
    },

    nextClicked: function (event) {
      event.preventDefault();
      event.stopPropagation();
      if (!this.canShowNextfn()) { return; }
      var doc = this.collection.first();

      if (doc) {
        this.previousIds.push(doc.id);
      }

      FauxtonAPI.navigate(this.nextUrlfn(), {trigger: false});
      FauxtonAPI.triggerRouteEvent('paginate', 'next');
    },

    serialize: function () {
      return {
        canShowNextfn: this.canShowNextfn,
        canShowPreviousfn: this.canShowPreviousfn,
      };
    }

  });

  //TODO allow more of the typeahead options.
  //Current this just does what we need but we
  //need to support the other typeahead options.
  Components.Typeahead = FauxtonAPI.View.extend({

    initialize: function (options) {
      this.source = options.source;
      _.bindAll(this);
    },

    afterRender: function () {
      var onUpdate = this.onUpdate;

      this.$el.typeahead({
        source: this.source,
        updater: function (item) {
          if (onUpdate) {
            onUpdate(item);
          }

          return item;
        }
      });
    }

  });


  Components.DbSearchTypeahead = Components.Typeahead.extend({
    initialize: function (options) {
      this.dbLimit = options.dbLimit || 30;
      this.onUpdate = options.onUpdate;
      _.bindAll(this);
    },
    source: function(query, process) {
      var url = [
        app.host,
        "/_all_dbs?startkey=%22",
        query,
        "%22&endkey=%22",
        query,
        "\u9999",
        "%22&limit=",
        this.dbLimit
      ].join('');

      if (this.ajaxReq) { this.ajaxReq.abort(); }

      this.ajaxReq = $.ajax({
        cache: false,
        url: url,
        dataType: 'json',
        success: function(data) {
          process(data);
        }
      });
    }
  });

  Components.DocSearchTypeahead = Components.Typeahead.extend({
    initialize: function (options) {
      this.docLimit = options.docLimit || 30;
      this.database = options.database;
      _.bindAll(this);
    },
    source: function(query, process) {
      var url = [
        app.host,
        "/",
        this.database.id,
        "/_all_docs?startkey=%22",
        query,
        "%22&endkey=%22",
        query,
        "\u9999",
        "%22&limit=",
        this.docLimit
      ].join('');

      if (this.ajaxReq) { this.ajaxReq.abort(); }

      this.ajaxReq = $.ajax({
        cache: false,
        url: url,
        dataType: 'json',
        success: function(data) {
          var ids = _.map(data.rows, function (row) {
            return row.id;
          });
          process(ids);
        }
      });
    }
  });

  Components.Editor = FauxtonAPI.View.extend({
    initialize: function (options) {
      this.editorId = options.editorId;
      this.mode = options.mode || "json";
      this.commands = options.commands;
      this.theme = options.theme || 'crimson_editor';
      this.couchJSHINT = options.couchJSHINT;
      this.edited = false;
    },

    afterRender: function () {
      this.editor = ace.edit(this.editorId);
      this.setHeightToLineCount();
      this.editor.setTheme("ace/theme/" + this.theme);
      this.editor.getSession().setMode("ace/mode/" + this.mode);
      this.editor.getSession().setUseWrapMode(true);
      this.editor.setShowPrintMargin(false);
      this.editor.gotoLine(2);
      this.addCommands();

      if (this.couchJSHINT) {
        this.removeIncorrectAnnotations();
      }

      var that = this;
      this.editor.getSession().on('change', function () {
        that.setHeightToLineCount();
        that.edited = true;
      });

      $(window).on('beforeunload.editor', function() {
        if (that.edited) {
          return 'Your changes have not been saved. Click cancel to return to the document.';
        }
      });

      FauxtonAPI.beforeUnload("editor", function (deferred) {
        if (that.edited) {
          return 'Your changes have not been saved. Click cancel to return to the document.';
        }
      });
    },

    cleanup: function () {
      $(window).off('beforeunload.editor');
      FauxtonAPI.removeBeforeUnload("editor");
    },

    setHeightToLineCount: function () {
      var lines = this.editor.getSession().getDocument().getLength();
      this.editor.setOptions({
        maxLines: lines
      });

      this.editor.resize();
    },

    addCommands: function () {
      _.each(this.commands, function (command) {
        this.editor.commands.addCommand(command);
      }, this);
    },

    removeIncorrectAnnotations: function () {
      var editor = this.editor;

      this.editor.getSession().on("changeAnnotation", function(){
        var annotations = editor.getSession().getAnnotations();

        var newAnnotations = _.reduce(annotations, function (annotations, error) {
          if (!FauxtonAPI.isIgnorableError(error.raw)) {
            annotations.push(error);
          }
          return annotations;
        }, []);

        if (annotations.length !== newAnnotations.length) {
          editor.getSession().setAnnotations(newAnnotations);
        }
      });
    },

    editSaved: function () {
      this.edited = false;
    },

    setValue: function (data, lineNumber) {
      lineNumber = lineNumber ? lineNumber : -1;
      this.editor.setValue(data, lineNumber);
    },

    getValue: function () {
      return this.editor.getValue();
    },

    getAnnotations: function () {
      return this.editor.getSession().getAnnotations();
    },

    hadValidCode: function () {
     var errors = this.getAnnotations();
     // By default CouchDB view functions don't pass lint
     return _.every(errors, function(error) {
      return FauxtonAPI.isIgnorableError(error.raw);
      },this);
    }

  });

  return Components;
});

