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
  "spin"
],

function(app, FauxtonAPI, ace, spin) {
  var Components = FauxtonAPI.addon();

  Components.Pagination = FauxtonAPI.View.extend({
    template: "addons/fauxton/templates/pagination",

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
    template: "addons/fauxton/templates/index_pagination",
    events: {
      "click a": 'scrollTo',
      "click a#next": 'nextClicked',
      "click a#previous": 'previousClicked'
    },

    scrollTo: function () {
      if (!this.scrollToSelector) { return; }
      $(this.scrollToSelector).animate({ scrollTop: 0 }, 'slow');
    },

    initialize: function (options) {
      this.previousUrlfn = options.previousUrlfn;
      this.nextUrlfn = options.nextUrlfn;
      this.scrollToSelector = options.scrollToSelector;
      _.bindAll(this);
      this.docLimit = options.docLimit || 1000000;
      this.perPage = options.perPage || 20;
      this.setDefaults();
    },

    setDefaults: function () {
      this._pageNumber = [];
      this._pageStart = 1;
      this.enabled = true;
      this.currentPage = 1;
    },

    canShowPreviousfn: function () {
      if (!this.enabled) { return this.enabled; }
      return this.collection.hasPrevious();
    },

    canShowNextfn: function () {
      if (!this.enabled) { return this.enabled; }

      if ((this.pageStart() + this.perPage) >= this.docLimit) {
        return false;
      }

      return this.collection.hasNext();
    },

    previousClicked: function (event) {
      event.preventDefault();
      event.stopPropagation();
      if (!this.canShowPreviousfn()) { return; }

      this.decPageNumber();

      FauxtonAPI.triggerRouteEvent('paginate', {
       direction: 'previous',
       perPage: this.perPage,
       currentPage: this.currentPage
      });
    },

    documentsLeftToFetch: function () {
      var documentsLeftToFetch = this.docLimit - this.totalDocsViewed(),
          limit = this.perPage;

      if (documentsLeftToFetch < this.perPage ) {
        limit = documentsLeftToFetch;
      }

      return limit;
    },

    nextClicked: function (event) {
      event.preventDefault();
      event.stopPropagation();
      if (!this.canShowNextfn()) { return; }

      this.incPageNumber();

      FauxtonAPI.triggerRouteEvent('paginate', {
       direction: 'next',
       perPage: this.documentsLeftToFetch(),
       currentPage: this.currentPage
      });

    },

    serialize: function () {
      return {
        canShowNextfn: this.canShowNextfn,
        canShowPreviousfn: this.canShowPreviousfn,
      };
    },

    updatePerPage: function (newPerPage) {
      this.setDefaults();
      this.perPage = newPerPage;
    },

    page: function () {
      return this._pageStart - 1;
    },

    incPageNumber: function () {
      this.currentPage = this.currentPage + 1;
      this._pageNumber.push({perPage: this.perPage});
      this._pageStart = this._pageStart + this.perPage;
    },

    totalDocsViewed: function () {
      return _.reduce(this._pageNumber, function (total, value) {
        return total + value.perPage;
      }, 0);
    },

    decPageNumber: function () {
      this.currentPage = this.currentPage - 1;
      this._pageNumber.pop();
      var val = this._pageStart - this.perPage;
      if (val < 1) {
        val = 1;
      }

      this._pageStart = val;
    },

    pageStart: function () {
      return this._pageStart;
    },

    pageEnd: function () {
      return this.page() + this.collection.length;
    },

    disable: function () {
      this.enabled = false;
    },

    enable: function () {
      this.enabled = true;
    },

    setCollection: function (collection) {
      this.collection = collection;
      this.setDefaults();
    },

  });



  Components.ModalView = FauxtonAPI.View.extend({

    disableLoader: true,

    initialize: function (options) {
      _.bindAll(this);
    },

    showModal: function () {
      if (this._showModal){ this._showModal();}
      this.clear_error_msg();
      this.$('.modal').modal();
      // hack to get modal visible
      $('.modal-backdrop').css('z-index',1025);
    },

    hideModal: function () {
      this.$('.modal').modal('hide');
    },

    set_error_msg: function (msg) {
      var text;
      if (typeof(msg) == 'string') {
        text = msg;
      } else {
        text = JSON.parse(msg.responseText).reason;
      }
      this.$('#modal-error').text(text).removeClass('hide');
    },

    clear_error_msg: function () {
      this.$('#modal-error').text(' ').addClass('hide');
    },

    serialize: function () {
      if (this.model){
        return this.model.toJSON();
      }
      return {};
    }
  });

  //TODO allow more of the typeahead options.
  //Current this just does what we need but we
  //need to support the other typeahead options.
  Components.Typeahead = FauxtonAPI.View.extend({

    initialize: function (options) {
      this.source = options.source;
      this.onUpdate = options.onUpdate;
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

  Components.FilterView = FauxtonAPI.View.extend({
    template: "addons/fauxton/templates/filter",

    initialize: function (options) {
      this.eventListener = options.eventListener;
      this.eventNamespace = options.eventNamespace;
    },

    events: {
      "submit .js-log-filter-form": "filterLogs"
    },

    filterLogs: function (event) {
      event.preventDefault();
      var $filter = this.$('input[name="filter"]'),
          filter = $.trim($filter.val());

      if (!filter) {
        return;
      }

      this.eventListener.trigger(this.eventNamespace + ":filter", filter);

      this.insertView(".filter-list", new Components.FilterItemView({
        filter: filter,
        eventListener: this.eventListener,
        eventNamespace: this.eventNamespace
      })).render();

      $filter.val('');
    }

  });

  Components.FilterItemView = FauxtonAPI.View.extend({
    template: "addons/fauxton/templates/filter_item",
    tagName: "li",

    initialize: function (options) {
      this.filter = options.filter;
      this.eventListener = options.eventListener;
      this.eventNamespace = options.eventNamespace;
    },

    events: {
      "click .js-remove-filter": "removeFilter"
    },

    serialize: function () {
      return {
        filter: this.filter
      };
    },

    removeFilter: function (event) {
      event.preventDefault();

      this.eventListener.trigger(this.eventNamespace + ":remove", this.filter);
      this.remove();
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

      _.bindAll(this);
    },

    afterRender: function () {
      this.editor = ace.edit(this.editorId);
      this.setHeightToLineCount();

      this.editor.setTheme("ace/theme/" + this.theme);

      this.editor.getSession().setMode("ace/mode/" + this.mode);
      this.editor.setShowPrintMargin(false);
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

    getLines: function(){
     return this.editor.getSession().getDocument().getLength();
    },

    addCommands: function () {
      _.each(this.commands, function (command) {
        this.editor.commands.addCommand(command);
      }, this);
    },

    removeIncorrectAnnotations: function () {
      var editor = this.editor,
          isIgnorableError = this.isIgnorableError;

      this.editor.getSession().on("changeAnnotation", function () {
        var annotations = editor.getSession().getAnnotations();

        var newAnnotations = _.reduce(annotations, function (annotations, error) {
          if (!isIgnorableError(error.raw)) {
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

    setReadOnly: function (value) {
      return this.editor.setReadOnly(value);
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
      return this.isIgnorableError(error.raw);
      },this);
    },

    // List of JSHINT errors to ignore
    // Gets around problem of anonymous functions not being a valid statement
    excludedViewErrors: [
      "Missing name in function declaration.",
      "['{a}'] is better written in dot notation."
    ],

    isIgnorableError: function(msg) {
      return _.contains(this.excludedViewErrors, msg);
    }

  });

  //need to make this into a backbone view...
  var routeObjectSpinner;
  FauxtonAPI.RouteObject.on('beforeEstablish', function (routeObject) {
    if (!routeObject.disableLoader){ 
      var opts = {
        lines: 16, // The number of lines to draw
        length: 8, // The length of each line
        width: 4, // The line thickness
        radius: 12, // The radius of the inner circle
        color: '#333', // #rbg or #rrggbb
        speed: 1, // Rounds per second
        trail: 10, // Afterglow percentage
        shadow: false // Whether to render a shadow
     };

     if (routeObjectSpinner) { return; }

     if (!$('.spinner').length) {
       $('<div class="spinner"></div>')
        .appendTo('#app-container');
     }

     routeObjectSpinner = new Spinner(opts).spin();
     $('.spinner').append(routeObjectSpinner.el);
   }
  });

  var removeRouteObjectSpinner = function () {
    if (routeObjectSpinner) {
      routeObjectSpinner.stop();
      routeObjectSpinner = null;
      $('.spinner').remove();
    }
  };

  var removeViewSpinner = function (selector) {
    var viewSpinner = viewSpinners[selector];

    if (viewSpinner){
      viewSpinner.stop();
      $(selector).find('.spinner').remove();
      delete viewSpinners[selector];
    }
  };

  var viewSpinners = {};
  FauxtonAPI.RouteObject.on('beforeRender', function (routeObject, view, selector) {
    removeRouteObjectSpinner();

    if (!view.disableLoader){ 
      var opts = {
        lines: 16, // The number of lines to draw
        length: 8, // The length of each line
        width: 4, // The line thickness
        radius: 12, // The radius of the inner circle
        color: '#333', // #rbg or #rrggbb
        speed: 1, // Rounds per second
        trail: 10, // Afterglow percentage
        shadow: false // Whether to render a shadow
      };

      var viewSpinner = new Spinner(opts).spin();
      $('<div class="spinner"></div>')
        .appendTo(selector)
        .append(viewSpinner.el);

      viewSpinners[selector] = viewSpinner;
    }
  });

  FauxtonAPI.RouteObject.on('afterRender', function (routeObject, view, selector) {
    removeViewSpinner(selector);
  });

  FauxtonAPI.RouteObject.on('viewHasRendered', function (view, selector) {
    removeViewSpinner(selector);
    removeRouteObjectSpinner();
  });

  return Components;
});

