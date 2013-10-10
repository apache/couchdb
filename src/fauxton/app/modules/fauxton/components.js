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
       // Libs
       "api"
],

function(app, FauxtonAPI) {
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
      FauxtonAPI.navigate(this.previousUrlfn(), {trigger: false});
      FauxtonAPI.triggerRouteEvent('paginate', 'previous');
    },

    nextClicked: function (event) {
      event.preventDefault();
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
      this.$el.typeahead({
        source: this.source
      });
    }

  });


  Components.DbSearchTypeahead = Components.Typeahead.extend({
    initialize: function (options) {
      this.dbLimit = options.dbLimit || 30;
      _.bindAll(this);
    },
    source: function(query, process) {
      var url = [
        app.host,
        "/_all_dbs?startkey=%22",
        query,
        "%22&endkey=%22",
        query,
        "\u9999%22&limit=",
        this.dbLimit
      ].join('');

      if (this.ajaxReq) { this.ajaxReq.abort(); }

      this.ajaxReq = $.ajax({
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
        "\u9999%22&limit=",
        this.docLimit
      ].join('');

      if (this.ajaxReq) { this.ajaxReq.abort(); }

      this.ajaxReq = $.ajax({
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

  return Components;
});

