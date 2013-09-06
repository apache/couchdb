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
  var Paginate = app.module();
   
  Paginate.Pagination = FauxtonAPI.View.extend({
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

  Paginate.IndexPagination = FauxtonAPI.View.extend({
    template: "templates/fauxton/index_pagination",
    events: {
      "click a": 'scrollTo',
      "click a#next": 'nextClicked',
      "click a#previous": 'previousClicked'
    },

    currentDirection: 'next',
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
      this.currentDirection = 'previous';
      FauxtonAPI.navigate(this.previousUrlfn());
    },

    nextClicked: function (event) {
      event.preventDefault();
      this.currentDirection = 'next';
      this.previousIds.push(this.collection.first().id);
      FauxtonAPI.navigate(this.nextUrlfn());
    },

    serialize: function () {
      return {
        canShowNextfn: this.canShowNextfn,
        canShowPreviousfn: this.canShowPreviousfn,
      };
    }

  });

  return Paginate;
});

