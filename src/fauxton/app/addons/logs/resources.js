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
  "backbone"
],

function (app, FauxtonAPI, Backbone) {

  var Log = FauxtonAPI.addon();

  Log.Model = Backbone.Model.extend({

    date: function () {
      var date = new Date(this.get('date'));

      var formatted_time = date.getHours() + ":" + date.getMinutes() + ":" + date.getSeconds();
      var formatted_date = date.toDateString().slice(4, 10);

      return formatted_date + ' ' + formatted_time;
    },

    logLevel: function () {
      return this.get('log_level').replace(/ /g,'');
    },

    pid: function () {
      return _.escape(this.get('pid'));
    },

    args: function () {
      return _.escape(this.get('args'));
    }

  });

  Log.Collection = Backbone.Collection.extend({
    model: Log.Model,

    initialize: function (options) {
      this.params = {bytes: 5000};
    },
    
    documentation: "log",

    url: function () {
      query = "?" + $.param(this.params);
      return app.host + '/_log' + query;
    },

    // override fetch because backbone expects json and couchdb sends text/html for logs,
    // I think its more elegant to set the dataType here than where ever fetch is called
    fetch: function (options) {
      options = options ? options : {};

      return Backbone.Collection.prototype.fetch.call(this, _.extend(options, {dataType: "html"}));
    },

    parse: function (resp) {
      var lines =  resp.split(/\n/);
      return _.foldr(lines, function (acc, logLine) {
        var match = logLine.match(/^\[(.*?)\]\s\[(.*?)\]\s\[(.*?)\]\s(.*)/);

        if (!match) { return acc;}

        acc.push({
                  date: match[1],
                  log_level: match[2],
                  pid: match[3],
                  args: match[4]
                 });

        return acc;
      }, []);
    }
  });

  Log.events = {};
  _.extend(Log.events, Backbone.Events);

  Log.Views.View = FauxtonAPI.View.extend({
    template: "addons/logs/templates/dashboard",

    initialize: function (options) {
      this.refreshTime = options.refreshTime || 5000;

      Log.events.on("log:filter", this.filterLogs, this);
      Log.events.on("log:remove", this.removeFilterLogs, this);

      this.filters = [];

      this.collection.on("add", function () {
        this.render();
      }, this);
    },

    establish: function () {
      return [this.collection.fetch()];
    },

    serialize: function () {
      return { logs: new Log.Collection(this.createFilteredCollection())};
    },

    afterRender: function () {
      this.startRefreshInterval();
    },

    cleanup: function () {
      this.stopRefreshInterval();
    },

    filterLogs: function (filter) {
      this.filters.push(filter);
      this.render();
    },

    createFilteredCollection: function () {
      var that = this;

      return _.reduce(this.filters, function (logs, filter) {

        return _.filter(logs, function (log) {
          var match = false;

          _.each(log, function (value) {
            if (value.toString().match(new RegExp(filter))) {
              match = true;
            }
          });
          return match;
        });


      }, this.collection.toJSON(), this);

    },

    removeFilterLogs: function (filter) {
      this.filters.splice(this.filters.indexOf(filter), 1);
      this.render();
    },

    startRefreshInterval: function () {
      var collection = this.collection;

      // Interval already set
      if (this.intervalId) { return ; }

      this.intervalId = setInterval(function () {
        collection.fetch();
      }, this.refreshTime);

    },

    stopRefreshInterval: function () {
      clearInterval(this.intervalId);
    }
  });

  Log.Views.FilterView = FauxtonAPI.View.extend({
    template: "addons/logs/templates/sidebar",

    events: {
      "submit #log-filter-form": "filterLogs"
    },

    filterLogs: function (event) {
      event.preventDefault();
      var $filter = this.$('input[name="filter"]'),
          filter = $filter.val();

      Log.events.trigger("log:filter", filter);

      this.insertView("#filter-list", new Log.Views.FilterItemView({
        filter: filter
      })).render();

      $filter.val('');
    }

  });

  Log.Views.FilterItemView = FauxtonAPI.View.extend({
    template: "addons/logs/templates/filterItem",
    tagName: "li",

    initialize: function (options) {
      this.filter = options.filter;
    },

    events: {
      "click .remove-filter": "removeFilter"
    },

    serialize: function () {
      return {
        filter: this.filter
      };
    },

    removeFilter: function (event) {
      event.preventDefault();

      Log.events.trigger("log:remove", this.filter);
      this.remove();
    }

  });


  return Log;

});
