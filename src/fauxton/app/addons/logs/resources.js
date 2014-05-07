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
  "backbone",
  "d3",
  "addons/fauxton/components"
],

function (app, FauxtonAPI, Backbone, d3, Components) {

  var Log = FauxtonAPI.addon();

  Log.Model = Backbone.Model.extend({

    initialize: function () {
      this.dateObject = new Date(this.get('date'));
    },

    date: function () {
      var formatter = d3.time.format("%b %e");

      return formatter(this.dateObject);
    },

    time: function () {
      var formatter = d3.time.format("%H:%M:%S");

      return formatter(this.dateObject);
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
      var query = "?" + $.param(this.params);
      return app.host + '/_log' + query;
    },

    // override fetch because backbone expects json and couchdb sends text/html for logs,
    // I think its more elegant to set the dataType here than where ever fetch is called
    fetch: function (options) {
      options = options ? options : {};

      return Backbone.Collection.prototype.fetch.call(this, _.extend(options, {dataType: "html"}));
    },

    sortLogsIntoDays: function () {
      return _.reduce(this.toArray(), function (sortedCollection, log, key) {
        var date = log.date();

        if (!sortedCollection[date]) {
          sortedCollection[date] = [];
        }

        sortedCollection[date].push(log);
        return sortedCollection;
      }, {});
    },

    parse: function (resp) {
      resp = resp.replace(/\n\s/g, '');
      var lines = resp.split(/\n/);
      return _.foldr(lines, function (acc, logLine) {
        var match = logLine.match(/^\[(.*?)\]\s\[(.*?)\]\s\[(.*?)\]\s(.*)/);

        if (!match) { return acc;}

        acc.push({
                  date: match[1],
                  log_level: match[2],
                  pid: match[3],
                  args: match[4].replace(/\s\s+/g, '')
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
      var collection = new Log.Collection(this.createFilteredCollection());

      return {
        days: collection.sortLogsIntoDays()
      };
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

  Log.Views.Sidebar = FauxtonAPI.View.extend({
    template: "addons/logs/templates/sidebar",

    initialize: function (options) {
      this.setView(".js-filter", new Components.FilterView({
        eventListener: Log.events,
        eventNamespace: "log"
      }));
    }
  });

  return Log;

});
