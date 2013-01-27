define([
  "app",
  "api"
],

function (app, FauxtonAPI) {

  var Log = FauxtonAPI.addon();

  Log.Model = Backbone.Model.extend({ });

  Log.Collection = Backbone.Collection.extend({
    model: Log.Model,

    initialize: function (options) {
      this.params = {bytes: 5000};
    },

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
      this.filteredCollection = new Log.Collection(this.collection.toJSON());
      this.collection.on("reset", function () {
        this.createFilteredCollection();
      }, this);
    },

    serialize: function () {
      return { logs: this.filteredCollection.toJSON()};
    },

    afterRender: function () {
      this.startRefreshInterval();
    },

    cleanup: function () {
      this.stopRefreshInterval();
    },

    filterLogs: function (filter) {
      this.filters.push(filter);
      this.createFilteredCollection();
    },

    resetFilterCollectionAndRender: function (logs) {
      this.filteredCollection.reset(logs);
      this.render();
    },

    createFilteredCollection: function () {
      var self = this;

      var filtered = _.reduce(this.filters, function (logs, filter) {

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

      this.resetFilterCollectionAndRender(filtered);
    },

    removeFilterLogs: function (filter) {
      this.filters.splice(this.filters.indexOf(filter), 1);
      this.createFilteredCollection();
    },

    startRefreshInterval: function () {
      var self = this;

      // Interval already set
      if (self.intervalId) { return ; }

      self.intervalId = setInterval(function () {
        self.collection.fetch();
      }, self.refreshTime);

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
