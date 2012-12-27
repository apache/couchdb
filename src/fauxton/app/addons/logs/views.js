define([
  "app",
  "api"
],

function(app, FauxtonAPI) {
  var Views = {};

  Views.View = FauxtonAPI.View.extend({
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

  Views.FilterView = FauxtonAPI.View.extend({
    template: "addons/logs/templates/sidebar",

    events: {
      "submit #log-filter-form": "filterLogs"
    },

    filterLogs: function (event) {
      event.preventDefault();
      var $filter = this.$('input[name="filter"]'),
          filter = $filter.val();

      Log.events.trigger("log:filter", filter);

      this.insertView("#filter-list", new Log.FilterItemView({
        filter: filter
      })).render();

      $filter.val('');
    }

  });

  Views.FilterItemView = FauxtonAPI.View.extend({
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

  return Views;
});
