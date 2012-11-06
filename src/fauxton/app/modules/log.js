define([
  "app",

  // Libs
  "backbone",

  // Modules
  "modules/fauxton"

],

function (app, backbone, Fauxton) {

  var Log = app.module(); 

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
      var logs = [],
          lines =  resp.split(/\n/);

      _.each(lines, function (log) {
        raw_items = log.split("]");

        if (raw_items.length < 4) { return; }

        var log_row = {
                        date: raw_items[0].replace("["," "),
                        log_level: raw_items[1].replace("["," "),
                        pid: raw_items[2].replace("["," "),
                        args: raw_items[3].replace("["," ")
                      };

        logs.push(log_row);
      });

      return logs.reverse();
    }

  });

  Log.View = Backbone.View.extend({
    template: "log/log",

    initialize: function (options) {
      this.refreshTime = options.refreshTime || 5000;

      this.collection.on('reset', this.render, this);
    },

    serialize: function () {
      return { logs: this.collection.toJSON()};
    },

    afterRender: function () {
      this.startRefreshInterval();
    },

    cleanup: function() {
      this.stopRefreshInterval();
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


  return Log;

});
