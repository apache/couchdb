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
