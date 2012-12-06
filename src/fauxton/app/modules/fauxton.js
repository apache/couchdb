define([
  "app",

  // Libs
  "backbone"

  // Modules

  // Views

  // Plugins
],

function(app, Backbone) {
  var Fauxton = app.module();

  Fauxton.Breadcrumbs = Backbone.View.extend({
    template: "templates/fauxton/breadcrumbs",

    serialize: function() {
      var crumbs = _.clone(this.crumbs);
      var last = crumbs.pop();
      return {
        crumbs: crumbs,
        last: last
      };
    },

    initialize: function(options) {
      this.crumbs = options.crumbs;
    }
  });

  Fauxton.NavBar = Backbone.View.extend({
    template: "templates/fauxton/nav_bar",
    // TODO: can we generate this list from the router?
    navLinks: [
      {href:"#/", title:"Dashboard"},
      {href:"#/_all_dbs", title:"Databases"},
      // {href:"#/_status", title:"Status"},
      // {href:"#/_stats", title:"Stats"},
      {href:"#/_config", title:"Config"},
      {href:"#/_log", title:"Log"}
    ],

    initialize: function() {
      this.on("link:add", this.render, this);
    },

    serialize: function() {
      return {navLinks: this.navLinks};
    },

    addLink: function(link) {
      this.navLinks.push(link);
      this.trigger("link:add");
      this.render();
    }

    // TODO: ADD ACTIVE CLASS
  });

  Fauxton.ApiBar = Backbone.View.extend({
    template: "templates/fauxton/api_bar",
    endpoint: '_all_docs',

    serialize: function() {
      return {endpoint: this.endpoint};
    },

    update: function(endpoint) {
      // Take endpoint and write it into the api bar.
      console.log('ApiBar endpoint: ' + endpoint);
      this.endpoint = endpoint;
      this.render();
    }

  });

  Fauxton.Notification = Backbone.View.extend({
    template: "templates/fauxton/notification",
    fadeTimer: 5000,

    initialize: function(options) {
      this.msg = options.msg;
      this.type = options.type || "info";
      this.selector = options.selector;
      this.fade = options.fade === undefined ? true : options.fade;
      this.clear = options.clear;
    },

    serialize: function() {
      return {
        msg: this.msg,
        type: this.type
      };
    },

    delayedFade: function() {
      var that = this;
      if (this.fade) {
        setTimeout(function() {
          that.$el.fadeOut();
        }, this.fadeTimer);
      }
    },

    renderNotification: function(selector) {
      selector = selector || this.selector;
      if (this.clear) {
        $(selector).html('');
      }
      this.render().view.$el.appendTo(selector);
      this.delayedFade();
      return this;
    }
  });

  return Fauxton;
});
