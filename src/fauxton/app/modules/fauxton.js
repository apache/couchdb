define([
  "app",

  // Libs
  "backbone"

  // Modules

  // Views

  // Plugins
],

function(app, Backbone, Logs) {
  var Fauxton = app.module();

  Fauxton.Breadcrumbs = Backbone.View.extend({
    template: "fauxton/breadcrumbs",

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
    template: "fauxton/nav_bar",

    navLinks: [
      {href:"#/", title:"Dashboard"},
      {href:"#/_all_dbs", title:"Databases"},
      {href:"#/_status", title:"Status"},
      {href:"#/_stats", title:"Stats"},
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
    template: "fauxton/api_bar",
    endpoint: '_all_docs',

    serialize: function() {
      return {endpoint: this.endpoint};
    },

    update: function(endpoint) {
      // Take endpoint and write it into the api bar.
      this.endpoint = endpoint;
      this.render();
    }

  });

  return Fauxton;
});
