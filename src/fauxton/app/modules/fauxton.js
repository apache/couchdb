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
      {href:"#/", title:"Home"},
      {href:"#/_all_dbs", title:"Databases"},
      {href:"#/_status", title:"Status"},
      {href:"#/_stats", title:"Stats"},
      {href:"#/_log", title:"Log"}
    ],

    initialize: function() {
      console.log('initialize NavBar wat');
      this.on("link:add", this.render, this);
      console.log('initialize NavBar ftw');
    },

    serialize: function() {
      return {navLinks: this.navLinks};
    },

    addLink: function(link) {
      console.log("ADDING HEADER LINK");
      console.log(link);
      this.navLinks.push(link);
      this.trigger("link:add");
      this.render();
    }

    // TODO: ADD ACTIVE CLASS
  });

  return Fauxton;
});
