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
       "backbone"

],

function(app, Backbone) {
  var Fauxton = app.module();

  Fauxton.Breadcrumbs = Backbone.View.extend({
    template: "templates/fauxton/breadcrumbs",

    serialize: function() {
      var crumbs = _.clone(this.crumbs);
      return {
        crumbs: crumbs
      };
    },

    initialize: function(options) {
      this.crumbs = options.crumbs;
    }
  });

  Fauxton.Footer = Backbone.View.extend({
    template: "templates/fauxton/footer",
    serialize: function() {
      return {
        version: app.version
      };
    }
  });

  Fauxton.NavBar = Backbone.View.extend({
    template: "templates/fauxton/nav_bar",
    // TODO: can we generate this list from the router?
    navLinks: [
      {href:"#/_all_dbs", title:"Databases"}
    ],

    initialize: function() {
      this.on("link:add", this.render, this);
    },

    serialize: function() {
      return {navLinks: this.navLinks};
    },

    addLink: function(link) {
      if (link.top){
        this.navLinks.unshift(link);
      } else {
        this.navLinks.push(link);
      }
      this.trigger("link:add");

      this.render();
    },

    beforeRender: function () {
      this.addLinkViews();
    },

    addLinkViews: function () {
      var self = this;

      _.each(this.navLinks, function (link) {
        if (!link.view) { return; }

        //TODO check if establish is a function
        var establish = link.establish || [];
        $.when.apply(null, establish).done( function () {
          self.insertView('#nav-links', link.view).render();
        });
      }, this);
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

  Fauxton.Pagination = Backbone.View.extend({
    template: "templates/fauxton/pagination",

    initialize: function(options) {
      this.page = options.page;
      this.perPage = options.perPage;
      this.total = options.total;
      this.totalPages = Math.ceil(this.total / this.perPage);
      this.urlFun = options.urlFun;
      this.routeEvent = options.routeEvent;

    },

    serialize: function() {
      return {
        page: this.page,
        perPage: this.perPage,
        total: this.total,
        totalPages: this.totalPages,
        urlFun: this.urlFun,
        routeEvent: this.routeEvent
      };
    }
  });

  return Fauxton;
});
