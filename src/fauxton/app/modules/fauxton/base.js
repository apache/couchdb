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
       "backbone",
       "resizeColumns"

],

function(app, Backbone, resizeColumns) {


   //resizeAnimation
   app.resizeColumns = new resizeColumns({});
   app.resizeColumns.onResizeHandler();

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

  Fauxton.VersionInfo = Backbone.Model.extend({
    url: app.host
  });

  // TODO: this View should extend from FauxtonApi.View.
  // Chicken and egg problem, api.js extends fauxton/base.js.
  // Need to sort the loading order.
  Fauxton.Footer = Backbone.View.extend({
    template: "templates/fauxton/footer",

    initialize: function() {
      this.versionInfo = new Fauxton.VersionInfo();
    },

    establish: function() {
      return [this.versionInfo.fetch()];
    },

    serialize: function() {
      return {
        version: this.versionInfo.get("version")
      };
    }
  });

  Fauxton.NavBar = Backbone.View.extend({
    className:"navbar",
    template: "templates/fauxton/nav_bar",
    // TODO: can we generate this list from the router?
    navLinks: [
      {href:"#/_all_dbs", title:"Databases", icon: "fonticon-database", className: 'databases'}
    ],

    bottomNavLinks: [],
    footerNavLinks: [],

    serialize: function() {
      return {
        navLinks: this.navLinks,
        bottomNavLinks: this.bottomNavLinks,
        footerNavLinks: this.footerNavLinks
      };
    },

    addLink: function(link) {
      // link.top means it gets pushed to the top of the array,
      // link.bottomNav means it goes to the additional bottom nav
      // link.footerNav means goes to the footer nav
      if (link.top && !link.bottomNav){
        this.navLinks.unshift(link);
      } else if (link.top && link.bottomNav){
        this.bottomNavLinks.unshift(link);
      } else if (link.bottomNav) {
        this.bottomNavLinks.push(link);
      } else if (link.footerNav) {
        this.footerNavLinks.push(link);
      } else {
        this.navLinks.push(link);
      }

      //this.trigger("link:add");

      //this.render();
    },

    removeLink: function (removeLink) {
      var links = this.navlinks;

      if (removeLink.bottomNav) {
        links = this.bottomNavLinks;
      } else if (removeLink.footerNav) {
        links = this.footerNavLinks;
      }

      var foundIndex = -1;

      _.each(links, function (link, index) {
        if (link.title === removeLink.title) {
          foundIndex = index;
        }
      });

      if (foundIndex === -1) {return;}
      links.splice(foundIndex, 1);
      this.render();
    },

    afterRender: function(){

      $('#primary-navbar li[data-nav-name="' + app.selectedHeader + '"]').addClass('active');

      var menuOpen = true;
      var $selectorList = $('body');
      $('.brand').off();
      $('.brand').on({
        click: function(e){
          if(!$(e.target).is('a')){
            toggleMenu();
          }
         }
      });

      function toggleMenu(){
        $selectorList.toggleClass('closeMenu');
        menuOpen = $selectorList.hasClass('closeMenu');
        app.resizeColumns.onResizeHandler();
      }

      $('#primary-navbar').on("click", ".nav a", function(){
        if (!($selectorList.hasClass('closeMenu'))){
        setTimeout(
          function(){
            $selectorList.addClass('closeMenu');
            app.resizeColumns.onResizeHandler();
          },3000);

        }
      });

      app.resizeColumns.initialize();
    },

    beforeRender: function () {
      this.addLinkViews();
    },

    addLinkViews: function () {
      var that = this;

      _.each(_.union(this.navLinks, this.bottomNavLinks), function (link) {
        if (!link.view) { return; }

        //TODO check if establish is a function
        var establish = link.establish || [];
        $.when.apply(null, establish).then( function () {
          var selector =  link.bottomNav ? '#bottom-nav-links' : '#nav-links';
          that.insertView(selector, link.view).render();
        });
      }, this);
    }

    // TODO: ADD ACTIVE CLASS
  });

  Fauxton.ApiBar = Backbone.View.extend({
    template: "templates/fauxton/api_bar",
    endpoint: '_all_docs',

    events:  {
      "click .api-url-btn" : "toggleAPIbar"
    },

    toggleAPIbar: function(e){
      var $currentTarget = $(e.currentTarget).find('span');
      if ($currentTarget.hasClass("fonticon-plus")){
        $currentTarget.removeClass("fonticon-plus").addClass("fonticon-minus");
      }else{
        $currentTarget.removeClass("fonticon-minus").addClass("fonticon-plus");
      }

      $('.api-navbar').toggle();

    },

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
    fadeTimer: 5000,

    initialize: function(options) {
      this.msg = options.msg;
      this.type = options.type || "info";
      this.selector = options.selector;
      this.fade = options.fade === undefined ? true : options.fade;
      this.clear = options.clear;
      this.data = options.data || "";
      this.template = options.template || "templates/fauxton/notification";
    },

    serialize: function() {
      return {
        data: this.data,
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
      this.render().$el.appendTo(selector);
      this.delayedFade();
      return this;
    }
  });

  Fauxton.Pagination = Backbone.View.extend({
    template: "templates/fauxton/pagination",

    initialize: function(options) {
      this.page = parseInt(options.page, 10);
      this.perPage = options.perPage;
      this.total = options.total;
      this.totalPages = Math.ceil(this.total / this.perPage);
      this.urlFun = options.urlFun;
    },

    serialize: function() {
      return {
        page: this.page,
        perPage: this.perPage,
        total: this.total,
        totalPages: this.totalPages,
        urlFun: this.urlFun
      };
    }
  });

  return Fauxton;
});
