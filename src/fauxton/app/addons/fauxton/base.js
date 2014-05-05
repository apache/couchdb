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
  "addons/fauxton/resizeColumns"
],

function(app, FauxtonAPI, resizeColumns) {

  var Fauxton = FauxtonAPI.addon();
  FauxtonAPI.addNotification = function (options) {
    options = _.extend({
      msg: "Notification Event Triggered!",
      type: "info",
      selector: "#global-notifications",
      escape: true
    }, options);

    var view = new Fauxton.Notification(options);
    return view.renderNotification();
  };

  FauxtonAPI.UUID = FauxtonAPI.Model.extend({
    initialize: function(options) {
      options = _.extend({count: 1}, options);
      this.count = options.count;
    },

    url: function() {
      return app.host + "/_uuids?count=" + this.count;
    },

    next: function() {
      return this.get("uuids").pop();
    }
  });

  Fauxton.initialize = function () {
    // app.footer = new Fauxton.Footer({el: "#footer-content"}),
    app.navBar = new Fauxton.NavBar();
    app.apiBar = new Fauxton.ApiBar();

    FauxtonAPI.when.apply(null, app.navBar.establish()).done(function() {
      FauxtonAPI.masterLayout.setView("#primary-navbar", app.navBar, true);
      FauxtonAPI.masterLayout.setView("#api-navbar", app.apiBar, true);
      app.navBar.render();
      app.apiBar.render();
    });

    FauxtonAPI.masterLayout.navBar = app.navBar;
    FauxtonAPI.masterLayout.apiBar = app.apiBar;

    FauxtonAPI.RouteObject.on('beforeFullRender', function (routeObject) {
      $('#primary-navbar li').removeClass('active');

      if (routeObject.selectedHeader) {
        app.selectedHeader = routeObject.selectedHeader;
        $('#primary-navbar li[data-nav-name="' + routeObject.selectedHeader + '"]').addClass('active');
      }
    });

    FauxtonAPI.RouteObject.on('beforeEstablish', function (routeObject) {
      FauxtonAPI.masterLayout.removeView('#breadcrumbs');
      var crumbs = routeObject.get('crumbs');

      if (crumbs.length) {
        FauxtonAPI.masterLayout.setView('#breadcrumbs', new Fauxton.Breadcrumbs({
          crumbs: crumbs
        }), true).render();
      }
    });

    FauxtonAPI.RouteObject.on('renderComplete', function (routeObject) {
      var masterLayout = FauxtonAPI.masterLayout;
      if (routeObject.get('apiUrl')){
        masterLayout.apiBar.update(routeObject.get('apiUrl'));
      } else {
        masterLayout.apiBar.hide();
      }
    });
  };

  Fauxton.Breadcrumbs = FauxtonAPI.View.extend({
    template: "addons/fauxton/templates/breadcrumbs",

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
    url: function () {
      return app.host;
    }
  });

  Fauxton.Footer = FauxtonAPI.View.extend({
    tagName: "p",
    template: "addons/fauxton/templates/footer",

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

  Fauxton.NavBar = FauxtonAPI.View.extend({
    className:"navbar",
    template: "addons/fauxton/templates/nav_bar",

    events:  {
      "click .burger" : "toggleMenu"
    },

    toggleMenu: function(){
       var $selectorList = $('body');
       $selectorList.toggleClass('closeMenu');
       this.resizeColumns.onResizeHandler();
    },

    // TODO: can we generate this list from the router?
    navLinks: [
      {href:"#/_all_dbs", title:"Databases", icon: "fonticon-database", className: 'databases'}
    ],

    bottomNavLinks: [],
    footerNavLinks: [],

    initialize: function () {
      _.bindAll(this);
      //resizeAnimation
      app.resizeColumns = this.resizeColumns = new resizeColumns({});
      this.resizeColumns.onResizeHandler();

      FauxtonAPI.extensions.on('add:navbar:addHeaderLink', this.addLink);
      FauxtonAPI.extensions.on('removeItem:navbar:addHeaderLink', this.removeLink);
      this.versionFooter = new Fauxton.Footer({});
    },

    serialize: function() {
      return {
        navLinks: this.navLinks,
        bottomNavLinks: this.bottomNavLinks,
        footerNavLinks: this.footerNavLinks
      };
    },

    establish: function(){
      return [this.versionFooter.establish()];
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

      var $selectorList = $('body');
      var that = this;
      $('#primary-navbar').on("click", ".nav a", function(){
        if (!($selectorList.hasClass('closeMenu'))){
          setTimeout(
            function(){
            $selectorList.addClass('closeMenu');
            that.resizeColumns.onResizeHandler();
          },3000);

        }
      });

      this.resizeColumns.initialize();
    },

    beforeRender: function () {
      this.insertView(".version", this.versionFooter);
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

  Fauxton.ApiBar = FauxtonAPI.View.extend({
    template: "addons/fauxton/templates/api_bar",
    endpoint: '_all_docs',

    documentation: 'docs',

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
      return {
        endpoint: this.endpoint,
        documentation: this.documentation
      };
    },

    hide: function(){
      this.$el.addClass('hide');
    },
    show: function(){
      this.$el.removeClass('hide');
    },
    update: function(endpoint) {
      this.show();
      this.endpoint = endpoint[0];
      this.documentation = endpoint[1];
      this.render();
    }

  });

  Fauxton.Notification = FauxtonAPI.View.extend({
    fadeTimer: 5000,

    initialize: function(options) {
      this.htmlToRender = options.msg;
      // escape always, except the value is false
      if (options.escape !== false) {
        this.htmlToRender = _.escape(this.htmlToRender);
      }
      this.type = options.type || "info";
      this.selector = options.selector;
      this.fade = options.fade === undefined ? true : options.fade;
      this.clear = options.clear;
      this.data = options.data || "";
      this.template = options.template || "addons/fauxton/templates/notification";
    },

    serialize: function() {
      return {
        data: this.data,
        htmlToRender: this.htmlToRender,
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

  return Fauxton;
});
