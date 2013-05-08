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

  // Modules
  "modules/fauxton/base"
],

function(app, Fauxton) {
  var FauxtonAPI = app.module();

  FauxtonAPI.moduleExtensions = {
    Routes: {
    }
  };

  FauxtonAPI.addonExtensions = {
    initialize: function() {}
  };

  // List of JSHINT errors to ignore
  // Gets around problem of anonymous functions not being a valid statement
  FauxtonAPI.excludedViewErrors = [
    "Missing name in function declaration."
  ];

  FauxtonAPI.isIgnorableError = function(msg) {
    return _.contains(FauxtonAPI.excludedViewErrors, msg);
  };

  FauxtonAPI.View = Backbone.View.extend({
    // This should return an array of promises, an empty array, or null
    establish: function() {
      return null;
    },

    hasRendered: function () {
      return !!this.__manager__.hasRendered;
    },

    reRender: function () {
      this.__manager__.hasRendered = false;
    }
  });

  FauxtonAPI.navigate = function(url) {
    Backbone.history.navigate(url, true);
  };

  FauxtonAPI.addHeaderLink = function(link) {
    app.masterLayout.navBar.addLink(link);
  };

  FauxtonAPI.Deferred = function() {
    return $.Deferred();
  };

  FauxtonAPI.addRoute = function(route) {
    app.router.route(route.route, route.name, route.callback);
  };

  FauxtonAPI.triggerRouteEvent = function (routeEvent, args) {
    app.router.triggerRouteEvent("route:"+routeEvent, args);
  };

  FauxtonAPI.module = function(extra) {
    return app.module(_.extend(FauxtonAPI.moduleExtensions, extra));
  };

  FauxtonAPI.addon = function(extra) {
    return FauxtonAPI.module(FauxtonAPI.addonExtensions, extra);
  };

  FauxtonAPI.addNotification = function(options) {
    options = _.extend({
      msg: "Notification Event Triggered!",
      type: "info",
      selector: "#global-notifications"
    }, options);
    var view = new Fauxton.Notification(options);

    return view.renderNotification();
  };

  FauxtonAPI.UUID = Backbone.Model.extend({
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

  // This is not exposed externally as it should not need to be accessed or overridden
  var Auth = function (options) {
    this._options = options;
    this.initialize.apply(this, arguments);
  };

  // Piggy-back on Backbone's self-propagating extend function,
  Auth.extend = Backbone.Model.extend;

  _.extend(Auth.prototype, Backbone.Events, {
    authDeniedCb: function() {},

    initialize: function() {
      var self = this;

      $(document).ajaxError(function(event, jqxhr, settings, exception) {
        console.log("UNAUTH");
        console.log(arguments);
        if (exception === "Unauthorized" || exception === "Forbidden") {
          self.authDeniedCb();
        }
      });
    },

    authHandlerCb : function (roles, layout) {
      var deferred = $.Deferred();
      deferred.resolve();
      return deferred;
    },

    registerAuth: function (authHandlerCb) {
      this.authHandlerCb = authHandlerCb;
    },

    registerAuthDenied: function (authDeniedCb) {
      this.authDeniedCb = authDeniedCb;
    },

    checkAccess: function (roles) {
      var requiredRoles = roles || [],
          authDeniedCb = this.authDeniedCb,
          promise = $.when.apply(null, this.authHandlerCb(requiredRoles));

      promise.fail(function () { authDeniedCb();});

      return promise;
    }
  });

  FauxtonAPI.auth = new Auth();

  FauxtonAPI.RouteObject = function(options) {
    this._options = options;

    this._configure(options || {});
    this.initialize.apply(this, arguments);
    this.addEvents();
  };

  // Piggy-back on Backbone's self-propagating extend function
  FauxtonAPI.RouteObject.extend = Backbone.Model.extend;

  var routeObjectOptions = ["views", "routes", "events", "data", "crumbs", "layout", "apiUrl", "establish"];

  _.extend(FauxtonAPI.RouteObject.prototype, Backbone.Events, {
    // Should these be default vals or empty funcs?
    views: {},
    routes: {},
    events: {},
    data: {},
    crumbs: [],
    layout: "with_sidebar",
    apiUrl: null,
    renderedState: false,
    establish: function() {},
    route: function() {},
    roles: [],
    initialize: function() {}
  }, {

    // TODO:: combine this and the renderWith function
    // All the things should go through establish, as it will resolve
    // immediately if its already done, but this way the RouteObject.route
    // function can rebuild the deferred as needed
    render: function(route, masterLayout, args) {
      this.renderWith.apply(this, Array.prototype.slice.call(arguments));
    },

    renderWith: function(route, masterLayout, args) {
      var routeObject = this;

      // Can look at replacing this with events eg beforeRender, afterRender
      this.route.call(this, route, args);

      // Only want to redo the template if its a full render
      if (!this.renderedState) {
        masterLayout.setTemplate(this.layout);
      }

      masterLayout.clearBreadcrumbs();
      var crumbs = this.get('crumbs');

      if (crumbs.length) {
        masterLayout.setBreadcrumbs(new Fauxton.Breadcrumbs({
          crumbs: crumbs
        }));
      }

      $.when.apply(this, this.establish()).done(function(resp) {
        _.each(routeObject.getViews(), function(view, selector) {
          if(view.hasRendered()) { return; }

          masterLayout.setView(selector, view);
          console.log('SET SELECTOR AND RENDER ', selector, view); 

          $.when.apply(null, view.establish()).then(function(resp) {
            masterLayout.renderView(selector);
          }, function(resp) {
            view.establishError = {
              error: true,
              reason: resp
            };
            masterLayout.renderView(selector);
          });

          var hooks = masterLayout.hooks[selector];

          _.each(hooks, function(hook){
            if (_.any(hook.routes, function(route){return route == boundRoute;})){
              hook.callback(view);
            }
          });
        });
      });

      if (this.get('apiUrl')) masterLayout.apiBar.update(this.get('apiUrl'));

      // Track that we've done a full initial render
      this.renderedState = true;
    },

    get: function(key) {
      return _.isFunction(this[key]) ? this[key]() : this[key];
    },

    addEvents: function(events) {
      events = events || this.get('events');
      _.each(events, function(method, event) {
        if (!_.isFunction(method) && !_.isFunction(this[method])) {
          throw new Error("Invalid method: "+method);
        }
        method = _.isFunction(method) ? method : this[method];

        this.on(event, method);
      }, this);
    },

    _configure: function(options) {
      _.each(_.intersection(_.keys(options), routeObjectOptions), function(key) {
        this[key] = options[key];
      }, this);
    },

    getView: function(selector) {
      return this.views[selector];
    },

    setView: function(selector, view) {
      this.views[selector] = view;
      return view;
    },

    getViews: function() {
      return this.views;
    },

    // Could move getRouteUrls into the Constructor function and so it defines the urls
    // only once. This would give us a small speed up.
    getRouteUrls: function () {
      return _.keys(this.get('routes'));
    },

    hasRoute: function (route) {
      if (this.getRouteUrls().indexOf(route) > -1) {
        return true;
      }

      return false;
    },

    routeCallback: function (route) {
      var routes = this.get('routes');
      var routeObj = routes[route];

      if (typeof routeObj === 'object') {
        return this[routeObj.route];
      } else {
        return this[routeObj];
      }
    },

    getRouteRoles: function (routeUrl) {
      var route = this.get('routes')[routeUrl];

      if ((typeof route === 'object') && route.roles) {
       return route.roles; 
      }

      return this.roles;
    }

  });

  app.fauxtonAPI = FauxtonAPI;
  return app.fauxtonAPI;
});
