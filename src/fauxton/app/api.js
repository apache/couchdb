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
       "addons/fauxton/base",
       "spin"
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
    "Missing name in function declaration.",
    "['{a}'] is better written in dot notation."
  ];

  FauxtonAPI.isIgnorableError = function(msg) {
    return _.contains(FauxtonAPI.excludedViewErrors, msg);
  };

  FauxtonAPI.View = Backbone.View.extend({
    // This should return an array of promises, an empty array, or null
    establish: function() {
      return null;
    },

    loaderClassname: 'loader',

    disableLoader: false,

    forceRender: function () {
      this.hasRendered = false;
    }
  });

  FauxtonAPI.navigate = function(url, _opts) {
    var options = _.extend({trigger: true}, _opts );
    app.router.navigate(url,options);
  };

  FauxtonAPI.beforeUnload = function () {
    app.router.beforeUnload.apply(app.router, arguments);
  };

  FauxtonAPI.removeBeforeUnload = function () {
    app.router.removeBeforeUnload.apply(app.router, arguments);
  };

  FauxtonAPI.addHeaderLink = function(link) {
    app.masterLayout.navBar.addLink(link);
  };

  FauxtonAPI.removeHeaderLink = function(link) {
    app.masterLayout.navBar.removeLink(link);
  };

  FauxtonAPI.Deferred = function() {
    return $.Deferred();
  };

  FauxtonAPI.when = function (deferreds) {
    if (deferreds instanceof Array) {
      return $.when.apply(null, deferreds);
    }

    return $.when(deferreds);
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

  FauxtonAPI.Session = Backbone.Model.extend({
    url: app.host + '/_session',

    user: function () {
      var userCtx = this.get('userCtx');

      if (!userCtx || !userCtx.name) { return null; }

      return {
        name: userCtx.name,
        roles: userCtx.roles
      };
    },

    fetchOnce: function (opt) {
      var options = _.extend({}, opt);

      if (!this._deferred || this._deferred.state() === "rejected" || options.forceFetch ) {
        this._deferred = this.fetch();
      }

      return this._deferred;
    },

    fetchUser: function (opt) {
      var that = this,
      currentUser = this.user();

      return this.fetchOnce(opt).then(function () {
        var user = that.user();

        // Notify anyone listening on these events that either a user has changed
        // or current user is the same
        if (currentUser !== user) {
          that.trigger('session:userChanged');
        } else {
          that.trigger('session:userFetched');
        }

        // this will return the user as a value to all function that calls done on this
        // eg. session.fetchUser().done(user) { .. do something with user ..}
        return user; 
      });
    }
  });

  FauxtonAPI.setSession = function (newSession) {
    app.session = FauxtonAPI.session = newSession;
    return FauxtonAPI.session.fetchUser();
  };

  FauxtonAPI.setSession(new FauxtonAPI.Session());

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
      var that = this;
    },

    authHandlerCb : function (roles) {
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
      that = this;

      return FauxtonAPI.session.fetchUser().then(function (user) {
        return FauxtonAPI.when(that.authHandlerCb(FauxtonAPI.session, requiredRoles));
      });
    }
  });

  FauxtonAPI.auth = new Auth();

  FauxtonAPI.RouteObject = function(options) {
    this._options = options;

    this._configure(options || {});
    this.initialize.apply(this, arguments);
    this.addEvents();
  };

  var broadcaster = {};
  _.extend(broadcaster, Backbone.Events);

  FauxtonAPI.RouteObject.on = function (eventName, fn) {
    broadcaster.on(eventName, fn); 
  };
  
  /* How Route Object events work
   To listen to a specific route objects events:

   myRouteObject = FauxtonAPI.RouteObject.extend({
    events: {
      "beforeRender": "beforeRenderEvent"
    },

    beforeRenderEvent: function (view, selector) {
      console.log('Hey, beforeRenderEvent triggered',arguments);
    },
   });

    It is also possible to listen to events triggered from all Routeobjects. 
    This is great for more general things like adding loaders, hooks.

    FauxtonAPI.RouteObject.on('beforeRender', function (routeObject, view, selector) {
      console.log('hey, this will trigger when any routeobject renders a view');
    });

   Current Events to subscribe to:
    * beforeFullRender -- before a full render is being done
    * beforeEstablish -- before the routeobject calls establish
    * AfterEstablish -- after the routeobject has run establish
    * beforeRender -- before a view is rendered
    * afterRender -- a view is finished being rendered
    * renderComplete -- all rendering is complete
    
  */

  // Piggy-back on Backbone's self-propagating extend function
  FauxtonAPI.RouteObject.extend = Backbone.Model.extend;

  var routeObjectOptions = ["views", "routes", "events", "roles", "crumbs", "layout", "apiUrl", "establish"];

  _.extend(FauxtonAPI.RouteObject.prototype, Backbone.Events, {
    // Should these be default vals or empty funcs?
    views: {},
    routes: {},
    events: {},
    crumbs: [],
    layout: "with_sidebar",
    apiUrl: null,
    disableLoader: false,
    loaderClassname: 'loader',
    renderedState: false,
    establish: function() {},
    route: function() {},
    roles: [],
    initialize: function() {}
  }, {

    renderWith: function(route, masterLayout, args) {
      var routeObject = this,
          triggerBroadcast = _.bind(this.triggerBroadcast, this);

      // Only want to redo the template if its a full render
      if (!this.renderedState) {
        masterLayout.setTemplate(this.layout);
        triggerBroadcast('beforeFullRender');
        $('#primary-navbar li').removeClass('active');

        if (this.selectedHeader) {
          app.selectedHeader = this.selectedHeader;
          $('#primary-navbar li[data-nav-name="' + this.selectedHeader + '"]').addClass('active');
        }
      }

      masterLayout.clearBreadcrumbs();
      var crumbs = this.get('crumbs');

      if (crumbs.length) {
        masterLayout.setBreadcrumbs(new Fauxton.Breadcrumbs({
          crumbs: crumbs
        }));
      }

      triggerBroadcast('beforeEstablish');
      FauxtonAPI.when(this.establish()).then(function(resp) {
        triggerBroadcast('afterEstablish');
        _.each(routeObject.getViews(), function(view, selector) {
          if(view.hasRendered) { 
            triggerBroadcast('viewHasRendered', view, selector);
            return;
          }

          triggerBroadcast('beforeRender', view, selector);
          FauxtonAPI.when(view.establish()).then(function(resp) {
            masterLayout.setView(selector, view);

            masterLayout.renderView(selector);
            triggerBroadcast('afterRender', view, selector);
            }, function(resp) {
              view.establishError = {
                error: true,
                reason: resp
              };

              if (resp) { 
                var errorText = JSON.parse(resp.responseText).reason;
                FauxtonAPI.addNotification({
                  msg: 'An Error occurred: ' + errorText,
                  type: 'error',
                  clear: true
                });
              }

              masterLayout.renderView(selector);
          });

        });
      }.bind(this), function (resp) {
          if (!resp) { return; }
          FauxtonAPI.addNotification({
                msg: 'An Error occurred' + JSON.parse(resp.responseText).reason,
                type: 'error',
                clear: true
          });
      });

      if (this.get('apiUrl')){
        masterLayout.apiBar.update(this.get('apiUrl'));
      } else {
        masterLayout.apiBar.hide();
      }

      // Track that we've done a full initial render
      this.renderedState = true;
      triggerBroadcast('renderComplete');
    },

    triggerBroadcast: function (eventName) {
      var args = Array.prototype.slice.call(arguments);
      this.trigger.apply(this, args);

      args.splice(0,1, eventName, this);
      broadcaster.trigger.apply(broadcaster, args);
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

    removeViews: function () {
      _.each(this.views, function (view, selector) {
        view.remove();
        delete this.views[selector];
      }, this);
    },

    getRouteUrls: function () {
      return _.keys(this.get('routes'));
    },

    hasRoute: function (route) {
      if (this.get('routes')[route]) {
        return true;
      }
      return false;
    },

    routeCallback: function (route, args) {
      var routes = this.get('routes'),
      routeObj = routes[route],
      routeCallback;

      if (typeof routeObj === 'object') {
        routeCallback = this[routeObj.route];
      } else {
        routeCallback = this[routeObj];
      }

      routeCallback.apply(this, args);
    },

    getRouteRoles: function (routeUrl) {
      var route = this.get('routes')[routeUrl];

      if ((typeof route === 'object') && route.roles) {
        return route.roles; 
      }

      return this.roles;
    }

  });

  // We could look at moving the spinner code out to its own module
  var routeObjectSpinner;
  FauxtonAPI.RouteObject.on('beforeEstablish', function (routeObject) {
    if (!routeObject.disableLoader){ 
      var opts = {
        lines: 16, // The number of lines to draw
        length: 8, // The length of each line
        width: 4, // The line thickness
        radius: 12, // The radius of the inner circle
        color: '#333', // #rbg or #rrggbb
        speed: 1, // Rounds per second
        trail: 10, // Afterglow percentage
        shadow: false // Whether to render a shadow
     };

     if (!$('.spinner').length) {
       $('<div class="spinner"></div>')
        .appendTo('#app-container');
     }

     routeObjectSpinner = new Spinner(opts).spin();
     $('.spinner').append(routeObjectSpinner.el);
   }
  });

  var removeRouteObjectSpinner = function () {
    if (routeObjectSpinner) {
      routeObjectSpinner.stop();
      $('.spinner').remove();
    }
  };

  var removeViewSpinner = function () {
    if (viewSpinner){
      viewSpinner.stop();
      $('.spinner').remove();
    }
  };

  var viewSpinner;
  FauxtonAPI.RouteObject.on('beforeRender', function (routeObject, view, selector) {
    removeRouteObjectSpinner();

    if (!view.disableLoader){ 
      var opts = {
        lines: 16, // The number of lines to draw
        length: 8, // The length of each line
        width: 4, // The line thickness
        radius: 12, // The radius of the inner circle
        color: '#333', // #rbg or #rrggbb
        speed: 1, // Rounds per second
        trail: 10, // Afterglow percentage
        shadow: false // Whether to render a shadow
      };

      viewSpinner = new Spinner(opts).spin();
      $('<div class="spinner"></div>')
        .appendTo(selector)
        .append(viewSpinner.el);
    }
  });

  FauxtonAPI.RouteObject.on('afterRender', function (routeObject, view, selector) {
    removeViewSpinner();
  });

  FauxtonAPI.RouteObject.on('viewHasRendered', function () {
    removeViewSpinner();
    removeRouteObjectSpinner();
  });

  var extensions = _.extend({}, Backbone.Events);
  // Can look at a remove function later.
  FauxtonAPI.registerExtension = function (name, view) {
    if (!extensions[name]) {
      extensions[name] = [];
    }

    extensions.trigger('add:' + name, view);
    extensions[name].push(view);
  };

  FauxtonAPI.getExtensions = function (name) {
    var views = extensions[name];

    if (!views) {
      views = [];
    }

    return views;
  };

  FauxtonAPI.extensions = extensions;

  app.fauxtonAPI = FauxtonAPI;
  return app.fauxtonAPI;
});
