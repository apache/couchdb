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
  // Application.
  "initialize",

  // Libraries
  "jquery",
  "lodash",
  "backbone",
  "bootstrap",

  "helpers",
  "utils",
  // Modules
  "resizeColumns",
  "core/api",
  "addons/fauxton/base",
   // Plugins.
  "plugins/backbone.layoutmanager",
  "plugins/jquery.form"

],

function(app, $, _, Backbone, Bootstrap, Helpers, Utils, resizeColumns, FauxtonAPI, Fauxton) {

   // Make sure we have a console.log
  if (typeof console == "undefined") {
    console = {
      log: function(){}
    };
  }

  // Provide a global location to place configuration settings and module
  // creation also mix in Backbone.Events
  _.extend(app, Backbone.Events, {
    utils: Utils,

    // Thanks to: http://stackoverflow.com/a/2880929
    getParams: function(queryString) {
      if (queryString) {
        // I think this could be combined into one if
        if (queryString.substring(0,1) === "?") {
          queryString = queryString.substring(1);
        } else if (queryString.indexOf('?') > -1) {
          queryString = queryString.split('?')[1];
        }
      }
      var hash = window.location.hash.split('?')[1];
      queryString = queryString || hash || window.location.search.substring(1);
      var match,
      urlParams = {},
      pl     = /\+/g,  // Regex for replacing addition symbol with a space
      search = /([^&=]+)=?([^&]*)/g,
      decode = function (s) { return decodeURIComponent(s.replace(pl, " ")); },
      query  = queryString;

      if (queryString) {
        while ((match = search.exec(query))) {
          urlParams[decode(match[1])] = decode(match[2]);
        }
      }

      return urlParams;
    }

  });

  //MOVE THIS ELSEWHERE
  app.footer = new Fauxton.Footer({el: "#footer-content"});
  // TODO: move this to a proper Fauxton.View
  $.when.apply(null, app.footer.establish()).done(function() {
    app.footer.render();
  });
  app.navBar = new Fauxton.NavBar();
  app.apiBar = new Fauxton.ApiBar();
  // Define your master router on the application namespace and trigger all
  // navigation from this instance.
  app.router = new FauxtonAPI.Router();
  FauxtonAPI.config({
    router: app.router,
    masterLayout: new Layout(app.navBar, app.apiBar),
    addNotification: function (options) {
      options = _.extend({
        msg: "Notification Event Triggered!",
        type: "info",
        selector: "#global-notifications"
      }, options);

      var view = new Fauxton.Notification(options);
      return view.renderNotification();
    }
  });
  // Trigger the initial route and enable HTML5 History API support, set the
  // root folder to '/' by default.  Change in app.js.
  Backbone.history.start({ pushState: false, root: app.root });

  // Localize or create a new JavaScript Template object.
  var JST = window.JST = window.JST || {};

  // Configure LayoutManager with Backbone Boilerplate defaults.
  Backbone.Layout.configure({
    // Allow LayoutManager to augment Backbone.View.prototype.
    manage: true,

    prefix: "app/",

    // Inject app/helper.js for shared functionality across all html templates
    renderTemplate: function(template, context) {
      return template(_.extend(Helpers, context));
    },

    fetchTemplate: function(path) {
      // Initialize done for use in async-mode
      var done;

      // Concatenate the file extension.
      path = path + ".html";

      // If cached, use the compiled template.
      if (JST[path]) {
        return JST[path];
      } else {
        // Put fetch into `async-mode`.
        done = this.async();
        // Seek out the template asynchronously.
        return $.ajax({ url: app.root + path }).then(function(contents) {
          done(JST[path] = _.template(contents));
        });
      }
    }
  });


  return app;
});
