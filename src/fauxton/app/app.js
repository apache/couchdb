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
  "core/utils",
  // Modules
  "core/api",
  "core/couchdbSession",
  // Plugins.
  "plugins/backbone.layoutmanager",
  "plugins/jquery.form"

],

function(app, $, _, Backbone, Bootstrap, Helpers, Utils, FauxtonAPI, Couchdb) {
  // Make sure we have a console.log
  if (typeof console == "undefined") {
    console = {
      log: function(){},
      trace: function(){},
      debug: function(){}
    };
  }

  // Provide a global location to place configuration settings and module
  // creation also mix in Backbone.Events
  _.extend(app, {
    utils: Utils,
    getParams: FauxtonAPI.utils.getParams
  });

  // Localize or create a new JavaScript Template object.
  var JST = window.JST = window.JST || {};

  // Configure LayoutManager with Backbone Boilerplate defaults.
  FauxtonAPI.Layout.configure({
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

  FauxtonAPI.setSession(new Couchdb.Session());

  // Define your master router on the application namespace and trigger all
  // navigation from this instance.
  FauxtonAPI.config({
    el: "#app-container",
    masterLayout: new FauxtonAPI.Layout(),
    
    addHeaderLink: function(link) {
      FauxtonAPI.registerExtension('navbar:addHeaderLink', link);
    },

    removeHeaderLink: function(link) {
      // TODO add remove to extensions
    }
  });

  return app;
});
