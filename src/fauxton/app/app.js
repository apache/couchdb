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
  // Libraries.
  "jquery",
  "lodash",
  "backbone",

  "helpers",
  "mixins",

  // Plugins.
  "plugins/backbone.layoutmanager",
  "plugins/jquery.form"
],

function($, _, Backbone, Helpers, Mixins) {

  // Make sure we have a console.log
  if (typeof console == "undefined") {
    console = {
      log: function(){}
    };
  }

  // Provide a global location to place configuration settings and module
  // creation.
  var app = {
    // The root path to run the application.
    root: "/",
    version: "0.0.1",
    mixins: Mixins,
    // move this to here otherwise every once in a while,
    // the footer fails to configure as the url for it is not configured.
    // Having the host declared here fixes it
    host: window.location.protocol + "//" + window.location.host,
  };

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

  // Mix Backbone.Events, and modules into the app object.
  return _.extend(app, {
    // Create a custom object with a nested Views object.
    module: function(additionalProps) {
      return _.extend({ Views: {} }, additionalProps);
    }
  }, Backbone.Events);

});
