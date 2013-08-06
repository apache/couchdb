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
    mixins: Mixins
  };

  // Localize or create a new JavaScript Template object.
  var JST = window.JST = window.JST || {};

  // Configure LayoutManager with Backbone Boilerplate defaults.
  Backbone.Layout.configure({
    // Allow LayoutManager to augment Backbone.View.prototype.
    manage: true,

    prefix: "app/",

    // Inject app/helper.js for shared functionality across all html templates
    render: function(template, context) {
      return template(_.extend(Helpers, context));
    },

    fetch: function(path) {
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
