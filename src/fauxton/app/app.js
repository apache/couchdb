define([
  // Libraries.
  "jquery",
  "lodash",
  "backbone",

  // Plugins.
  "plugins/backbone.layoutmanager"
],

function($, _, Backbone) {

  // Provide a global location to place configuration settings and module
  // creation.
  var app = {
    // The root path to run the application.
    root: "/"
  };

  // Localize or create a new JavaScript Template object.
  var JST = window.JST = window.JST || {};

  // Configure LayoutManager with Backbone Boilerplate defaults.
  Backbone.LayoutManager.configure({
    // Allow LayoutManager to augment Backbone.View.prototype.
    manage: true,

    prefix: "app/",

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
