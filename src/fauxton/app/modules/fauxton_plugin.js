define([
  "app",

  // Libs
  "backbone"

  // Modules

  // Views

  // Plugins
],

function(app, Backbone) {
  var Plugin = app.module();

  Plugin.addHeaderLink = function(link) {
    app.navBar.addLink(link);
  };

  Plugin.addRoute = function(route) {
    app.router.route(route.route, route.name, route.callback);
  };

  Plugin.module = function() {
    return app.module();
  };

  return Plugin;
});
