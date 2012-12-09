define([
  "app",

  "api",

  // Modules
  "addons/config/resources"
],

function(app, FauxtonAPI, Config) {
  var config = function () {
    var configs = new Config.Collection();
    var deferred = FauxtonAPI.Deferred();

    return {
      layout: "one_pane",
      crumbs: [
        {"name": "Home","link": app.root},
        {"name": "Config","link": app.root}
      ],
      views: {
        "#dashboard-content": new Config.View({collection: configs})
      },
      apiUrl: configs.url(),
      establish: function() {
        configs.fetch().done(function(resp) {
          deferred.resolve();
        });
        return [deferred];
      }
    };
  };

  Config.Routes = {
    "_config": "config"
  };

  return Config;

});