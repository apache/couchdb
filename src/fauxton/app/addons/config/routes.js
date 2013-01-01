define([
  "app",

  "api",

  // Modules
  "addons/config/resources"
],

function(app, FauxtonAPI, Config) {
  var configRoute = function () {
    var configs = new Config.Collection();

    return {
      layout: "one_pane",
      crumbs: [
        {"name": "Config","link": "_config"}
      ],
      views: {
        "#dashboard-content": new Config.View({collection: configs})
      },
      apiUrl: configs.url()
    };
  };

  Config.Routes = {
    "_config": configRoute
  };

  return Config;

});
