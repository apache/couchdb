define([
  "app",

  "api",

  // Modules
  "addons/config/routes"
],

function(app, FauxtonAPI, Config) {
  Config.initialize = function() {
    FauxtonAPI.addHeaderLink({title: "Config", href: "#_config"});
  };

  return Config;
});
