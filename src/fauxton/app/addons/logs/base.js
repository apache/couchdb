define([
  "app",

  "api",

  // Modules
  "addons/logs/routes"
],

function(app, FauxtonAPI, Log) {
  Log.initialize = function() {
    FauxtonAPI.addHeaderLink({title: "Log", href: "#_log"});
  };

  return Log;
});
