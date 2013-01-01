define([
  "app",

  "api",

  // Modules
  "addons/logs/resources"
],

function(app, FauxtonAPI, Log) {

  Log.Routes = {
    "_log": function() {
      var logs = new Log.Collection();

      return {
        layout: "with_sidebar",
        crumbs: [
          {"name": "Logs", "link": "_log"}
        ],
        views: {
          "#dashboard-content": new Log.Views.View({collection: logs}),
          "#sidebar-content": new Log.Views.FilterView({})
        },
        apiUrl: logs.url(),
        establish: function() {
          return [logs.fetch()];
        }
      };
    }
  };

  return Log;

});
