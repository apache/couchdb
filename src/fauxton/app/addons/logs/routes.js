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
          {"name": "Dashboard", "link": app.root},
          {"name": "Logs","link": app.root}
        ],
        views: {
          "#dashboard-content": new Log.View({collection: logs}),
          "#sidebar-content": new Log.FilterView({})
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
