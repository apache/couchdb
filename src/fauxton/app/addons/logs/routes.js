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
      var deferred = FauxtonAPI.Deferred();

      return {
        layout: "one_pane",
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
          logs.fetch().done(function(resp) {
            deferred.resolve();
          });
          return [deferred];
        }
      };
    }
  };

  return Log;

});