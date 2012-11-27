define([
  "app",

  "fauxton_api",

  // Modules
  "modules/databases"
],

function(app, FauxtonAPI, Databases) {
  var allDbsCallback = function() {
    var data = {
      databases: new Databases.List()
    };
    var deferred = FauxtonAPI.Deferred();

    return {
      layout: "with_sidebar",

      data: data,

      crumbs: [
        {"name": "Dashboard", "link": app.root}
      ],

      views: {
        "#dashboard-content": new Databases.Views.List({
          collection: data.databases
        }),

        "#sidebar-content": new Databases.Views.Sidebar({
          collection: data.databases
        })
      },

      apiUrl: data.databases.url(),

      establish: function() {
        data.databases.fetch().done(function(resp) {
          $.when.apply(null, data.databases.map(function(database) {
            return database.status.fetch();
          })).done(function(resp) {
            deferred.resolve();
          });
        });
        return [deferred];
      }
    };
  };

  Databases.Routes = {
    "": allDbsCallback,
    "index.html": allDbsCallback,
    "_all_dbs": allDbsCallback
  };

  return Databases;
});