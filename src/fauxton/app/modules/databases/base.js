define([
  "app",

  "api",

  // Modules
  "modules/databases/routes",
  // Views
  "modules/databases/views"

],

function(app, FauxtonAPI, Databases, Views) {
  Databases.Views = Views;

  // Utility functions
  Databases.databaseUrl = function(database) {
    var name = _.isObject(database) ? database.id : database;

    return ["/database/", name, "/_all_docs?limit=10"].join('');
  };

  return Databases;
});
