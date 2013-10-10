// Licensed under the Apache License, Version 2.0 (the "License"); you may not
// use this file except in compliance with the License. You may obtain a copy of
// the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
// WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
// License for the specific language governing permissions and limitations under
// the License.

define([
  "app",

  "api",

  // Modules
  "modules/databases/resources",
  // TODO:: fix the include flow modules so we don't have to require views here
  "modules/databases/views"
],

function(app, FauxtonAPI, Databases, Views) {

  var AllDbsRouteObject = FauxtonAPI.RouteObject.extend({
    layout: "one_pane",

    crumbs: [
      {"name": "Databases", "link": "/_all_dbs"}
    ],

    routes: {
      "": "allDatabases",
      "index.html": "allDatabases",
      "_all_dbs(:params)": "allDatabases"
    },

    apiUrl: function() {
      return [this.databases.url(), this.databases.documentation()];
    },

    selectedHeader: "Databases",

    initialize: function() {
      this.databases = new Databases.List();
      this.deferred = FauxtonAPI.Deferred();
    },

    allDatabases: function() {
      var params = app.getParams(),
          dbPage = params.page;

      this.databasesView = this.setView("#dashboard-content", new Views.List({
        collection: this.databases
      }));

      this.databasesView.setPage(dbPage);
    },

    establish: function() {
      var databases = this.databases;
      var deferred = this.deferred;

      databases.fetch().done(function(resp) {
        FauxtonAPI.when(databases.map(function(database) {
          return database.status.fetch();
        })).always(function(resp) {
          //make this always so that even if a user is not allowed access to a database
          //they will still see a list of all databases
          deferred.resolve();
        });
      });

      return [deferred];
    }
  });

  Databases.RouteObjects = [AllDbsRouteObject];

  return Databases;
});
