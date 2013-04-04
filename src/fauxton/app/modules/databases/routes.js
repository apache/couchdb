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
    layout: "with_sidebar",

    crumbs: [
      {"name": "Databases", "link": "/_all_dbs"}
    ],

    routes: ["", "index.html", "_all_dbs(:params)"],

    apiUrl: function() {
      return this.databases.url();
    },

    initialize: function() {
      this.databases = new Databases.List();
      this.deferred = FauxtonAPI.Deferred();

      this.databasesView = this.setView("#dashboard-content", new Views.List({
          collection: this.databases
      }));
      this.sidebarView = this.setView("#sidebar-content", new Views.Sidebar({
          collection: this.databases
      }));
    },

    route: function() {
      var params = app.getParams();
      this.databasesView.setPage(params.page);
    },

    rerender: function() {
      this.databasesView.render();
    },

    establish: function() {
      var databases = this.databases;
      var deferred = this.deferred;

      databases.fetch().done(function(resp) {
        $.when.apply(null, databases.map(function(database) {
          return database.status.fetch();
        })).done(function(resp) {
          deferred.resolve();
        });
      });

      return [deferred];
    },

    mrEvent: function() {
      console.log("Triggering a most excellent event!!!!");
    },

    events: {
      "myrandom_event": "mrEvent"
    }
  });

  var allDbsCallback = function() {
    var data = {
      databases: new Databases.List()
    };
    var deferred = FauxtonAPI.Deferred();

    return {
      layout: "with_sidebar",

      data: data,

      crumbs: [
        {"name": "Databases", "link": "/_all_dbs"}
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

  /*
  Databases.Routes = {
    "": allDbsCallback,
    "index.html": allDbsCallback,
    "_all_dbs(:params)": allDbsCallback
  };
  */

  Databases.RouteObjects = [new AllDbsRouteObject()];

  return Databases;
});
