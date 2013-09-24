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
       "modules/databases/base",
       "addons/permissions/views"
],
function (app, FauxtonAPI, Databases, Permissions) {
  
  var PermissionsRouteObject = FauxtonAPI.RouteObject.extend({
    layout: 'one_pane',
    selectedHeader: 'Databases',

    routes: {
      'database/:database/permissions': 'permissions'
    },

    initialize: function (route, masterLayout, options) {
      var docOptions = app.getParams();
      docOptions.include_docs = true;

      this.databaseName = options[0];
      this.database = new Databases.Model({id:this.databaseName});
      this.security = new Permissions.Security(null, {
        database: this.database
      });
    },

    establish: function () {
      return [this.database.fetch(), this.security.fetch()];
    },

    permissions: function () {
      this.setView('#dashboard-content', new Permissions.Permissions({
        database: this.database,
        model: this.security
      }));

    },

    crumbs: function () {
      return [
        {"name": "Databases", "link": "/_all_dbs"},
        {"name": this.database.id, "link": Databases.databaseUrl(this.database)},
        {"name": "Permissions", "link": "/permissions"}
      ];
    },

  });
  
  Permissions.RouteObjects = [PermissionsRouteObject];
  return Permissions;
});
