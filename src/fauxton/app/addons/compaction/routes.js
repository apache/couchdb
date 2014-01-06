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
       "addons/compaction/views",
       "addons/databases/resources"
],

function(app, FauxtonAPI, Compaction, Databases) {

  var  CompactionRouteObject = FauxtonAPI.RouteObject.extend({
    layout: "one_pane",

    crumbs: function () {
      return [
        {"name": this.database.id, "link": Databases.databaseUrl(this.database)},
        {"name": "Compact & Clean", "link": "compact"}
      ];
    },

    routes: {
      "database/:database/compact": "compaction"
    },

    initialize: function(route, masterLayout, options) {
      var databaseName = options[0];

      this.database = this.database || new Databases.Model({id: databaseName});
    },

    compaction: function () {
      this.setView('#dashboard-content', new Compaction.Layout({model: this.database}));
    },

    establish: function () {
      return this.database.fetch();
    }

    /*apiUrl: function() {
      return [this.compactions.url(), this.compactions.documentation];
    },*/

  });

  Compaction.RouteObjects = [CompactionRouteObject];

  return Compaction;

});


