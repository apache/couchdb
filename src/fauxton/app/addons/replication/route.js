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
  "addons/replication/resources",
  "addons/replication/views"
],
function(app, FauxtonAPI, Replication, Views) {
  var  RepRouteObject = FauxtonAPI.RouteObject.extend({
    layout: "one_pane",
    roles: ["_admin"],
    routes: {
      "replication": "defaultView",
      "replication/:dbname": "defaultView"
    },
    selectedHeader: "Replication",
    apiUrl: function() {
      return [this.replication.url(), this.replication.documentation];
    },
    crumbs: [
      {"name": "Replicate changes from: ", "link": "replication"}
    ],
    defaultView: function(dbname){
			this.databases = new Replication.DBList({});
      this.tasks = new Replication.Tasks({id: "ReplicationTasks"});
      this.replication = new Replication.Replicate({});
			this.setView("#dashboard-content", new Views.ReplicationForm({
        selectedDB: dbname ||"",
				collection: this.databases,
        status:  this.tasks
			}));  
    }
  });


	Replication.RouteObjects = [RepRouteObject];

  return Replication;
});
