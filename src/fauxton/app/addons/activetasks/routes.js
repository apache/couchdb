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
  "addons/activetasks/resources",
  "addons/activetasks/views"
],

function (app, FauxtonAPI, Activetasks, Views) {

  var  ActiveTasksRouteObject = FauxtonAPI.RouteObject.extend({
    layout: "with_sidebar",
    routes: {
      "activetasks/:id": "defaultView",
      "activetasks": "defaultView"
    },
    crumbs: [],
    apiUrl: function(){
      return app.host+"/_active_tasks";
    }, 
    defaultView: function(id){
      var newtasks = new Activetasks.Tasks({
        currentView: "all", 
        id:'activeTasks'
      });
      this.setView("#sidebar-content", new Views.TabMenu({
        currentView: "all",
        model: newtasks
      })); 

      this.setView("#dashboard-content", new Views.DataSection({
        model: newtasks,
        currentView: "all"
      })); 
    }
  });

  Activetasks.RouteObjects = [ActiveTasksRouteObject];

  return Activetasks;
});
