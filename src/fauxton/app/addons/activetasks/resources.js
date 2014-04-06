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
  "backbone",
  "addons/fauxton/base",
  "d3"
],

function (app, backbone, Fauxton) {
  var Active = {},
      apiv = app.versionAPI;
      app.taskSortBy = 'type';


  Active.events = {};
  _.extend(Active.events, Backbone.Events);

  Active.Task = Backbone.Model.extend({
    initialize: function () {
      this.set({"id": this.get('pid')});
    }
  });

  Active.AllTasks = Backbone.Collection.extend({
    model: Active.Task,

    sortByColumn: function (colName) {
      app.taskSortBy = colName;
      this.sort();
    },

    comparator: function (item) {
      return item.get(app.taskSortBy);
    },

    documentation: "_active_tasks",

    url: function (context) {
      if (context === "apiurl") {
        return window.location.origin + "/_active_tasks";
      } else {
        return app.host + "/_active_tasks";
      }
    }
  });


  return Active;
});
