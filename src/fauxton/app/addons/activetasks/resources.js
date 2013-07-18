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
  "modules/fauxton/base",
  "d3"
],

function (app, backbone, Fauxton) {
	var Active = {},
      apiv = app.versionAPI;

  Active.Task = Backbone.Model.extend({});

// ALL THE TASKS
  Active.Tasks = Backbone.Model.extend({
    alltypes: {
      "all": "All tasks",
      "replication": "Replication",
      "database_compaction":" Database Compaction",
      "indexer": "Indexer",
      "view_compaction": "View Compaction"
    },
    url:  '/_active_tasks',
    parse: function(resp){
      var typeCollections= {},
      types = this.getUniqueTypes(resp);

      _.each(types, function(val, key){
        typeCollections[key] = new Active.AllTasks(this.sortThis(resp, key));
      }, this);

      typeCollections.all = new Active.AllTasks(resp);

      this.set(typeCollections);  //now set them all to the model
    },
    getUniqueTypes: function(resp){
      var types = this.alltypes;

      _.each(resp, function(type){
        if( typeof(types[type.type]) == "undefined"){
          types[type.type] = type.type.replace(/_/g,' ');
        }
      },this);

      this.alltypes = types;
      return types;
    },
    sortThis: function(resp, type){
      return _.filter(resp, function(item) { return item.type === type; });
    },
    changeView: function (view){
      this.set({
        "currentView": view
      });
    },
    getCurrentViewData: function(){
      var currentView = this.get('currentView');
      return this.get(currentView);
    },
    getDatabaseCompactions: function(){
      return this.get('databaseCompactions');
    },
    getIndexes: function(){
      return this.get('indexes');
    },
    getViewCompactions: function(){
      return this.get('viewCompactions');
    }
  });

//ALL TASKS

//NEW IDEA. Lets make this extremely generic, so if there are new weird tasks, they get sorted and collected.

  Active.AllTasks = Backbone.Collection.extend({
    model: Active.Task
  });


	return Active;
});
