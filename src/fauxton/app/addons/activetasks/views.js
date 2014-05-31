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
        "addons/activetasks/resources"
],

function (app, FauxtonAPI, ActiveTasks) {

  var Views = {},
      Events = {},
      pollingInfo = {
        rate: "5",
        intervalId: null
      };


  Views.Events = _.extend(Events, Backbone.Events);

  Views.View = FauxtonAPI.View.extend({
    tagName: "table",

    className: "table table-bordered table-striped active-tasks",

    template: "addons/activetasks/templates/table",

    events: {
      "click th": "sortByType"
    },

    filter: "all",

    initialize: function (options) {
      this.listenTo(ActiveTasks.events, "tasks:filter", this.filterAndRender);
      this.listenTo(this.collection, "reset", this.render);
    },

    beforeRender: function () {
      this.filterAndInsertView(this.filter);
    },

    filterAndRender: function (view) {
      this.filter = view;
      this.render();
    },

    filterAndInsertView: function () {
      var that = this;
      this.removeView(".js-tasks-go-here");

      this.collection.forEach(function (item) {
        if (that.filter !== "all" && item.get("type") !== that.filter) {
          return;
        }
        var view = new Views.TableDetail({
          model: item
        });
        this.insertView(".js-tasks-go-here", view);
      }, this);
    },

    afterRender: function () {
      Events.bind("update:poll", this.setPolling, this);
      this.setPolling();
    },

    establish: function () {
      return [this.collection.fetch()];
    },

    serialize: function () {
      return {
        currentView: this.currentView,
        collection: this.collection
      };
    },

    sortByType: function (e) {
      var currentTarget = e.currentTarget,
          datatype = this.$(currentTarget).attr("data-type");

      this.collection.sortByColumn(datatype);
      this.render();
    },

    setPolling: function () {
      var collection = this.collection;

      clearInterval(pollingInfo.intervalId);
      pollingInfo.intervalId = setInterval(function () {
        collection.fetch({reset: true});
      }, pollingInfo.rate * 1000);
    },

    cleanup: function () {
      clearInterval(pollingInfo.intervalId);
    }
  });

  Views.TabMenu = FauxtonAPI.View.extend({
    template: "addons/activetasks/templates/tabs",

    events: {
      "click .task-tabs li": "requestByType",
      "change #pollingRange": "changePollInterval"
    },

    serialize: function () {
      return {
        filters: {
          "all": "All tasks",
          "replication": "Replication",
          "database_compaction":" Database Compaction",
          "indexer": "Indexer",
          "view_compaction": "View Compaction"
        }
      };
    },

    afterRender: function(){
      this.$('.task-tabs').find('li').eq(0).addClass('active');
    },

    changePollInterval: function(e){
      var range = this.$(e.currentTarget).val();
      this.$('label[for="pollingRange"] span').text(range);
      pollingInfo.rate = range;
      clearInterval(pollingInfo.intervalId);
      Events.trigger('update:poll');
    },

    cleanup: function () {
      clearInterval(pollingInfo.intervalId);
    },

    requestByType: function(e){
      var currentTarget = e.currentTarget,
          datatype = this.$(currentTarget).attr("data-type");

      this.$('.task-tabs').find('li').removeClass('active');
      this.$(currentTarget).addClass('active');

      ActiveTasks.events.trigger("tasks:filter", datatype);
    }
  });

  Views.TableDetail = FauxtonAPI.View.extend({
    tagName: 'tr',

    template: "addons/activetasks/templates/tabledetail",

    initialize: function(){
      this.type = this.model.get('type');
    },

    getObject: function(){
      var objectField = this.model.get('database');
      if (this.type === "replication"){
        objectField = this.model.get('source') + " to " + this.model.get('target');
      } else if (this.type === "indexer") {
        objectField = this.model.get("database") + " / " + this.model.get("design_document");
      }
      return objectField;
    },

    getProgress:  function(){
      var progress = "";
      if (this.type === "indexer"){
        progress = "Processed " +this.model.get('changes_done')+ " of "+this.model.get('total_changes')+ ' changes.';
      } else if (this.type === "replication"){
        progress = this.model.get('docs_written')+ " docs written. ";
        if (this.model.get('changes_pending') !== undefined) {
          progress += this.model.get('changes_pending') + ' pending changes. ';
        }
      }
      if (this.model.get('source_seq') !== undefined) {
        progress += "Current source sequence: " + this.model.get('source_seq') + ". ";
      }
      if (this.model.get('changes_done') !== undefined) {
        progress += this.model.get('changes_done') + " Changes done. ";
      }
      if (this.model.get('progress') !== undefined) {
        progress += "Progress: " + this.model.get('progress') + "% ";
      }

      return progress;
    },

    serialize: function(){
      return {
        model: this.model,
        objectField: this.getObject(),
        progress: this.getProgress()
      };
    }
  });

  return Views;
});
