define([
  "app",

  // Libs
  "backbone",

  // Modules
  "modules/databases"

  // Views
  //"modules/api/views"

  // Plugins
],

function(app, Backbone, Databases) {
  /* API DIFFERENCES BETWEEN CLOUDANT AND COUCHDB
   * /_active_tasks
   * /_stats
   * /_changes
   * /_system
   * quorum params
   * /db/_compact*
   * /db/_view_cleanup
   * /db/_temp_view
   * /db/_ensure_full_commit
   * /db/_purge
   * /db/_rev_limit ?
   */

  var API = app.module();

  API.methods = {
  };

  API.ActiveTask = Backbone.Model.extend({
    taskType: function() {
      return this.get("type");
    }
  });

  API.ActiveTasks = Backbone.Collection.extend({
    model: API.ActiveTask,

    url: function() {
      return app.host + "/_active_tasks";
    }
  });

  return API;
});
