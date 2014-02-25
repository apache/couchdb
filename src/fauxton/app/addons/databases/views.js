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

  "addons/fauxton/components",
  "api",
  "addons/databases/resources"
],

function(app, Components, FauxtonAPI, Databases) {
  var Views = {};

  Views.Item = FauxtonAPI.View.extend({
    template: "addons/databases/templates/item",
    tagName: "tr",
    establish: function(){
      return [this.model.fetch()];
    },
    serialize: function() {

      return {
        encoded: app.utils.safeURLName(this.model.get("name")),
        database: this.model,
        docLimit: Databases.DocLimit
      };
    }
  });

  Views.List = FauxtonAPI.View.extend({
    dbLimit: 20,
    perPage: 20,
    template: "addons/databases/templates/list",
    events: {
      "click button.all": "selectAll",
      "submit form#jump-to-db": "switchDatabase"
    },

    initialize: function(options) {
      var params = app.getParams();
      this.page = params.page ? parseInt(params.page, 10) : 1;
    },

    serialize: function() {
      return {
        databases: this.collection
      };
    },
    establish: function(){
      var currentDBs = this.paginated();
      var deferred = FauxtonAPI.Deferred();

      FauxtonAPI.when(currentDBs.map(function(database) {
        return database.status.fetchOnce();
      })).always(function(resp) {
        //make this always so that even if a user is not allowed access to a database
        //they will still see a list of all databases
        deferred.resolve();
      });
      return [deferred];
    },
    switchDatabase: function(event, selectedName) {
      event && event.preventDefault();

      var dbname = this.$el.find("[name='search-query']").val().trim();

      if (selectedName) {
        dbname = selectedName;
      }

      if (dbname && this.collection.where({"id":app.utils.safeURLName(dbname)}).length > 0){
          // TODO: switch to using a model, or Databases.databaseUrl()
          // Neither of which are in scope right now
          // var db = new Database.Model({id: dbname});
          var url = ["/database/", app.utils.safeURLName(dbname), "/_all_docs?limit=" + Databases.DocLimit].join('');
          FauxtonAPI.navigate(url);
      } else {
        FauxtonAPI.addNotification({
          msg: 'Database does not exist.',
          type: 'error'
        });
      }
    },

    paginated: function() {
      var start = (this.page - 1) * this.perPage;
      var end = this.page * this.perPage;
      return this.collection.slice(start, end);
    },

    beforeRender: function() {

      this.insertView("#newButton", new Views.NewDatabaseButton({
        collection: this.collection
      }));

      _.each(this.paginated(), function(database) {
        this.insertView("table.databases tbody", new Views.Item({
          model: database
        }));
      }, this);

      this.insertView("#database-pagination", new Components.Pagination({
        page: this.page,
        perPage: this.perPage,
        total: this.collection.length,
        urlFun: function(page) {
          return "#/_all_dbs?page=" + page;
        }
      }));
    },

    setPage: function(page) {
      this.page = page || 1;
    },

    afterRender: function() {
      var that = this;
      this.dbSearchTypeahead = new Components.DbSearchTypeahead({
        dbLimit: this.dbLimit,
        el: "input.search-autocomplete",
        onUpdate: function (item) {
          that.switchDatabase(null, item);
        }
      });

      this.dbSearchTypeahead.render();
    },

    selectAll: function(evt){
      $("input:checkbox").attr('checked', !$(evt.target).hasClass('active'));
    }
  });


  Views.NewDatabaseButton = FauxtonAPI.View.extend({
    template: "addons/databases/templates/newdatabase",

    events: {
      "click a#new": "newDatabase"
    },

    newDatabase: function(){
      this.newDatabaseModal.showModal();
    },

    beforeRender: function(manage) {
      this.newDatabaseModal = this.insertView(
        '#add-db-modal',
        new Views.NewDatabaseModal({
          collection: this.collection
        })
      );
    }
  });

  Views.NewDatabaseModal = Components.ModalView.extend({
    template: "addons/databases/templates/new_database_modal",

    events: {
      "click #add-db-btn": "newDatabase",
      "submit #add-db-check": "newDatabase"
    },

    newDatabase: function(event) {
      event.preventDefault();
      var notification;
      var db;
      var name = this.$('#db_name')[0].value;

      if (name === null || name.length === 0 || name.match(/^[a-z][a-z0-9\_\$\(\)\+\/\-]/g) === null) {
        msg = name + " is an invalid database name. ";
        msg += "Only lower case letters (a-z), digits (0-9), and any of the ";
        msg += "characters _, $, (, ), +, -, / are allowed, and the name ";
        msg += "must begin with a letter.";
        this.set_error_msg(msg);
      } else {
        db = new this.collection.model({
          id: encodeURIComponent(name),
          name: name
        });
        notification = FauxtonAPI.addNotification({msg: "Creating database."});
        var that = this;
        db.save().done(function() {
          notification = FauxtonAPI.addNotification({
            msg: "Database created successfully",
            type: "success",
            clear: true
          });
          that.hideModal();
          var route = "#/database/" +  name + "/_all_docs?limit=" + Databases.DocLimit;
          app.router.navigate(route, { trigger: true });
        }).error(function(xhr) {
          var responseText = JSON.parse(xhr.responseText).reason;
          that.set_error_msg("Create database failed: " + responseText);
        });
      }
    }
  });

  Views.Sidebar = FauxtonAPI.View.extend({
    template: "addons/databases/templates/sidebar",

    events: {
      "click a#new": "newDatabase",
      "click a#owned": "showMine",
      "click a#shared": "showShared"
    },

    newDatabase: function(){
      this.newDatabaseModal.showModal();
    },

    beforeRender: function(manage) {
      // I think this should use the ensure thingy
      this.newDatabaseModal = this.insertView(
        '#add-db-modal',
        new Views.NewDatabaseModal({
          collection: this.collection
        })
      );
    },

    showMine: function(){
      $.contribute(
        'Show unshared databases',
        'app/addons/databases/views.js'
      );
    },

    showShared: function(){
      $.contribute(
        'Show shared databases (e.g. continuous replications to/from the database)',
        'app/addons/databases/views.js'
      );
    }
  });

  return Views;
});
