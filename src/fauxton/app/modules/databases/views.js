define([
  "app",

  "api"
],

function(app, FauxtonAPI) {
  var Views = {};

  Views.Item = FauxtonAPI.View.extend({
    template: "templates/databases/item",
    tagName: "tr",

    serialize: function() {
      return {
        database: this.model
      };
    }
  });

  Views.List = FauxtonAPI.View.extend({
    dbLimit: 10,
    template: "templates/databases/list",
    events: {
      "click button.all": "selectAll",
      "submit form.database-search": "switchDatabase"
    },

    initialize: function(options) {
      this.collection.on("add", this.render, this);
    },

    serialize: function() {
      return {
        databases: this.collection
      };
    },

    switchDatabase: function(event) {
      event.preventDefault();
      var dbname = this.$el.find("input.search-query").val();

      if (dbname) {
        // TODO: switch to using a model, or Databases.databaseUrl()
        // Neither of which are in scope right now
        // var db = new Database.Model({id: dbname});
        var url = ["/database/", dbname, "/_all_docs?limit=10"].join('');
        FauxtonAPI.navigate(url);
      }
    },

    beforeRender: function() {
      this.collection.each(function(database) {
        this.insertView("table.databases tbody", new Views.Item({
          model: database
        }));
      }, this);
    },

    afterRender: function() {
      var dbLimit = this.dbLimit;
      var ajaxReq;

      this.$el.find("input.search-query").typeahead({
        source: function(query, process) {
          var url = [
            app.host,
            "/_all_dbs?startkey=%22",
            query,
            "%22&endkey=%22",
            query,
            "\u9999%22&limit=",
            dbLimit
          ].join('');
          if (ajaxReq) ajaxReq.abort();
          ajaxReq = $.ajax({
            url: url,
            dataType: 'json',
            success: function(data) {
              process(data);
            }
          });
        }
      });
    },

    selectAll: function(evt){
      $("input:checkbox").attr('checked', !$(evt.target).hasClass('active'));
    }
  });

  Views.Sidebar = FauxtonAPI.View.extend({
    template: "templates/databases/sidebar",
    events: {
      "click a#new": "newDatabase",
      "click a#owned": "showMine",
      "click a#shared": "showShared"
    },

    newDatabase: function() {
      var notification;
      // TODO: use a modal here instead of the prompt
      var name = prompt('Name of database', 'newdatabase');
      if (name === null) {
        return;
      } else if (name.length === 0) {
        notification = FauxtonAPI.addNotification({
          msg: "Please enter a valid database name",
          type: "error",
          clear: true
        });
        return;
      }
      var db = new this.collection.model({
        id: encodeURIComponent(name),
        name: name
      });
      notification = FauxtonAPI.addNotification({msg: "Creating database."});
      db.save().done(function() {
        notification = FauxtonAPI.addNotification({
          msg: "Database created successfully",
          type: "success",
          clear: true
        });
        var route = "#/database/" +  name + "/_all_docs?limit=100";
        app.router.navigate(route, { trigger: true });
      }
      ).error(function(xhr) {
        var responseText = JSON.parse(xhr.responseText).reason;
        notification = FauxtonAPI.addNotification({
          msg: "Create database failed: " + responseText,
          type: "error",
          clear: true
        });
      }
      );
    },

    showMine: function(){
      console.log('will show users databases and hide shared');
    },

    showShared: function(){
      console.log('will show shared databases and hide the users');
      alert('Support for shared databases coming soon');
    }
  });

  return Views;
});
