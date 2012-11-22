define([
  "app",

  "fauxton_api"
],

function(app, FauxtonAPI) {
  var Views = {};

  Views.Item = FauxtonAPI.View.extend({
    template: "databases/item",
    tagName: "tr",

    serialize: function() {
      return {
        database: this.model
      };
    }
  });

  Views.List = FauxtonAPI.View.extend({
    template: "databases/list",
    events: {
      "click button.all": "selectAll"
    },

    initialize: function(options) {
      this.collection.on("add", this.render, this);
    },

    serialize: function() {
      return {
        databases: this.collection
      };
    },

    beforeRender: function() {
      this.collection.each(function(database) {
        this.insertView("table.databases tbody", new Views.Item({
          model: database
        }));
      }, this);
    },

    selectAll: function(evt){
      $("input:checkbox").attr('checked', !$(evt.target).hasClass('active'));
    }
  });

  Views.Sidebar = FauxtonAPI.View.extend({
    template: "databases/sidebar",
    events: {
      "click a#new": "newDatabase",
      "click a#owned": "showMine",
      "click a#shared": "showShared"
    },

    newDatabase: function() {
      // TODO: use a modal here instead of the prompt
      var name = prompt('Name of database', 'newdatabase');
      var db = new this.collection.model({
        id: encodeURIComponent(name),
        name: name
      });
      db.save().done(function() {
        var route = "#/database/" +  name + "/_all_docs?limit=100";
        app.router.navigate(route, { trigger: true });
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
