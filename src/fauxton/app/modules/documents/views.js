define([
  "app",

  "fauxton_api",

  // Libs
  "codemirror",
  "jshint",

  // Plugins
  "plugins/codemirror-javascript",
  "plugins/prettify"
],

function(app, FauxtonAPI, Codemirror, JSHint) {
  var Views = {};

  Views.Tabs = FauxtonAPI.View.extend({
    template: "documents/tabs"
  });

  Views.FieldEditorTabs = FauxtonAPI.View.extend({
    template: "documents/doc_field_editor_tabs",

    initialize: function(options) {
      this.selected = options.selected;
    },

    events: {
      "click button.delete": "destroy",
      "click button.duplicate": "duplicate"
    },

    destroy: function(event) {
      if (!window.confirm("Are you sure you want to delete this doc?")) {
        return false;
      }

      var database = this.model.database;

      this.model.destroy().then(function(resp) {
        FauxtonAPI.addNotification({
          msg: "Succesfully destroyed your doc"
        });
        FauxtonAPI.navigate(database.url("index"));
      }, function(resp) {
        FauxtonAPI.addNotification({
          msg: "Failed to destroy your doc!",
          type: "error"
        });
      });
    },

    duplicate: function(event) {
      FauxtonAPI.addNotification({
        type: "warning",
        msg: "Duplicate functionality coming soon."
      });
    },

    serialize: function() {
      var selected = this.selected;
      return {
        doc: this.model,
        isSelectedClass: function(item) {
          return item && item === selected ? "active" : "";
        }
      };
    },

    establish: function() {
      return [this.model.fetch()];
    }
  });

  Views.Document = FauxtonAPI.View.extend({
    template: "documents/all_docs_item",
    tagName: "tr",

    events: {
      "click button.delete": "destroy"
    },

    serialize: function() {
      return {
        doc: this.model
      };
    },

    establish: function() {
      return [this.model.fetch()];
    },

    destroy: function(event) {
      event.preventDefault();
      var that = this;

      if (!window.confirm("Are you sure you want to delete this doc?")) {
        return false;
      }

      this.model.destroy().then(function(resp) {
        FauxtonAPI.addNotification({
          msg: "Succesfully destroyed your doc"
        });
        that.$el.fadeOut();
        that.model.collection.remove(that.id);
      }, function(resp) {
        FauxtonAPI.addNotification({
          msg: "Failed to destroy your doc!",
          type: "error"
        });
      });
    }
  });

  Views.Row = FauxtonAPI.View.extend({
    template: "documents/index_row",
    tagName: "tr",

    serialize: function() {
      return {
        doc: this.model
      };
    }
  });

  Views.IndexItem = FauxtonAPI.View.extend({
    template: "documents/index_menu_item",
    tagName: "li",
    initialize: function(options){
      this.index = options.index;
      this.ddoc = options.ddoc;
      this.database = options.database;
    },

    serialize: function() {
      return {
        index: this.index,
        ddoc: this.ddoc,
        database: this.database
      };
    }
  });

  // TODO: Rename to reflect that this is a list of rows or documents
  Views.AllDocsList = FauxtonAPI.View.extend({
    template: "documents/all_docs_list",
    events: {
      "click button.all": "selectAll"
    },

    initialize: function(options){
      this.nestedView = options.nestedView || Views.Document;
    },

    establish: function() {
      return [this.collection.fetch()];
    },

    selectAll: function(evt){
      $("input:checkbox").attr('checked', !$(evt.target).hasClass('active'));
    },

    serialize: function() {
      return {
        database: this.collection
      };
    },

    beforeRender: function() {
      this.collection.each(function(doc) {
        this.insertView("table.all-docs tbody", new this.nestedView({
          model: doc
        }));
      }, this);
    },

    afterRender: function(){
      prettyPrint();
    }
  });

  Views.Doc = FauxtonAPI.View.extend({
    template: "documents/doc",

    events: {
      "click button.save-doc": "saveDoc"
    },

    initialize: function() {
      this.model.on("sync", this.updateValues, this);
    },

    updateValues: function() {
      notification = FauxtonAPI.addNotification({
        msg: "Document saved successfully.",
        type: "success",
        clear: true
      });
      this.editor.setValue(this.model.prettyJSON());
    },

    establish: function() {
      return [this.model.fetch()];
    },

    saveDoc: function(event) {
      var json, notification;
      if (this.hasValidCode()) {
        json = JSON.parse(this.editor.getValue());
        this.model.set(json);
        notification = FauxtonAPI.addNotification({msg: "Saving document."});
        this.model.save();
      } else {
        notification = FauxtonAPI.addNotification({
          msg: "Please fix the JSON errors and try again.",
          type: "error",
          selector: "#doc .errors-container"
        });
      }
    },

    hasValidCode: function() {
      return JSHINT(this.editor.getValue()) !== false;
    },

    runJSHint: function() {
      var json = this.editor.getValue();
      var output = JSHint(json);

      // Clear existing markers
      for (var i = 0, l = this.editor.lineCount(); i < l; i++) {
        this.editor.clearMarker(i);
      }

      if (output === false) {
        _.map(JSHint.errors, function(error) {
          var line = error.line - 1;
          var className = "view-code-error-line-" + line;
          this.editor.setMarker(line, "●", "view-code-error "+className);

          setTimeout(function() {
            $(".CodeMirror ."+className).tooltip({
              title: "ERROR: " + error.reason
            });
          }, 0);
        }, this);
      }
    },

    serialize: function() {
      return {
        doc: this.model
      };
    },

    afterRender: function() {
      var that = this;
      this.editor = Codemirror.fromTextArea(this.$el.find("textarea.doc-code").get()[0], {
        mode: "application/json",
        json: false,
        lineNumbers: true,
        matchBrackets: true,
        lineWrapping: true,
        onChange: function() {
          that.runJSHint();
        },
        extraKeys: {
          "Ctrl-S": function(instance) { that.saveDoc(); },
          "Ctrl-/": "undo"
        }
      });
    }
  });

  Views.DocFieldEditor = FauxtonAPI.View.extend({
    template: "documents/doc_field_editor",

    events: {
      "click button.save": "saveDoc"
    },

    saveDoc: function(event) {
      FauxtonAPI.addNotification({
        type: "warning",
        msg: "Save functionality coming soon."
      });
    },

    serialize: function() {
      return {
        doc: this.getModel()
      };
    },

    getModel: function() {
      return this.model;
    },

    establish: function() {
      return [this.model.fetch()];
    }
  });

  Views.Sidebar = FauxtonAPI.View.extend({
    template: "documents/sidebar",
    events: {
      "click a.new#index": "newIndex",
      "click .nav-list.views a.new": "showNew",
      // "click .nav-list.views a.toggle-view": "toggleView",
      "click .nav-list a.toggle-view#all-docs": "toggleView",
      "click .nav-list a.toggle-view#design-docs": "toggleView",
      "click .nav-list.search a.new": "showNew",
      "click .nav-list.search a.toggle-view": "toggleView"
    },

    establish: function() {
      if (this.collection) {
        return [this.collection.fetch()];
      } else {
        return null;
      }
    },

    serialize: function() {
      return {
        index: [1,2,3],
        view: [1,2],
        database: this.collection.database
      };
    },

    newIndex:  function(event){
      event.preventDefault();
      alert('coming soon');
    },

    showNew: function(event){
      event.preventDefault();
      alert('show new search/view dialog');
    },

    toggleView: function(event){
      alert('filter data by search/view/type');
      event.preventDefault();
      url = event.currentTarget.href.split('#')[1];
      app.router.navigate(url);
    },

    buildIndexList: function(collection, selector, design){

      _.each(_.keys(collection), function(key){
        this.insertView("ul.nav." + selector, new Views.IndexItem({
          ddoc: design,
          index: key,
          database: this.collection.database.id
        }));
      }, this);
    },

    beforeRender: function(manage) {
      this.collection.each(function(design) {
        if (design.has('doc')){
          var ddoc = design.id.split('/')[1];
          if (design.get('doc').views){
            this.buildIndexList(design.get('doc').views, "views", ddoc);
          }
          if (design.get('doc').indexes){
            this.buildIndexList(design.get('doc').indexes, "search", ddoc);
          }
        }
      }, this);
    }

  });

  Views.Indexed = FauxtonAPI.View.extend({});

  return Views;
});
