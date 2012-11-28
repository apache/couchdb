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

  Views.AllDocsItem = FauxtonAPI.View.extend({
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

    destroy: function(event) {
      event.preventDefault();
      var that = this;

      if (!window.confirm("Are you sure you want to delete this doc?")) {
        return false;
      }

      window.theDoc = this.model;
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

  Views.IndexItem = FauxtonAPI.View.extend({
    template: "documents/index_item",
    tagName: "li",
    initialize: function(options){
      this.index = options.index;
      this.ddoc = options.ddoc;
    },

    serialize: function() {
      return {
        index: this.index,
        ddoc: this.ddoc
      };
    }
  });

  Views.AllDocsList = FauxtonAPI.View.extend({
    template: "documents/all_docs_list",

    events: {
       "click button.all": "selectAll"
     },

    establish: function() {
      return [this.model.allDocs.fetch()];
    },

    selectAll: function(evt){
      $("input:checkbox").attr('checked', !$(evt.target).hasClass('active'));
    },

    serialize: function() {
      return {
        database: this.model
      };
    },

    beforeRender: function() {
      this.model.allDocs.each(function(doc) {
        this.insertView("table.all-docs tbody", new Views.AllDocsItem({
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

  Views.Sidebar = FauxtonAPI.View.extend({
    template: "documents/sidebar",
    events: {
      "click a.new#doc": "newDocument",
      "click a.new#index": "newIndex"
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
        view: [1,2]
      };
    },

    newDocument: function(){
      alert('coming soon');
    },

    newIndex:  function(){
      alert('coming soon');
    },

    buildIndexList: function(collection, selector, design){
      _.each(_.keys(collection), function(key){
        this.insertView("ul.nav." + selector, new Views.IndexItem({
          ddoc: design,
          index: key
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

  return Views;
});
