define([
  "app",

  // Libs
  "backbone",
  "codemirror",
  "jshint",

  // Plugins
  "plugins/codemirror-javascript"
],

function(app, Backbone, Codemirror, JSHint) {
  var Views = {};

  Views.Popover = Backbone.View.extend({
    template: "databases/popover",

    serialize: function() {
      return {
        database: this.model,
        status: this.model.status
      };
    }
  });

  Views.Item = Backbone.View.extend({
    template: "databases/item",
    tagName: "tr",

    events: {
      "mouseover i.database-info": "displayInfo",
      "mouseout i.database-info": "hideInfo"
    },

    displayInfo: function(event) {
      var ele = $(event.currentTarget);
      ele.popover("show");
    },

    hideInfo: function(event) {
      var ele = $(event.currentTarget);
      ele.popover("hide");
    },

    afterRender: function() {
      var view = this;
      app.renderView(this, "FAKEITEM", Views.Popover, {model:view.model}, function(popoverEle) {
        view.$el.find("i.database-info").popover({
          title: view.model.id + " info",
          content: popoverEle.el
        });
      });
    },

    serialize: function() {
      return {
        database: this.model
      };
    }
  });

  Views.List = Backbone.View.extend({
    template: "databases/list",

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
    }
  });

  Views.AllDocsItem = Backbone.View.extend({
    template: "databases/all_docs_item",
    tagName: "tr",

    serialize: function() {
      return {
        doc: this.model
      };
    }
  });

  Views.AllDocsList = Backbone.View.extend({
    template: "databases/all_docs_list",

    serialize: function() {
      return {
        database: this.model
      };
    },

    beforeRender: function(manage) {
      this.model.allDocs.each(function(doc) {
        this.insertView("table.all-docs tbody", new Views.AllDocsItem({
          model: doc
        }));
      }, this);
    }
  });

  Views.Doc = Backbone.View.extend({
    template: "databases/doc",

    events: {
      "click button.save-doc": "saveDoc"
    },

    saveDoc: function(event) {
      alert("Save functionality coming soon.");
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
        }
      });
    }
  });

  Views.Sidebar = Backbone.View.extend({
    template: "databases/sidebar",
    events: {
      "click a#owned": "showMine",
      "click a#shared": "showShared"
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
