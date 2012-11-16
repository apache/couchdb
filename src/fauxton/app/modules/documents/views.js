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

  Views.AllDocsItem = Backbone.View.extend({
    template: "documents/all_docs_item",
    tagName: "tr",

    serialize: function() {
      return {
        doc: this.model
      };
    }
  });

  Views.IndexItem = Backbone.View.extend({
    template: "documents/index_item",
    tagName: "li",
    initialize: function(options){
      this.index = options.index;
      this.ddoc = options.ddoc;
    },

    serialize: function() {
      console.log(this);
      return {
        index: this.index,
        ddoc: this.ddoc
      };
    }
  });

  Views.AllDocsList = Backbone.View.extend({
    template: "documents/all_docs_list",

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
    template: "documents/doc",

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
    template: "documents/sidebar",
    events: {
      "click a.new#doc": "newDocument",
      "click a.new#index": "newIndex"
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
        console.log(key);
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