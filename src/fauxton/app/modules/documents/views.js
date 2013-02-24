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
    template: "templates/documents/tabs",
    initialize: function(options){
      this.collection = options.collection;
      this.database = options.database;
      this.active_id = options.active_id;
    },

    events: {
      "click #delete-database": "delete_database"
    },

    serialize: function () {
      return {
        // TODO make this not hard coded here
        changes_url: '#' + this.database.url('changes'),
        db_url: '#' + this.database.url('index') + '?limit=100'
      };
    },

    beforeRender: function(manage) {
      this.insertView("#search", new Views.SearchBox({
        collection: this.collection,
        database: this.database.id
      }));
    },

    afterRender: function () {
      if (this.active_id) {
        this.$('.active').removeClass('active');
        this.$('#'+this.active_id).addClass('active');
      }
    },

    delete_database: function (event) {
      event.preventDefault();

      var result = confirm("Are you sure you want to delete this database?");

      if (!result) { return; }

      return this.database.destroy().done(function () {
        app.router.navigate('/', {trigger: true});
      });
    }
  });

  Views.SearchBox = FauxtonAPI.View.extend({
    template: "templates/documents/search",
    tagName: "form",
    initialize: function(options){
      this.collection = options.collection;
      this.database = options.database;
    },
    afterRender: function(){
      var collection = this.collection;
      var form = this.$el;
      var searchbox = form.find("input#searchbox");
      var database = this.database;

      form.submit(function(evt){
        evt.preventDefault();
        var viewname = form.find("input#view").val().split('/');
        var url = "#database/" + database + "/_design/";
        url += viewname[0] + "/_view/" + viewname[1];
        if (searchbox.val() !== ""){
          // TODO: this'll need to work when val() is a number etc.
          url += '?start_key="' + searchbox.val() + '"';
        }
        FauxtonAPI.navigate(url);
      });

      searchbox.typeahead({
        source: function(query, process) {
          // TODO: include _all_docs and view keys somehow
          var views = _.map(collection.pluck('doc'), function(d){
            return _.map(_.keys(d.views), function(view){
              return d._id.split('/')[1] + "/" + view;
            });
          });
          return _.flatten(views);
        },
        minLength: 3,
        updater: function(item){
          // TODO: some way to return the original search box
          this.$element.removeClass('span12');
          this.$element.addClass('span6');
          this.$element.attr('placeholder', 'Search by view key');
          $('<span class="add-on span6">' + item +'</span>').insertBefore(this.$element);
          $('<input type="hidden" id="view" value="' + item +'"/>').insertBefore(this.$element);
          // Remove the type ahead for now
          $('.typehead').unbind();
        }
      });
    }
  });

  Views.FieldEditorTabs = FauxtonAPI.View.extend({
    template: "templates/documents/doc_field_editor_tabs",

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
    template: "templates/documents/all_docs_item",
    tagName: "tr",
    className: "all-docs-item",

    events: {
      "click button.delete": "destroy"
    },

    attributes: function() {
      return {
        "data-id": this.model.id
      };
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
    template: "templates/documents/index_row_docular",
    tagName: "tr",

    serialize: function() {
      return {
        doc: this.model
      };
    }
  });

  Views.IndexItem = FauxtonAPI.View.extend({
    template: "templates/documents/index_menu_item",
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
    template: "templates/documents/all_docs_list",
    events: {
      "click button.all": "selectAll",
      "click button.bulk-delete": "bulkDelete",
      "change form.view-query-update input": "updateFilters",
      "change form.view-query-update select": "updateFilters",
      "submit form.view-query-update": "updateView"
    },

    initialize: function(options){
      this.nestedView = options.nestedView || Views.Document;
      this.rows = {};
      this.viewList = !! options.viewList;
      this.params = options.params;
      if (options.ddocInfo) {
        this.designDocs = options.ddocInfo.designDocs;
        this.ddocID = options.ddocInfo.id;
      }
    },

    establish: function() {
      var deferreds = [
        this.collection.fetch().error(function() {
          // TODO: handle error requests that slip through
          // This should just throw a notification, not break the page
          console.log("ERROR: ", arguments);
        })
      ];
      if (this.designDocs) {
        deferreds.push(this.designDocs.fetch());
      }
      return deferreds;
    },

    selectAll: function(evt){
      $("input:checkbox").attr('checked', !$(evt.target).hasClass('active'));
    },

    // TODO:: HACK::
    // Hack to grab info about the ddoc and current view to determine whether
    // or not the view has a reduce function so we can display the advanced
    // options appropriately.
    //
    // NOTE: we have this here temporarily because we have to wait for the
    // design docs to be present.
    //
    // NOTE: We should probably refactor this View out into a separate View
    // dedicated to displaying view query results.
    // If nothing else, we should at least switch to something along the lines
    // of fetchOnce to ensure we're not reloading the ddocs here in addition to
    // the sidebar.
    setDdocInfo: function() {
      if (!this.ddoc && this.designDocs) {
        this.ddoc = this.designDocs.get(this.ddocID);
      }
    },

    serialize: function() {
      this.setDdocInfo();
      var data = {
        database: this.collection,
        viewList: this.viewList,
        hasReduce: false,
        params: this.params
      };
      if (this.ddoc) {
        data.ddoc = this.ddoc;
        data.hasReduce = this.ddoc.viewHasReduce(this.collection.view);
      }
      return data;
    },

    updateView: function(event) {
      event.preventDefault();
      var $form = $(event.currentTarget);

      // Ignore params without a value
      var params = _.filter($form.serializeArray(), function(param) {
        return param.value;
      });

      // Validate *key* params to ensure they're valid JSON
      var keyParams = ["key","keys","startkey","endkey"];
      var errorParams = _.filter(params, function(param) {
        if (_.contains(keyParams, param.name)) {
          try {
            JSON.parse(param.value);
            return false;
          } catch(e) {
            return true;
          }
        } else {
          return false;
        }
      });

      if (_.any(errorParams)) {
        _.map(errorParams, function(param) {

          // TODO: Where to add this error?
          // bootstrap wants the error on a control-group div, but we're not using that
          //$('form.view-query-update input[name='+param+'], form.view-query-update select[name='+param+']').addClass('error');

          return FauxtonAPI.addNotification({
            msg: "JSON Parse Error on field: "+param.name,
            type: "error",
            selector: ".view.show .errors-container"
          });
        });

        FauxtonAPI.addNotification({
          msg: "Make sure that strings are properly quoted and any other values are valid JSON structures",
          type: "warning",
          selector: ".view.show .errors-container"
        });

        return false;
      }

      var fragment = window.location.hash.replace(/\?.*$/, '');
      fragment = fragment + '?' + $.param(params);
      FauxtonAPI.navigate(fragment);
    },

    updateFilters: function(event) {
      event.preventDefault();
      var $ele = $(event.currentTarget);
      var name = $ele.attr('name');
      this.updateFiltersFor(name, $ele);
    },

    updateFiltersFor: function(name, $ele) {
      var $form = $ele.parents("form.view-query-update:first");
      switch (name) {
        // Reduce constraints
        //   - Can't include_docs for reduce=true
        //   - can't include group_level for reduce=false
        case "reduce":
          if ($ele.prop('checked') === true) {
            if ($form.find("input[name=include_docs]").prop("checked") === true) {
              $form.find("input[name=include_docs]").prop("checked", false);
              var notification = FauxtonAPI.addNotification({
                msg: "include_docs has been disabled as you cannot include docs on a reduced view",
                type: "warn",
                selector: ".view.show .errors-container"
              });
            }
            $form.find("input[name=include_docs]").prop("disabled", true);
            $form.find("select[name=group_level]").prop("disabled", false);
          } else {
            $form.find("select[name=group_level]").prop("disabled", true);
            $form.find("input[name=include_docs]").prop("disabled", false);
          }
          break;
        case "include_docs":
          break;
      }
    },

    /*
     * TODO: this should be reconsidered
     * This currently performs delete operations on the model level,
     * when we could be using bulk docs with _deleted = true. Using
     * individual models is cleaner from a backbone standpoint, but
     * not from the couchdb api.
     * Also, the delete method is naive and leaves the body intact,
     * when we should switch the doc to only having id/rev/deleted.
     */
    bulkDelete: function() {
      var that = this;
      // yuck, data binding ftw?
      var eles = this.$el.find("input.row-select:checked").parents("tr.all-docs-item").map(function(e) { return $(this).attr("data-id"); }).get();

      if (!window.confirm("Are you sure you want to delete these " + eles.length + " docs?")) {
        return false;
      }

      _.each(eles, function(ele) {
        var model = this.collection.get(ele);

        model.destroy().then(function(resp) {
          that.rows[ele].$el.fadeOut();

          model.collection.remove(model.id);
        }, function(resp) {
          FauxtonAPI.addNotification({
            msg: "Failed to destroy your doc!",
            type: "error"
          });
        });
      }, this);
    },

    beforeRender: function() {
      this.collection.each(function(doc) {
        this.rows[doc.id] = this.insertView("table.all-docs tbody", new this.nestedView({
          model: doc
        }));
      }, this);
    },

    afterRender: function(){
      prettyPrint();
      if (this.params) {
        var $form = this.$el.find("form.view-query-update");
        _.each(this.params, function(val, key) {
          var $ele;
          switch (key) {
            case "limit":
            case "group_level":
              $form.find("select[name='"+key+"']").val(val);
              break;
            case "include_docs":
              $form.find("input[name='"+key+"']").prop('checked', true);
              break;
            case "reduce":
              $ele = $form.find("input[name='"+key+"']");
              if (val == "true") {
                $ele.prop('checked', true);
              }
              this.updateFiltersFor(key, $ele);
              break;
            default:
              $form.find("input[name='"+key+"']").val(val);
              break;
          }
        }, this);
      }
    }
  });

  Views.Doc = FauxtonAPI.View.extend({
    template: "templates/documents/doc",

    events: {
      "click button.save-doc": "saveDoc"
    },

    updateValues: function() {
      var notification;
      if (this.model.changedAttributes()) {
        notification = FauxtonAPI.addNotification({
          msg: "Document saved successfully.",
          type: "success",
          clear: true
        });
        this.editor.setValue(this.model.prettyJSON());
      }
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
        this.model.save().error(function(xhr) {
          var responseText = JSON.parse(xhr.responseText).reason;
          notification = FauxtonAPI.addNotification({
            msg: "Save failed: " + responseText,
            type: "error",
            clear: true
          });
        });
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
      this.model.on("sync", this.updateValues, this);
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
    template: "templates/documents/doc_field_editor",

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

  Views.ViewEditor = FauxtonAPI.View.extend({
    template: "templates/documents/view_editor",

    events: {
      "click button.save": "saveView",
      "change select#reduce-function-selector": "updateReduce"
    },

    langTemplates: {
      "javascript": {
        map: "function(doc) {\n  emit(null, doc);\n}",
        reduce: "function(keys, values, rereduce){\n  if (rereduce){\n    return sum(values);\n  } else {\n    return values.length;\n  }\n}"
      }
    },

    defaultLang: "javascript",

    initialize: function(options) {
      this.ddocs = options.ddocs;
    },

    updateValues: function() {
      var notification;
      if (this.model.changedAttributes()) {
        notification = FauxtonAPI.addNotification({
          msg: "Document saved successfully.",
          type: "success",
          clear: true
        });
        this.editor.setValue(this.model.prettyJSON());
      }
    },

    updateReduce: function(event) {
      var $ele = $("#reduce-function-selector");
      var $reduceContainer = $(".control-group.reduce-function");
      if ($ele.val() == "CUSTOM") {
        $reduceContainer.show();
      } else {
        $reduceContainer.hide();
      }
    },

    establish: function() {
      return [this.ddocs.fetch(), this.model.fetch()];
    },

    saveView: function(event) {
      var json, notification;
      if (this.hasValidCode()) {
        var mapVal = this.mapEditor.getValue();
        var reduceVal = this.reduceEditor.getValue();
        notification = FauxtonAPI.addNotification({
          msg: "Saving document.",
          selector: "#define-view .errors-container"
        });
        /*
        this.model.save().error(function(xhr) {
          var responseText = JSON.parse(xhr.responseText).reason;
          notification = FauxtonAPI.addNotification({
            msg: "Save failed: " + responseText,
            type: "error",
            clear: true
          });
        });
        */
      } else {
        notification = FauxtonAPI.addNotification({
          msg: "Please fix the JSON errors and try again.",
          type: "error",
          selector: "#define-view .errors-container"
        });
      }
    },

    isCustomReduceEnabled: function() {
      return $("#reduce-function-selector").val() == "CUSTOM";
    },

    reduceVal: function() {
    },

    hasValidCode: function() {
      return _.every(["mapEditor", "reduceEditor"], function(editorName) {
        var editor = this[editorName];
        if (editorName == "reduceEditor" && ! this.isCustomReduceEnabled()) {
          return true;
        } else if (JSHINT(editor.getValue()) !== false) {
          return true;
        } else {
          // By default CouchDB view functions don't pass lint
          return _.every(JSHINT.errors, function(error) {
            return FauxtonAPI.isIgnorableError(error.reason);
          });
        }
      }, this);
    },

    runJSHint: function(editorName) {
      var editor = this[editorName];
      var json = editor.getValue();
      var output = JSHint(json);

      // Clear existing markers
      for (var i = 0, l = editor.lineCount(); i < l; i++) {
        editor.clearMarker(i);
      }

      if (output === false) {
        _.map(JSHint.errors, function(error) {
          // By default CouchDB view functions don't pass lint
          if (FauxtonAPI.isIgnorableError(error.reason)) return true;

          var line = error.line - 1;
          var className = "view-code-error-line-" + line;
          editor.setMarker(line, "●", "view-code-error "+className);

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
        database: this.model,
        ddocs: this.ddocs
      };
    },

    afterRender: function() {
      this.model.on("sync", this.updateValues, this);
      var that = this;
      var mapFun = $("#map-function");
      mapFun.val(this.langTemplates[this.defaultLang].map);
      var reduceFun = $("#reduce-function");
      reduceFun.val(this.langTemplates[this.defaultLang].reduce);
      this.mapEditor = Codemirror.fromTextArea(mapFun.get()[0], {
        mode: "javascript",
        lineNumbers: true,
        matchBrackets: true,
        lineWrapping: true,
        onChange: function() {
          that.runJSHint("mapEditor");
        },
        extraKeys: {
          "Ctrl-S": function(instance) { that.saveDoc(); },
          "Ctrl-/": "undo"
        }
      });
      this.reduceEditor = Codemirror.fromTextArea(reduceFun.get()[0], {
        mode: "javascript",
        lineNumbers: true,
        matchBrackets: true,
        lineWrapping: true,
        onChange: function() {
          that.runJSHint("reduceEditor");
        },
        extraKeys: {
          "Ctrl-S": function(instance) { that.saveDoc(); },
          "Ctrl-/": "undo"
        }
      });
      // HACK: this should be in the html
      // but CodeMirror's head explodes and it won't set the hight properly.
      // So render it first, set the editor, then hide.
      $(".control-group.reduce-function").hide();
    }
  });

  Views.Sidebar = FauxtonAPI.View.extend({
    template: "templates/documents/sidebar",
    events: {
      "click a.new#index": "newIndex",
      // "click .nav-list.views a.toggle-view": "toggleView",
      "click .nav-list a.toggle-view#all-docs": "toggleView",
      "click .nav-list a.toggle-view#design-docs": "toggleView"
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
      $.contribute(
        'Create a new view.',
        'app/addons/documents/views.js'
      );
    },

    toggleView: function(event){
      event.preventDefault();
      $.contribute(
        'Filter data by type or view',
        'app/addons/databases/views.js'
      );
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
        }
      }, this);
    }

  });

  Views.Indexed = FauxtonAPI.View.extend({});

  Views.Changes = FauxtonAPI.View.extend({
    template: "templates/documents/changes",

    establish: function() {
      return [
        this.model.changes.fetch()
      ];
    },

    serialize: function () {
      return {
        changes: this.model.changes.toJSON(),
        database: this.model
      };
    }

  });


  return Views;
});
