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
       "modules/fauxton/components",

       "modules/documents/resources",
       "modules/pouchdb/base",

       // Libs
       "codemirror",
       "jshint",
       "resizeColumns",

       // Plugins
       "plugins/codemirror-javascript",
       "plugins/prettify"

],

function(app, FauxtonAPI, Components, Documents, pouchdb, Codemirror, JSHint, resizeColumns) {
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
        db_url: '#' + this.database.url('index') + '?limit=100',
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
        app.router.navigate('#/_all_dbs', {trigger: true});
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
          url += '?startkey="' + searchbox.val() + '"';
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

  Views.UploadModal = FauxtonAPI.View.extend({
    template: "templates/documents/upload_modal",

    disableLoader: true,
    
    initialize: function (options) {
      _.bindAll(this);
    },

    events: {
      "click a#upload-btn": "uploadFile"
    },

    uploadFile: function (event) {
      event.preventDefault();

      var docRev = this.model.get('_rev'),
          that = this,
          $form = this.$('#file-upload');

      if (!docRev) {
        return this.set_error_msg('The document needs to be saved before adding an attachment.');
      }

      if ($('input[type="file"]')[0].files.length === 0) {
        return this.set_error_msg('Selected a file to be uploaded.');
      }

      this.$('#_rev').val(docRev);

      $form.ajaxSubmit({
        url: this.model.url(),
        type: 'POST',
        beforeSend: this.beforeSend,
        uploadProgress: this.uploadProgress,
        success: this.success,
        error: function (resp) {
          console.log('ERR on upload', resp);
          return that.set_error_msg('Could not upload document: ' + JSON.parse(resp.responseText).reason);
        }
      });
    },

    success: function (resp) {
      var hideModal = this.hideModal,
          $form = this.$('#file-upload');

      FauxtonAPI.triggerRouteEvent('reRenderDoc');
      //slight delay to make this transistion a little more fluid and less jumpy
      setTimeout(function () {
        $form.clearForm();
        hideModal();
        $('.modal-backdrop').remove();
      }, 1000);
    },

    uploadProgress: function(event, position, total, percentComplete) {
      this.$('.bar').css({width: percentComplete + '%'});
    },

    beforeSend: function () {
      this.$('.progress').removeClass('hide');
    },

    showModal: function () {
      this.$('.bar').css({width: '0%'});
      this.$('.progress').addClass('hide');
      this.clear_error_msg();
      this.$('.modal').modal();
      // hack to get modal visible 
      $('.modal-backdrop').css('z-index',1025);
    },

    hideModal: function () {
      this.$('.modal').modal('hide');
    },

    set_error_msg: function (msg) {
      var text;
      if (typeof(msg) == 'string') {
        text = msg;
      } else {
        text = JSON.parse(msg.responseText).reason;
      }
      this.$('#modal-error').text(text).removeClass('hide');
    },

    clear_error_msg: function () {
      this.$('#modal-error').text(' ').addClass('hide');
    },

    serialize: function () {
      return this.model.toJSON();
    }
  });

  Views.DuplicateDocModal = FauxtonAPI.View.extend({
    template: "templates/documents/duplicate_doc_modal",

    initialize: function () {
      _.bindAll(this);
    },

    events: {
      "click #duplicate-btn":"duplicate"

    },

    duplicate: function (event) {
      event.preventDefault();
      var newId = this.$('#dup-id').val();

      this.hideModal();
      FauxtonAPI.triggerRouteEvent('duplicateDoc', newId);
    },

    _showModal: function () {
      this.$('.bar').css({width: '0%'});
      this.$('.progress').addClass('hide');
      this.clear_error_msg();
      this.$('.modal').modal();
      // hack to get modal visible 
      $('.modal-backdrop').css('z-index',1025);
    },

    showModal: function () {
      var showModal = this._showModal,
          setDefaultIdValue = this.setDefaultIdValue,
          uuid = new FauxtonAPI.UUID();

      uuid.fetch().then(function () {
        setDefaultIdValue(uuid.next());
        showModal();
      });
    },

    setDefaultIdValue: function (id) {
      this.$('#dup-id').val(id);
    },

    hideModal: function () {
      this.$('.modal').modal('hide');
    },

    set_error_msg: function (msg) {
      var text;
      if (typeof(msg) == 'string') {
        text = msg;
      } else {
        text = JSON.parse(msg.responseText).reason;
      }
      this.$('#modal-error').text(text).removeClass('hide');
    },

    clear_error_msg: function () {
      this.$('#modal-error').text(' ').addClass('hide');
    },

    serialize: function () {
      return this.model.toJSON();
    }

  });

  Views.FieldEditorTabs = FauxtonAPI.View.extend({
    template: "templates/documents/doc_field_editor_tabs",
    disableLoader: true,
    initialize: function(options) {
      this.selected = options.selected;
    },

    events: {
    },
    updateSelected: function (selected) {
      this.selected = selected;
      this.$('.active').removeClass('active');
      this.$('#'+this.selected).addClass('active');
    },

    serialize: function() {
      var selected = this.selected;
      return {
        doc: this.model,
        isNewDoc: this.model.isNewDoc(),
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
      this.selected = !! options.selected;
    },

    serialize: function() {
      return {
        index: this.index,
        ddoc: this.ddoc,
        database: this.database,
        selected: this.selected
      };
    },

    afterRender: function() {
      if (this.selected) {
        $("#sidenav ul.nav-list li").removeClass("active");
        this.$el.addClass("active");
      }
    }
  });

  // TODO: Rename to reflect that this is a list of rows or documents
  Views.AllDocsList = FauxtonAPI.View.extend({
    template: "templates/documents/all_docs_list",
    events: {
      "click button.all": "selectAll",
      "click button.bulk-delete": "bulkDelete",
      "change .row-select":"toggleTrash"
    },

    toggleTrash: function () {
      if (this.$('.row-select:checked').length > 0) {
        this.$('.bulk-delete').removeClass('disabled');
      } else {
        this.$('.bulk-delete').addClass('disabled');
      }
    },

    initialize: function(options){
      this.nestedView = options.nestedView || Views.Document;
      this.rows = {};
      this.viewList = !! options.viewList;
      this.database = options.database;
      if (options.ddocInfo) {
        this.designDocs = options.ddocInfo.designDocs;
        this.ddocID = options.ddocInfo.id;
      }
      this.newView = options.newView || false;
      this.addPagination();
    },

    establish: function() {
      if (this.newView) { return null; }

      return this.collection.fetch({reset: true}).fail(function() {
        // TODO: handle error requests that slip through
        // This should just throw a notification, not break the page
        console.log("ERROR: ", arguments);
      });
    },

    selectAll: function(evt){
      $("input:checkbox").prop('checked', !$(evt.target).hasClass('active'));
    },

    serialize: function() {
      var totalRows = 0,
          recordStart = 0,
          updateSeq = false;

      if (!this.newView) {
        totalRows = this.collection.totalRows();
        updateSeq = this.collection.updateSeq();
      }

      recordStart = this.collection.recordStart();

      var info = {
        database: this.collection.database.id,
        updateSeq: updateSeq,
        offset: recordStart,
        totalRows: totalRows,
        numModels: this.collection.models.length + recordStart - 1,
        viewList: this.viewList,
        requestDuration: null
      };

      if (this.collection.requestDurationInString) {
        info.requestDuration = this.collection.requestDurationInString();
      }

      return info;
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
          that.$('.bulk-delete').addClass('disabled');
        }, function(resp) {
          FauxtonAPI.addNotification({
            msg: "Failed to destroy your doc!",
            type: "error"
          });
        });
      }, this);
    },

    addPagination: function () {
      var collection = this.collection;

      this.pagination = new Components.IndexPagination({
        collection: this.collection,
        scrollToSelector: '#dashboard-content',
        previousUrlfn: function () {
          return collection.urlPreviousPage(20, this.previousIds.pop());
        },
        canShowPreviousfn: function () {
          if (collection.viewMeta.offset === 0) {
            return false;
          }

          return true;
        },
        canShowNextfn: function () {
          if (collection.length === 0 || (collection.viewMeta.offset + collection.length + 2) >= collection.viewMeta.total_rows) {
            return false;
          }

          return true;
        },
        
        nextUrlfn: function () {
          return collection.urlNextPage(20);
        }
      });
    },

    beforeRender: function() {
      this.insertView('#documents-pagination', this.pagination);
      this.collection.each(function(doc) {
        this.rows[doc.id] = this.insertView("table.all-docs tbody", new this.nestedView({
          model: doc
        }));
      }, this);
    },

    afterRender: function(){
      prettyPrint();
    }
  });

  Views.Doc = FauxtonAPI.View.extend({
    template: "templates/documents/doc",
    events: {
      "click button.save-doc": "saveDoc",
      "click button.delete": "destroy",
      "click button.duplicate": "duplicate",
      "click button.upload": "upload"
    },
    disableLoader: true,
    initialize: function (options) {
      this.database = options.database;
    },
    destroy: function(event) {
      if (this.model.isNewDoc()) {
        FauxtonAPI.addNotification({
          msg: 'This document has not been saved yet.',
          type: 'warning'
        });
        return;
      }

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

    beforeRender: function () {
      this.uploadModal = this.setView('#upload-modal', new Views.UploadModal({model: this.model}));
      this.uploadModal.render();

      this.duplicateModal = this.setView('#duplicate-modal', new Views.DuplicateDocModal({model: this.model}));
      this.duplicateModal.render();
    },

    upload: function (event) {
      event.preventDefault();
      if (this.model.isNewDoc()) {
        FauxtonAPI.addNotification({
          msg: 'Please save the document before uploading an attachment.',
          type: 'warning'
        });
        return;
      }
      this.uploadModal.showModal();
    },

    duplicate: function(event) {
      if (this.model.isNewDoc()) {
        FauxtonAPI.addNotification({
          msg: 'Please save the document before duplicating it.',
          type: 'warning'
        });
        return;
      }
      event.preventDefault();
      this.duplicateModal.showModal();
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
      var promise = this.model.fetch(),
          databaseId = this.database.id,
          deferred = $.Deferred();

      promise.then(function () {
        deferred.resolve();
      }, function (xhr, reason, msg) {
        if (xhr.status === 404) {
          FauxtonAPI.addNotification({
            msg: 'The document does not exist',
            type: 'error',
            clear: true
          });
          FauxtonAPI.navigate('/database/' + databaseId + '/_all_docs?limit=20');
        }
        deferred.reject();
     });
      
      return deferred;
    },

    saveDoc: function(event) {
      var json, notification, 
      that = this,
      validDoc = this.getDocFromEditor();

      if (validDoc) {
        this.getDocFromEditor();

        notification = FauxtonAPI.addNotification({msg: "Saving document."});
        console.log('save',this.model);

        this.model.save().then(function () {
          FauxtonAPI.navigate('/database/' + that.database.id + '/' + that.model.id);
        }).fail(function(xhr) {
          var responseText = JSON.parse(xhr.responseText).reason;
          notification = FauxtonAPI.addNotification({
            msg: "Save failed: " + responseText,
            type: "error",
            clear: true,
            selector: "#doc .errors-container"
          });
        });
      } else if(this.model.validationError && this.model.validationError === 'Cannot change a documents id.') {
          notification = FauxtonAPI.addNotification({
            msg: "Cannot save: " + 'Cannot change a documents _id, try Duplicate doc instead!',
            type: "error",
            selector: "#doc .errors-container"
          });
        delete this.model.validationError;
      } else {
        notification = FauxtonAPI.addNotification({
          msg: "Please fix the JSON errors and try again.",
          type: "error",
          selector: "#doc .errors-container"
        });
      }
    },

    getDocFromEditor: function () {
      if (!this.hasValidCode()) {
        return false;
      }

      json = JSON.parse(this.editor.getValue());
      this.model.set(json, {validate: true});
      if (this.model.validationError) {
        return false;
      }

      return this.model;
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
              title: "ERROR: " + error.reason,
              container: 'body'
            });
          }, 0);
        }, this);
      }
    },

    serialize: function() {
      return {
        doc: this.model,
        attachments: this.getAttachments()
      };
    },

    getAttachments: function () {
      var attachments = this.model.get('_attachments');

      if (!attachments) { return false; }

      return _.map(attachments, function (att, key) {
        return {
          fileName: key,
          size: att.length,
          contentType: att.content_type,
          url: this.model.url() + '/' + key
        };
      }, this);
    },

    afterRender: function() {
      this.model.on("sync", this.updateValues, this);
      var that = this;
      if ($('.CodeMirror').length > 0){
        $('.CodeMirror').remove();
      }
      this.editor = Codemirror.fromTextArea(this.$el.find("textarea.doc-code").get()[0], {
        mode: "application/json",
        json: false,
        lineNumbers: true,
        matchBrackets: true,
        lineWrapping: true,
        onChange: function() {
          try {
            that.runJSHint();
          } catch (e) {
            console.log('ERROR for jshint',e);
          }
        },
        extraKeys: {
          "Ctrl-S": function(instance) { that.saveDoc(); },
          "Ctrl-/": "undo"
        }
      });
      setTimeout(function(){that.editor.setSize(null,$('#dashboard').outerHeight()-295);},200);
    }
  });

  Views.DocFieldEditor = FauxtonAPI.View.extend({
    template: "templates/documents/doc_field_editor",
    disableLoader: true,
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
        doc: this.getModelWithoutAttachments(),
        attachments: this.getAttachments()
      };
    },

    getModelWithoutAttachments: function() {
      var model = this.model.toJSON();
      delete model._attachments;
      return model;
    },

    getAttachments: function () {
      var attachments = this.model.get('_attachments');

      if (!attachments) { return []; }

      return _.map(attachments, function (att, key) {
        return {
          fileName: key,
          size: att.length,
          contentType: att.content_type,
          url: this.model.url() + '/' + key
        };
      }, this);
    },

    establish: function() {
      return [this.model.fetch()];
    }
  });

  //TODO split this into two smaller views, one for advance query options and other for index editing
  Views.ViewEditor = FauxtonAPI.View.extend({
    template: "templates/documents/view_editor",
    builtinReduces: ['_sum', '_count', '_stats'],

    events: {
      "click button.save": "saveView",
      "click button.preview": "previewView",
      "click button.delete": "deleteView",
      "change select#reduce-function-selector": "updateReduce",
      "change form.view-query-update input": "updateFilters",
      "change form.view-query-update select": "updateFilters",
      "change select#ddoc": "updateDesignDoc",
      "submit form.view-query-update": "updateView",
      "click #db-views-tabs-nav": 'toggleIndexNav'
    },

    langTemplates: {
      "javascript": {
        map: "function(doc) {\n  emit(doc._id, 1);\n}",
        reduce: "function(keys, values, rereduce){\n  if (rereduce){\n    return sum(values);\n  } else {\n    return values.length;\n  }\n}"
      }
    },

    defaultLang: "javascript",

    initialize: function(options) {
      this.newView = options.newView || false;
      this.ddocs = options.ddocs;
      this.params = options.params;
      this.database = options.database;
      if (this.newView) {
        this.viewName = 'newView';
      } else {
        this.ddocID = options.ddocInfo.id;
        this.viewName = options.viewName;
        this.ddocInfo = new Documents.DdocInfo({_id: this.ddocID},{database: this.database});
      } 
    },

    establish: function () {
      if (this.ddocInfo) {
        return this.ddocInfo.fetch();
      }
    },

    updateDesignDoc: function () {

      if (this.$('#ddoc :selected').prop('id') === 'new-doc') {
        this.$('#new-ddoc-section').show();

      } else {
        this.$('#new-ddoc-section').hide();
      }

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

    queryParams: function () {
      var $form = $(".view-query-update");
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

      return {params: params, errorParams: errorParams};
    },

    deleteView: function (event) {
      event.preventDefault();

      if (this.newView) { return alert('Cannot delete a new view.'); }
      if (!confirm('Are you sure you want to delete this view?')) {return;}

      var that = this,
          promise,
          viewName = this.$('#index-name').val(),
          ddocName = this.$('#ddoc :selected').val(),
          ddoc = this.getCurrentDesignDoc();

      ddoc.removeDdocView(viewName);

      if (ddoc.hasViews()) {
        promise = ddoc.save(); 
      } else {
        promise = ddoc.destroy();
      }

      promise.then(function () {
        FauxtonAPI.navigate('/database/' + that.database.id + '/_all_docs?limit=100');
        FauxtonAPI.triggerRouteEvent('reloadDesignDocs');
      });
    },

    updateView: function(event) {
      event.preventDefault();

      if (this.newView) { return alert('Please save this new view before querying it.'); }

      var paramInfo = this.queryParams(),
      errorParams = paramInfo.errorParams,
      params = paramInfo.params;

      if (_.any(errorParams)) {
        _.map(errorParams, function(param) {

          // TODO: Where to add this error?
          // bootstrap wants the error on a control-group div, but we're not using that
          //$('form.view-query-update input[name='+param+'], form.view-query-update select[name='+param+']').addClass('error');
          return FauxtonAPI.addNotification({
            msg: "JSON Parse Error on field: "+param.name,
            type: "error",
            selector: ".advanced-options .errors-container"
          });
        });
        FauxtonAPI.addNotification({
          msg: "Make sure that strings are properly quoted and any other values are valid JSON structures",
          type: "warning",
          selector: ".advanced-options .errors-container"
        });

        return false;
      }

      var fragment = window.location.hash.replace(/\?.*$/, '');
      fragment = fragment + '?' + $.param(params);
      FauxtonAPI.navigate(fragment, {trigger: false});

      FauxtonAPI.triggerRouteEvent('updateAllDocs', {ddoc: this.ddocID, view: this.viewName});
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
              selector: ".view.show .all-docs-list.errors-container"
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

    previewView: function(event) {
      var that = this,
      mapVal = this.mapEditor.getValue(),
      reduceVal = this.reduceVal(),
      paramsArr = this.queryParams().params;

      var params = _.reduce(paramsArr, function (params, param) {
        params[param.name] = param.value;
        return params;
      }, {reduce: false});

      event.preventDefault();

      FauxtonAPI.addNotification({
        msg: "<strong>Warning!</strong> Preview executes the Map/Reduce functions in your browser, and may behave differently from CouchDB.",
        type: "warning",
        selector: ".advanced-options .errors-container",
        fade: true
      });

      var promise = FauxtonAPI.Deferred();

      if (!this.database.allDocs) {
        this.database.buildAllDocs({limit: "100", include_docs: true});
        promise = this.database.allDocs.fetch();
      } else {
        promise.resolve();
      }

      promise.then(function () {
        params.docs = that.database.allDocs.map(function (model) { return model.get('doc');}); 

        var queryPromise = pouchdb.runViewQuery({map: mapVal, reduce: reduceVal}, params);
        queryPromise.then(function (results) {
          FauxtonAPI.triggerRouteEvent('updatePreviewDocs', {rows: results.rows, ddoc: that.getCurrentDesignDoc().id, view: that.viewName});
        });
      });
    },

    saveView: function(event) {
      var json, notification,
      that = this;

      if (event) { event.preventDefault();}

      if (this.hasValidCode()) {
        var mapVal = this.mapEditor.getValue(), 
        reduceVal = this.reduceVal(),
        viewName = this.$('#index-name').val(),
        ddoc = this.getCurrentDesignDoc(),
        ddocName = ddoc.id;

        this.viewName = viewName;

        notification = FauxtonAPI.addNotification({
          msg: "Saving document.",
          selector: "#define-view .errors-container"
        });

        ddoc.setDdocView(viewName, mapVal, reduceVal);

        ddoc.save().then(function () {
          FauxtonAPI.addNotification({
            msg: "View has been saved.",
            type: "success",
            selector: "#define-view .errors-container"
          });

          if (that.newView) {
            var fragment = '/database/' + that.database.id +'/' + ddocName + '/_view/' + viewName; 

            FauxtonAPI.navigate(fragment, {trigger: false});
            FauxtonAPI.triggerRouteEvent('reloadDesignDocs',{selectedTab: ddocName.replace('_design/','') + '_' + viewName});

            that.newView = false;
          }

          FauxtonAPI.triggerRouteEvent('updateAllDocs', {ddoc: ddocName, view: viewName});

        }, function(xhr) {
          var responseText = JSON.parse(xhr.responseText).reason;
          notification = FauxtonAPI.addNotification({
            msg: "Save failed: " + responseText,
            type: "error",
            clear: true
          });
        });
      } else {
        notification = FauxtonAPI.addNotification({
          msg: "Please fix the Javascript errors and try again.",
          type: "error",
          selector: "#define-view .errors-container"
        });
      }
    },

    getCurrentDesignDoc: function () {
      if (this.newDesignDoc()) {
        var doc = {
          _id: '_design/' + this.$('#new-ddoc').val(),
          views: {},
          language: "javascript"
        };
        return new Documents.Doc(doc, {database: this.database});
      } else {
        var ddocName = this.$('#ddoc').val();
        return this.ddocs.find(function (ddoc) {
          return ddoc.id === ddocName;
        }).dDocModel();
      }

    },

    newDesignDoc: function () {
      return this.$('#ddoc :selected').prop('id') === 'new-doc';
    },

    isCustomReduceEnabled: function() {
      return $("#reduce-function-selector").val() == "CUSTOM";
    },

    reduceVal: function() {
      var reduceOption = this.$('#reduce-function-selector :selected').val(),
      reduceVal = "";

      if (reduceOption === 'CUSTOM') {
        reduceVal = this.reduceEditor.getValue();
      } else if ( reduceOption !== 'NONE') {
        reduceVal = reduceOption;
      }

      return reduceVal;
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
            return FauxtonAPI.isIgnorableError(error.raw);
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
    toggleIndexNav: function (event) {
      var $index = this.$('#index'),
          $targetId = this.$(event.target).attr('id');

      if ($targetId === 'index-nav') {
        if (this.newView) { return; }
        $index.toggle('slow');
      } else {
        $index.removeAttr('style');
      }
    },

    serialize: function() {
      return {
        ddocs: this.ddocs,
        ddoc: this.model,
        ddocName: this.model.id,
        viewName: this.viewName,
        reduceFunStr: this.reduceFunStr,
        hasReduce: this.reduceFunStr,
        isCustomReduce: this.hasCustomReduce(),
        newView: this.newView,
        langTemplates: this.langTemplates.javascript
      };
    },

    hasCustomReduce: function() {
      return this.reduceFunStr && ! _.contains(this.builtinReduces, this.reduceFunStr);
    },

    beforeRender: function () {

      if (this.newView) {
        this.reduceFunStr = '_sum';
        if (this.ddocs.length === 0) {
          this.model = new Documents.Doc(null, {database: this.database});
        } else {
          this.model = this.ddocs.first().dDocModel();
        }
        this.ddocID = this.model.id;
      } else {
        this.model = this.ddocs.get(this.ddocID).dDocModel();
        this.reduceFunStr = this.model.viewHasReduce(this.viewName);
        this.setView('#ddoc-info', new Views.DdocInfo({model: this.ddocInfo }));
      }
    },

    afterRender: function() {
      var that = this, 
          mapFun = this.$("#map-function"),
          reduceFun = this.$("#reduce-function");

      if (this.newView) {
        mapFun.val(this.langTemplates[this.defaultLang].map);
        reduceFun.val(this.langTemplates[this.defaultLang].reduce);
      } else {
        setTimeout(function(){this.$('#index').hide();}, 300);
        this.$('#index-nav').parent().removeClass('active');
      }

      this.updateDesignDoc();
      // This is a hack around a bug in backbone.layoutmanager with grunt dev
      // When in grunt dev mode we load templates asynchronously
      // and this can cause a double render which then gives us two 
      // mapeditors
      if (this.mapViewSet) { return;}
      this.mapViewSet = true;

      this.mapEditor = Codemirror.fromTextArea(mapFun.get()[0], {
        mode: "javascript",
        lineNumbers: true,
        matchBrackets: true,
        lineWrapping: true,
        onChange: function() {
          try {
            that.runJSHint("mapEditor");
          } catch (e) {
            console.log('ERROR for jshint',e);
          }
        },
        extraKeys: {
          "Ctrl-S": function(instance) { that.saveView(); },
          "Ctrl-/": "undo"
        }
      });
      this.reduceEditor = Codemirror.fromTextArea(reduceFun.get()[0], {
        mode: "javascript",
        lineNumbers: true,
        matchBrackets: true,
        lineWrapping: true,
        onChange: function() {
          try {
            that.runJSHint("reduceEditor");
          } catch (e) {
            console.log('ERROR for jshint',e);
          }
        },
        extraKeys: {
          "Ctrl-S": function(instance) { that.saveView(); },
          "Ctrl-/": "undo"
        }
      });
      // HACK: this should be in the html
      // but CodeMirror's head explodes and it won't set the hight properly.
      // So render it first, set the editor, then hide.
      if ( ! this.hasCustomReduce()) {
        $(".control-group.reduce-function").hide();
      }

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
              case "stale":
              case "descending":
              case "inclusive_end":
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

  Views.JumpToDoc = FauxtonAPI.View.extend({
    template: "templates/documents/jumpdoc",

    initialize: function (options) {
      this.database = options.database;
    },

    events: {
      "submit #jump-to-doc": "jumpToDoc",
    },

    jumpToDoc: function (event) {
      event.preventDefault();

      var docId = this.$('#jump-to-doc-id').val();

      FauxtonAPI.navigate('/database/' + this.database.id +'/' + docId, {trigger: true});
    },

    afterRender: function () {
     this.typeAhead = new Components.DocSearchTypeahead({el: '#jump-to-doc-id', database: this.database});
     this.typeAhead.render();
    }
  });

  Views.Sidebar = FauxtonAPI.View.extend({
    template: "templates/documents/sidebar",
    events: {
      "click a.new#index": "newIndex",
      "click button#delete-database": "deleteDatabase"
    },

    initialize: function(options) {
      this.database = options.database;
      if (options.ddocInfo) {
        this.ddocID = options.ddocInfo.id;
        this.currView = options.ddocInfo.currView;
      }
    },

    deleteDatabase: function (event) {
      event.preventDefault();

      var result = confirm('Are you sure you want to delete this database?');

      if (!result) { return; }
      var databaseName = this.database.id;

      this.database.destroy().then(function () {
        FauxtonAPI.navigate('#/_all_dbs');
        FauxtonAPI.addNotification({
          msg: 'The database ' + databaseName + ' has been deleted.'
        });
      }).fail(function (rsp, error, msg) {
        FauxtonAPI.addNotification({
          msg: 'Could not delete the database, reason ' + msg + '.',
          type: 'error'
        });
      });
    },

    serialize: function() {
      return {
        changes_url: '#' + this.database.url('changes'),
        permissions_url: '#' + this.database.url('app') + '/permissions',
        db_url: '#' + this.database.url('index') + '?limit=100',
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
        var selected = this.ddocID == "_design/"+design;
        this.insertView("ul.nav." + selector, new Views.IndexItem({
          ddoc: design,
          index: key,
          database: this.collection.database.id,
          selected: selected && key == this.currView
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
    },

    afterRender: function () {
      if (this.selectedTab) {
        this.setSelectedTab(this.selectedTab);
      }
    },

    setSelectedTab: function (selectedTab) {
      this.selectedTab = selectedTab;
      this.$('li').removeClass('active');
      this.$('#' + selectedTab).parent().addClass('active');
    }

  });

  Views.Indexed = FauxtonAPI.View.extend({});

  Views.Changes = FauxtonAPI.View.extend({
    template: "templates/documents/changes",

    establish: function() {
      return [ this.model.changes.fetch()];
    },

    serialize: function () {
      return {
        changes: this.model.changes.toJSON(),
        database: this.model
      };
    },

    afterRender: function(){
      prettyPrint();
    }
  });

  Views.DdocInfo = FauxtonAPI.View.extend({
    template: "templates/documents/ddoc_info",

    initialize: function (options) {
      this.refreshTime = options.refreshTime || 5000;
      this.listenTo(this.model, 'change', this.render);
    },

    serialize: function () {
      return {
        view_index: this.model.get('view_index')
      };
    },

    afterRender: function () {
      this.startRefreshInterval();
    },

    startRefreshInterval: function () {
      var model = this.model;

      // Interval already set
      if (this.intervalId) { return ; }

      this.intervalId = setInterval(function () {
        model.fetch();
      }, this.refreshTime);
    },

    stopRefreshInterval: function () {
      clearInterval(this.intervalId);
    },

    cleanup: function () {
      this.stopRefreshInterval();
    }
  });

  Documents.Views = Views;
  return Documents;
});
