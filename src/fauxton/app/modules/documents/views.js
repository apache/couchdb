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
       "resizeColumns",

       // Plugins
       "plugins/prettify"

],

function(app, FauxtonAPI, Components, Documents, pouchdb, resizeColumns) {
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
        that.$el.fadeOut(function () {
          that.remove();
        });

        that.model.collection.remove(that.model.id);
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

    events: {
      "click button.delete": "destroy"
    },

    destroy: function (event) {
      event.preventDefault(); 
      
      window.alert('Cannot delete a document generated from a view.');
    },

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

  Views.AllDocsNumber = FauxtonAPI.View.extend({
    template: "templates/documents/all_docs_number",

    initialize: function (options) {
      this.newView = options.newView || false;
      
      this.listenTo(this.collection, 'totalRows:decrement', this.render);
    },

    serialize: function () {
       var totalRows = 0,
          recordStart = 0,
          updateSeq = false;

      if (!this.newView) {
        totalRows = this.collection.totalRows();
        updateSeq = this.collection.updateSeq();
      }

      recordStart = this.collection.recordStart();

      return {
        database: this.collection.database.id,
        updateSeq: updateSeq,
        offset: recordStart,
        totalRows: totalRows,
        numModels: this.collection.models.length + recordStart - 1,
      };
    }

  });

  Views.AllDocsLayout = FauxtonAPI.View.extend({
    template: "templates/documents/all_docs_layout",
    className: "row",

    initialize: function (options) {
      this.database = options.database;
      this.params = options.params;
    },

    events: {
      'click #toggle-query': "toggleQuery"
    },

    toggleQuery: function (event) {
      this.$('#query').toggle('fast');
    },

    beforeRender: function () {
      this.advancedOptions = this.insertView('#query', new Views.AdvancedOptions({
        updateViewFn: this.updateView,
        previewFn: this.previewView,
        hasReduce: false,
        showPreview: false,
        database: this.database
      }));

      this.$('#query').hide();
    },

    afterRender: function () {
      if (this.params) {
        this.advancedOptions.updateFromParams(this.params);
      }

    },

    updateView: function (event, paramInfo) {
      event.preventDefault();

      var errorParams = paramInfo.errorParams,
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

      FauxtonAPI.triggerRouteEvent('updateAllDocs', {allDocs: true});
    },

    previewView: function (event) {
      event.preventDefault();
    }

  });

  // TODO: Rename to reflect that this is a list of rows or documents
  Views.AllDocsList = FauxtonAPI.View.extend({
    template: "templates/documents/all_docs_list",
    events: {
      "click button.all": "selectAll",
      "click button.bulk-delete": "bulkDelete",
      "click #collapse": "collapse",
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
      this.expandDocs = true;
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
      var requestDuration = false;

      if (this.collection.requestDurationInString) {
        requestDuration = this.collection.requestDurationInString();
      }

      return {
        viewList: this.viewList,
        requestDuration: requestDuration,
        expandDocs: this.expandDocs
      };
    },

    collapse: function (event) {
      event.preventDefault();

      if (this.expandDocs) {
        this.expandDocs = false;
      } else {
        this.expandDocs = true;
      }

      this.render();
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
      var eles = this.$el.find("input.row-select:checked")
                         .parents("tr.all-docs-item")
                         .map(function(e) { return $(this).attr("data-id"); })
                         .get();

      if (!window.confirm("Are you sure you want to delete these " + eles.length + " docs?")) {
        return false;
      }

      _.each(eles, function(ele) {
        var model = this.collection.get(ele);

        model.destroy().then(function(resp) {
          that.rows[ele].$el.fadeOut(function () {
            $(this).remove();
          });

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
      this.allDocsNumber = this.setView('#item-numbers', new Views.AllDocsNumber({
        collection: this.collection,
        newView: this.newView
      }));

      this.insertView('#documents-pagination', this.pagination);
      var docs = this.expandDocs ? this.collection : this.collection.simple();

      docs.each(function(doc) {
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
      "click button.upload": "upload",
      "click button.cancel-button": "goback"
    },
    disableLoader: true,
    initialize: function (options) {
      this.database = options.database;
      _.bindAll(this);
    },
    goback: function(){
      window.history.back();
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
      var errors = this.editor.getAnnotations();
      return errors.length === 0;
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
      var saveDoc = this.saveDoc;

      this.editor = new Components.Editor({
        editorId: "editor-container",
        commands: [{
          name: 'save',
          bindKey: {win: 'Ctrl-S',  mac: 'Ctrl-S'},
          exec: function(editor) {
            saveDoc();
          },
          readOnly: true // false if this command should not apply in readOnly mode
        }]
      });
      this.editor.render();
      this.model.on("sync", this.updateValues, this);
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

  Views.AdvancedOptions = FauxtonAPI.View.extend({
    template: "templates/documents/advanced_options",
    className: "advanced-options well",

    initialize: function (options) {
      this.database = options.database;
      this.ddocName = options.ddocName;
      this.viewName = options.viewName;
      this.updateViewFn = options.updateViewFn;
      this.previewFn = options.previewFn;
      this.hadReduce = options.hasReduce || true;

      if (typeof(options.hasReduce) === 'undefined') {
        this.hasReduce = true;
      } else {
        this.hasReduce = options.hasReduce;
      }

      if (typeof(options.showPreview) === 'undefined') {
        this.showPreview = true;
      } else {
        this.showPreview = options.showPreview;
      }
    },

    events: {
      "change form.view-query-update input": "updateFilters",
      "change form.view-query-update select": "updateFilters",
      "submit form.view-query-update": "updateView",
      "click button.preview": "previewView"
    },

    beforeRender: function () {
      if (this.viewName && this.ddocName) {
        var buttonViews = FauxtonAPI.getExtensions('advancedOptions:ViewButton');
        _.each(buttonViews, function (view) {
          this.insertView('#button-options', view);
          view.update(this.database, this.ddocName, this.viewName);
        }, this);
      }
    },

    queryParams: function () {
      var $form = this.$(".view-query-update");
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

    updateFromParams: function (params) {
      var $form = this.$el.find("form.view-query-update");
      _.each(params, function(val, key) {
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
    },

    updateView: function (event) {
      this.updateViewFn(event, this.queryParams());
    },

    previewView: function (event) {
      this.previewFn(event, this.queryParams());
    },

    serialize: function () {
      return {
        hasReduce: this.hasReduce,
        showPreview: this.showPreview
      };
    }
  });

  Views.DesignDocSelector = FauxtonAPI.View.extend({
    template: "templates/documents/design_doc_selector",

    events: {
      "change select#ddoc": "updateDesignDoc"
    },

    initialize: function (options) {
      this.ddocName = options.ddocName;
      this.database = options.database;
      this.listenTo(this.collection, 'add', this.ddocAdded);
    },

    ddocAdded: function (ddoc) {
      this.ddocName = ddoc.id;
      this.render();
    },

    serialize: function () {
      return {
        ddocName: this.ddocName,
        ddocs: this.collection
      };
    },

    updateDesignDoc: function () {
      if (this.$('#ddoc :selected').prop('id') === 'new-doc') {
        this.$('#new-ddoc-section').show();
      } else {
        this.$('#new-ddoc-section').hide();
      }
    },

    newDesignDoc: function () {
      return this.$('#ddoc :selected').prop('id') === 'new-doc';
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
        return this.collection.find(function (ddoc) {
          return ddoc.id === ddocName;
        }).dDocModel();
      }
    }
  });

  Views.ViewEditor = FauxtonAPI.View.extend({
    template: "templates/documents/view_editor",
    builtinReduces: ['_sum', '_count', '_stats'],

    events: {
      "click button.save": "saveView",
      "click button.delete": "deleteView",
      "change select#reduce-function-selector": "updateReduce",
      
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

      _.bindAll(this);
    },

    establish: function () {
      if (this.ddocInfo) {
        return this.ddocInfo.fetch();
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
        this.createReduceEditor();
        $reduceContainer.show();
      } else {
        $reduceContainer.hide();
      }
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

    saveView: function(event) {
      var json, notification,
      that = this;

      if (event) { event.preventDefault();}

      if (this.hasValidCode() && this.$('#new-ddoc:visible').val() !=="") {
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
        var errormessage = (this.$('#new-ddoc:visible').val() ==="")?"Enter a design doc name":"Please fix the Javascript errors and try again.";
        notification = FauxtonAPI.addNotification({
          msg: errormessage,
          type: "error",
          selector: "#define-view .errors-container"
        });
      }
    },

    previewView: function(event, paramsInfo) {
      var that = this,
      mapVal = this.mapEditor.getValue(),
      reduceVal = this.reduceVal(),
      paramsArr = paramsInfo.params;

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

    getCurrentDesignDoc: function () {
      return this.designDocSelector.getCurrentDesignDoc();
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
        if (editorName === "reduceEditor" && ! this.isCustomReduceEnabled()) {
          return true;
        } 
        return editor.hadValidCode();
      }, this);
    },

    toggleIndexNav: function (event) {
      var $index = this.$('#index'),
          $targetId = this.$(event.target).attr('id');

      if ($targetId === 'index-nav') {
        if (this.newView) { return; }
        $index.toggle('slow');
        this.showEditors();
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

    createReduceEditor: function () {
      if (this.reduceEditor) {
        this.reduceEditor.remove();
      }

      this.reduceEditor = new Components.Editor({
        editorId: "reduce-function",
        mode: "javascript",
        couchJSHINT: true
      });
      this.reduceEditor.render();
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

      this.designDocSelector = this.setView('.design-doc-group', new Views.DesignDocSelector({
        collection: this.ddocs,
        ddocName: this.model.id,
        database: this.database
      }));

      this.advancedOptions = this.insertView('#query', new Views.AdvancedOptions({
        updateViewFn: this.updateView,
        previewFn: this.previewView,
        database: this.database,
        viewName: this.viewName,
        ddocName: this.model.id
      }));
    },

    afterRender: function() {
      if (this.params) {
        this.advancedOptions.updateFromParams(this.params);
      }

      this.designDocSelector.updateDesignDoc();
      if (this.newView) {
        this.showEditors();
      } else {
        this.$('#index').hide();
        this.$('#index-nav').parent().removeClass('active');
      }
    },

    showEditors: function () {
      this.mapEditor = new Components.Editor({
        editorId: "map-function",
        mode: "javascript",
        couchJSHINT: true
      });
      this.mapEditor.render();

      if (this.hasCustomReduce()) {
        this.createReduceEditor();
      } else {
        $(".control-group.reduce-function").hide();
      }

      if (this.newView) {
        this.mapEditor.setValue(this.langTemplates[this.defaultLang].map);
        this.reduceEditor.setValue(this.langTemplates[this.defaultLang].reduce);
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
      "click #jump-to-doc-label": "jumpToDoc"
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
      "click button#delete-database": "deleteDatabase"
    },

    initialize: function(options) {
      this.database = options.database;
      this.showNewView = true;
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
      var docLinks = FauxtonAPI.getExtensions('docLinks');
      return {
        changes_url: '#' + this.database.url('changes'),
        permissions_url: '#' + this.database.url('app') + '/permissions',
        db_url: '#' + this.database.url('index') + '?limit=100',
        database: this.collection.database,
        database_url: '#' + this.database.url('app'), 
        docLinks: docLinks,
        showNewView: this.showNewView
      };
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
    },

    toggleNewView: function (show) {
      // only render if there is a change
      if (show !== this.showNewView) {
        this.showNewView = show;
        this.render();
      }
    },
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
