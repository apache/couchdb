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
       "addons/fauxton/components",

       "addons/documents/resources",
       "addons/databases/resources",
       "addons/pouchdb/base",

       // Libs
       "addons/fauxton/resizeColumns",

       // Plugins
       "plugins/beautify",
       "plugins/prettify",
],

function(app, FauxtonAPI, Components, Documents, Databases, pouchdb, resizeColumns, beautify) {
  var Views = {};
  Views.Tabs = FauxtonAPI.View.extend({
    template: "addons/documents/templates/tabs",
    initialize: function(options){
      this.collection = options.collection;
      this.database = options.database;
      this.active_id = options.active_id;
    },

    serialize: function () {
      return {
        // TODO make this not hard coded here
        changes_url: '#' + this.database.url('changes'),
        db_url: '#' + this.database.url('index') + '?limit=' + Databases.DocLimit,
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
    }
  });

  Views.SearchBox = FauxtonAPI.View.extend({
    template: "addons/documents/templates/search",
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

  Views.DeleteDBModal = Components.ModalView.extend({
    template: "addons/documents/templates/delete_database_modal",

    events: {
      "click #delete-db-btn": "deleteDatabase",
      "submit #delete-db-check": "deleteDatabase"
    },

    deleteDatabase: function (event) {
      event.preventDefault();
      var enterredName = this.$('#db_name')[0].value;
      if (this.database.id != enterredName) {
        this.set_error_msg(enterredName + " does not match database id - are you sure you want to delete " + this.database.id + "?");
        return;
      }
      this.hideModal();
      var databaseName = this.database.id;
      FauxtonAPI.addNotification({
        msg: "Deleting your database...",
        type: "error",
        clear: true
      });

      this.database.destroy().then(function () {
        FauxtonAPI.navigate('#/_all_dbs');
        FauxtonAPI.addNotification({
          msg: 'The database <code>' + databaseName + '</code> has been deleted.',
          clear: true
        });
      }).fail(function (rsp, error, msg) {
        FauxtonAPI.addNotification({
          msg: 'Could not delete the database, reason ' + msg + '.',
          type: 'error',
          clear: true
        });
      });
    }
  });

  Views.UploadModal = Components.ModalView.extend({
    template: "addons/documents/templates/upload_modal",

    events: {
      "click #upload-btn": "uploadFile"
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

    _showModal: function () {
      this.$('.bar').css({width: '0%'});
      this.$('.progress').addClass('hide');
    }
  });

  Views.DuplicateDocModal = Components.ModalView.extend({
    template: "addons/documents/templates/duplicate_doc_modal",

    initialize: function () {
      _.bindAll(this);
    },

    events: {
      "click #duplicate-btn":"duplicate",
      "submit #doc-duplicate": "duplicate"

    },

    duplicate: function (event) {
      event.preventDefault();
      var newId = this.$('#dup-id').val(),
          isDDoc = newId.match(/^_design\//),
          removeDDocID = newId.replace(/^_design\//,""),
          encodedID = isDDoc? "_design/"+ app.utils.safeURLName(removeDDocID):app.utils.safeURLName(newId);

      this.hideModal();
      FauxtonAPI.triggerRouteEvent('duplicateDoc', encodedID);
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
    }
  });

  Views.FieldEditorTabs = FauxtonAPI.View.extend({
    template: "addons/documents/templates/doc_field_editor_tabs",
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
    template: "addons/documents/templates/all_docs_item",
    tagName: "tr",
    className: "all-docs-item",

    events: {
      "click button.delete": "destroy",
      "dblclick pre.prettyprint": "edit"
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

    edit: function(event) {
      event.preventDefault();
      FauxtonAPI.navigate("#" + this.model.url('app'));
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
        if (!!that.model.id.match('_design')) {
          FauxtonAPI.triggerRouteEvent('reloadDesignDocs');
        }
      }, function(resp) {
        FauxtonAPI.addNotification({
          msg: "Failed to destroy your doc!",
          type: "error"
        });
      });
    }
  });

  Views.Row = FauxtonAPI.View.extend({
    template: "addons/documents/templates/index_row_docular",
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
        doc: this.model,
        url: this.model.url('app')
      };
    }
  });

  Views.IndexItem = FauxtonAPI.View.extend({
    template: "addons/documents/templates/index_menu_item",
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
        index_clean: app.utils.removeSpecialCharacters(this.index),
        ddoc_clean: app.utils.removeSpecialCharacters(this.ddoc),
        index_encoded: app.utils.safeURLName(this.index),
        ddoc_encoded: app.utils.safeURLName(this.ddoc),
        database_encoded: app.utils.safeURLName(this.database),
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
    template: "addons/documents/templates/all_docs_number",

    initialize: function (options) {
      this.newView = options.newView || false;
      this.pagination = options.pagination;
      _.bindAll(this);
      
      this._perPage = options.perPageDefault || 20;
      this.listenTo(this.collection, 'totalRows:decrement', this.render);
    },

    events: {
      'change #select-per-page': 'updatePerPage'
    },

    updatePerPage: function (event) {
      this._perPage = parseInt(this.$('#select-per-page :selected').val(), 10);
      this.pagination.updatePerPage(this.perPage());
      FauxtonAPI.triggerRouteEvent('perPageChange', this.pagination.documentsLeftToFetch());
    },

    afterRender: function () {
      this.$('option[value="' + this.perPage() + '"]').attr('selected', "selected");
    },

    serialize: function () {
       var totalRows = 0,
          updateSeq = false,
          pageStart = 0,
          pageEnd = 20;

      if (!this.newView) {
        totalRows = this.collection.length;
        updateSeq = this.collection.updateSeq();
      }

      if (this.pagination) {
        pageStart = this.pagination.pageStart();
        pageEnd =  this.pagination.pageEnd();
      }

      return {
        database: app.utils.safeURLName(this.collection.database.id),
        updateSeq: updateSeq,
        totalRows: totalRows,
        pageStart: pageStart,
        pageEnd: pageEnd
      };
    },

    perPage: function () {
      return this._perPage;
    },

    setCollection: function (collection) {
      this.collection = collection;
    }

  });

  Views.AllDocsLayout = FauxtonAPI.View.extend({
    template: "addons/documents/templates/all_docs_layout",

    initialize: function (options) {
      this.database = options.database;
      this.params = options.params;
    },

    events: {
      'click #toggle-query': "toggleQuery"
    },

    toggleQuery: function (event) {
      $('#dashboard-content').scrollTop(0);
      this.$('#query').toggle('slow');
    },

    beforeRender: function () {
      this.advancedOptions = this.insertView('#query', new Views.AdvancedOptions({
        updateViewFn: this.updateAllDocs,
        previewFn: this.previewView,
        hasReduce: false,
        showPreview: false,
        database: this.database,
      }));
    },

    afterRender: function () {
      if (this.params) {
        this.advancedOptions.updateFromParams(this.params);
      }
    },

    updateAllDocs: function (event, paramInfo) {
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

      if (!_.isEmpty(params)) {
        fragment = fragment + '?' + $.param(params);
      }

      FauxtonAPI.navigate(fragment, {trigger: false});
      FauxtonAPI.triggerRouteEvent('updateAllDocs', {allDocs: true});
    },

    previewView: function (event) {
      event.preventDefault();
    }

  });

  // TODO: Rename to reflect that this is a list of rows or documents
  Views.AllDocsList = FauxtonAPI.View.extend({
    template: "addons/documents/templates/all_docs_list",
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
      this.docParams = options.docParams;
      this.params = options.params || {};
      this.expandDocs = true;
      this.perPageDefault = this.docParams.limit || 20;
    },

    establish: function() {
      if (this.newView) { return null; }

      return this.collection.fetch({
        reset: true,
        success:  function() { },
        error: function(model, xhr, options){
          // TODO: handle error requests that slip through
          // This should just throw a notification, not break the page
          FauxtonAPI.addNotification({
            msg: "Bad Request",
            type: "error"
          });

          //now redirect back to alldocs
          FauxtonAPI.navigate(model.database.url("index") + "?limit=100");
          console.log("ERROR: ", arguments);
        }
      });
    },

    selectAll: function(evt){
      $('.all-docs').find("input:checkbox").prop('checked', !$(evt.target).hasClass('active')).trigger('change');
    },

    serialize: function() {
      return {
        viewList: this.viewList,
        requestDuration: false,
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

      if (eles.length === 0 || !window.confirm("Are you sure you want to delete these " + eles.length + " docs?")) {
        return false;
      }

      _.each(eles, function(ele) {
        var model = this.collection.get(ele);

        model.destroy().then(function(resp) {
          that.rows[ele].$el.fadeOut(function () {
            $(this).remove();
          });

          model.collection.remove(model.id);
          if (!!model.id.match('_design')) {
            FauxtonAPI.triggerRouteEvent('reloadDesignDocs');
          }
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
        docLimit: this.params.limit,
        perPage: this.perPageDefault
      });
    },

    cleanup: function () {
      this.pagination && this.pagination.remove();
      this.allDocsNumber && this.allDocsNumber.remove();
      _.each(this.rows, function (row) {row.remove();});
    },

    beforeRender: function() {

      if (!this.pagination) {
        this.addPagination();
      }

      if (!this.params.keys) { //cannot paginate with keys
        this.insertView('#documents-pagination', this.pagination);
      }

      if (!this.allDocsNumber) {
        this.allDocsNumber = new Views.AllDocsNumber({
          collection: this.collection,
          newView: this.newView,
          pagination: this.pagination,
          perPageDefault: this.perPageDefault
        });
      }

      this.setView('#item-numbers', this.allDocsNumber);

      var docs = this.expandDocs ? this.collection : this.collection.simple();

      docs.each(function(doc) {
        this.rows[doc.id] = this.insertView("table.all-docs tbody", new this.nestedView({
          model: doc
        }));
      }, this);
    },

    setCollection: function (collection) {
      this.collection = collection;
      this.pagination.setCollection(collection);
      this.allDocsNumber.setCollection(collection);
    },

    setParams: function (docParams, urlParams) {
      this.docParams = docParams;
      this.params = urlParams;
      this.perPageDefault = this.docParams.limit;

      if (this.params.limit) {
        this.pagination.docLimit = this.params.limit;
      }
    },

    afterRender: function(){
      prettyPrint();
    },

    perPage: function () {
      return this.allDocsNumber.perPage();
    },

    updatePerPage: function (newPerPage) {
      this.collection.updateLimit(newPerPage);
    }
  });

  Views.Doc = FauxtonAPI.View.extend({
    template: "addons/documents/templates/doc",
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
      FauxtonAPI.navigate(this.database.url("index") + "?limit=100");
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
          databaseId = this.database.safeID(),
          deferred = $.Deferred(),
          that = this;

      promise.then(function () {
        deferred.resolve();
      }, function (xhr, reason, msg) {
        if (xhr.status === 404) {
          FauxtonAPI.addNotification({
            msg: 'The document does not exist',
            type: 'error',
            clear: true
          });
          that.goback();
        }
        deferred.reject();
     });

      return deferred;
    },

    saveDoc: function(event) {
      var json, notification,
      that = this,
      editor = this.editor,
      validDoc = this.getDocFromEditor();

      if (validDoc) {
        this.getDocFromEditor();

        notification = FauxtonAPI.addNotification({msg: "Saving document."});

        this.model.save().then(function () {
          editor.editSaved();
          FauxtonAPI.navigate('/database/' + that.database.safeID() + '/' + that.model.id);
        }).fail(function(xhr) {

          var responseText = JSON.parse(xhr.responseText).reason;
          notification = FauxtonAPI.addNotification({
            msg: "Save failed: " + responseText,
            type: "error",
            fade: false,
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

      this.model.clear().set(json, {validate: true});
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
        forceMissingId: true,
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

      var editor = this.editor,
          model = this.model;

      editor.editor.on("change", function (event) {
        var changedDoc;
        try {
          changedDoc = JSON.parse(editor.getValue());
        } catch(exception) {
          //not complete doc. Cannot work with it
          return;
        }

        var keyChecked = ["_id"];
        if (model.get("_rev")) { keyChecked.push("_rev");}

        //check the changedDoc has all the required standard keys
        if (_.isEmpty(_.difference(keyChecked, _.keys(changedDoc)))) { return; }

        editor.setReadOnly(true);
        setTimeout(function () { editor.setReadOnly(false);}, 400);
        // use extend so that _id stays at the top of the object with displaying the doc
        changedDoc = _.extend({_id: model.id, _rev: model.get("_rev")}, changedDoc);
        editor.setValue(JSON.stringify(changedDoc, null, "  "));
        FauxtonAPI.addNotification({
          type: "error",
          msg: "Cannot remove a documents Id or Revision."
        });
      });
    },

    cleanup: function () {
      if (this.editor) this.editor.remove();
    }
  });

  Views.DocFieldEditor = FauxtonAPI.View.extend({
    template: "addons/documents/templates/doc_field_editor",
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
    template: "addons/documents/templates/advanced_options",
    className: "advanced-options well",

    initialize: function (options) {
      this.database = options.database;
      this.ddocName = options.ddocName;
      this.viewName = options.viewName;
      this.updateViewFn = options.updateViewFn;
      this.previewFn = options.previewFn;

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
      "change form.js-view-query-update input": "updateFilters",
      "change form.js-view-query-update select": "updateFilters",
      "submit form.js-view-query-update": "updateView",
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

    renderOnUpdatehasReduce: function (hasReduce) {
      this.hasReduce = hasReduce;
      this.render();
    },

    queryParams: function () {
      var $form = this.$(".js-view-query-update");
      // Ignore params without a value
      var params = _.reduce($form.serializeArray(), function(params, param) {
        if (!param.value) { return params; }
        if (param.name === "limit" && param.value === 'None') { return params; }

        params.push(param);
        return params;
      }, []);

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
      var $form = $ele.parents("form.js-view-query-update:first");
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
      var $form = this.$el.find("form.js-view-query-update");
      _.each(params, function(val, key) {
        var $ele;
        switch (key) {
          case "limit":
          case "group_level":
            if (!val) { return; }
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
    template: "addons/documents/templates/design_doc_selector",

    events: {
      "change select#ddoc": "updateDesignDoc"
    },

    initialize: function (options) {
      this.ddocName = options.ddocName;
      this.database = options.database;
      this.listenTo(this.collection, 'add', this.ddocAdded);
      this.DocModel = options.DocModel || Documents.Doc;
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
      if (this.newDesignDoc()) {
        this.$('#new-ddoc-section').show();
      } else {
        this.$('#new-ddoc-section').hide();
      }
    },

    newDesignDoc: function () {

      return this.$('#ddoc').val() === 'new-doc';
    },

    newDocValidation: function(){
      return this.newDesignDoc() && this.$('#new-ddoc').val()==="";
    },
    getCurrentDesignDoc: function () {
      if (this.newDesignDoc()) {
        var doc = {
          _id: '_design/' + this.$('#new-ddoc').val(),
          views: {},
          language: "javascript"
        };
        var ddoc = new this.DocModel(doc, {database: this.database});
        //this.collection.add(ddoc);
        return ddoc;
      } else if ( !this.newDesignDoc() ) {
        var ddocName = this.$('#ddoc').val();
        return this.collection.find(function (ddoc) {
          return ddoc.id === ddocName;
        }).dDocModel();
      }
    }
  });

  Views.ViewEditor = FauxtonAPI.View.extend({
    template: "addons/documents/templates/view_editor",
    builtinReduces: ['_sum', '_count', '_stats'],

    events: {
      "click button.save": "saveView",
      "click button.delete": "deleteView",
      "change select#reduce-function-selector": "updateReduce",
      "click button.preview": "previewView",
      "click #db-views-tabs-nav": 'toggleIndexNav',
      "click .beautify_map":  "beautifyCode",
      "click .beautify_reduce":  "beautifyCode",
      "click #query-options-wrapper": 'toggleIndexNav'
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

      this.showIndex = false;
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
        this.reduceEditor.setValue(this.langTemplates.javascript.reduce);
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
        FauxtonAPI.navigate('/database/' + that.database.safeID() + '/_all_docs?limit=' + Databases.DocLimit);
        FauxtonAPI.triggerRouteEvent('reloadDesignDocs');
      });
    },

    saveView: function(event) {
      var json, notification,
      that = this;

      if (event) { event.preventDefault();}

      $('#dashboard-content').scrollTop(0); //scroll up

      if (this.hasValidCode() && this.$('#new-ddoc:visible').val() !=="") {
        var mapVal = this.mapEditor.getValue(),
        reduceVal = this.reduceVal(),
        viewName = this.$('#index-name').val(),
        ddoc = this.getCurrentDesignDoc(),
        ddocName = ddoc.id,
        viewNameChange = false;

        if (this.viewName !== viewName) {
          ddoc.removeDdocView(this.viewName);
          this.viewName = viewName;
          viewNameChange = true;
        }

        notification = FauxtonAPI.addNotification({
          msg: "Saving document.",
          selector: "#define-view .errors-container",
          clear: true
        });

        ddoc.setDdocView(viewName, mapVal, reduceVal);

        ddoc.save().then(function () {
          that.ddocs.add(ddoc);

          that.mapEditor.editSaved();
          that.reduceEditor && that.reduceEditor.editSaved();


          FauxtonAPI.addNotification({
            msg: "View has been saved.",
            type: "success",
            selector: "#define-view .errors-container",
            clear: true
          });

          if (that.newView || viewNameChange) {
            var fragment = '/database/' + that.database.safeID() +'/' + ddoc.safeID() + '/_view/' + app.utils.safeURLName(viewName);

            FauxtonAPI.navigate(fragment, {trigger: false});
            that.newView = false;
            that.ddocID = ddoc.safeID();
            that.viewName = viewName;
            that.ddocInfo = ddoc;
            that.showIndex = true;
            that.render();
            FauxtonAPI.triggerRouteEvent('reloadDesignDocs', {
              selectedTab: app.utils.removeSpecialCharacters(ddocName.replace(/_design\//,'')) + '_' + app.utils.removeSpecialCharacters(viewName)
            });
          }

          if (that.reduceFunStr !== reduceVal) {
            that.reduceFunStr = reduceVal;
            that.advancedOptions.renderOnUpdatehasReduce(that.hasReduce());
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
          selector: "#define-view .errors-container",
          clear: true
        });
      }
    },

    updateView: function(event, paramInfo) {
       event.preventDefault();

       if (this.newView) { return alert('Please save this new view before querying it.'); }

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
             selector: ".advanced-options .errors-container",
             clear: true
           });
         });
         FauxtonAPI.addNotification({
           msg: "Make sure that strings are properly quoted and any other values are valid JSON structures",
           type: "warning",
           selector: ".advanced-options .errors-container",
           clear: true
         });

         return false;
      }

       var fragment = window.location.hash.replace(/\?.*$/, '');
       if (!_.isEmpty(params)) {
        fragment = fragment + '?' + $.param(params);
       }

       FauxtonAPI.navigate(fragment, {trigger: false});
       FauxtonAPI.triggerRouteEvent('updateAllDocs', {ddoc: this.ddocID, view: this.viewName});
    },


    previewView: function(event, paramsInfo) {
      event.preventDefault();
      var that = this,
      mapVal = this.mapVal(),
      reduceVal = this.reduceVal(),
      paramsArr = [];

      if (paramsInfo && paramsInfo.params) {
        paramsArr = paramsInfo.params;
      }

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

      if (!this.database.allDocs || this.database.allDocs.params.include_docs !== true) {
        this.database.buildAllDocs({limit: Databases.DocLimit.toString(), include_docs: true});
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

    mapVal: function () {
      if (this.mapEditor) {
        return this.mapEditor.getValue();
      }

      return this.$('#map-function').text();
    },

    reduceVal: function() {
      var reduceOption = this.$('#reduce-function-selector :selected').val(),
      reduceVal = "";

      if (reduceOption === 'CUSTOM') {
        if (!this.reduceEditor) { this.createReduceEditor(); }
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
      var $targetId = this.$(event.target).attr('id'),
          $previousTab = this.$(this.$('li.active a').attr('href')),
          $targetTab = this.$(this.$(event.target).attr('href'));

      if ($targetTab.attr('id') !== $previousTab.attr('id')) {
        $previousTab.removeAttr('style');
      }

      if ($targetId === 'index-nav') {
        if (this.newView) { return; }
        var that = this;
        $('#dashboard-content').scrollTop(0); //scroll up
        $targetTab.toggle('slow', function(){
           that.showEditors();
        });
      } else {
        $targetTab.toggle('slow');
      }
    },

    serialize: function() {
      return {
        ddocs: this.ddocs,
        ddoc: this.model,
        ddocName: this.model.id,
        viewName: this.viewName,
        reduceFunStr: this.reduceFunStr,
        isCustomReduce: this.hasCustomReduce(),
        newView: this.newView,
        langTemplates: this.langTemplates.javascript
      };
    },

    hasCustomReduce: function() {
      return this.reduceFunStr && ! _.contains(this.builtinReduces, this.reduceFunStr);
    },

    hasReduce: function () {
      return this.reduceFunStr || false;
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

      if (this.reduceEditor.getLines() === 1){
        this.$('.beautify_reduce').removeClass("hide");
        $('.beautify-tooltip').tooltip();
      }
    },

    beforeRender: function () {

      if (this.newView) {
        this.reduceFunStr = '';
        if (this.ddocs.length === 0) {
          this.model = new Documents.Doc(null, {database: this.database});
        } else {
          this.model = this.ddocs.first().dDocModel();
        }
        this.ddocID = this.model.id;
      } else {
        var ddocDecode = decodeURIComponent(this.ddocID);
        this.model = this.ddocs.get(ddocDecode).dDocModel();
        this.reduceFunStr = this.model.viewHasReduce(this.viewName);
        this.setView('#ddoc-info', new Views.DdocInfo({model: this.ddocInfo }));
      }

      this.designDocSelector = this.setView('.design-doc-group', new Views.DesignDocSelector({
        collection: this.ddocs,
        ddocName: this.model.id,
        database: this.database
      }));


      if (!this.newView) {
        this.eventer = _.extend({}, Backbone.Events);

        this.advancedOptions = this.insertView('#query', new Views.AdvancedOptions({
          updateViewFn: this.updateView,
          previewFn: this.previewView,
          database: this.database,
          viewName: this.viewName,
          ddocName: this.model.id,
          hasReduce: this.hasReduce(),
          eventer: this.eventer
        }));
      }

    },

    afterRender: function() {
      if (this.params && !this.newView) {
        this.advancedOptions.updateFromParams(this.params);
      }

      this.designDocSelector.updateDesignDoc();
      if (this.newView || this.showIndex) {
        this.showEditors();
        this.showIndex = false;
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
        //Use a built in view by default
        //this.reduceEditor.setValue(this.langTemplates[this.defaultLang].reduce);
      }

      this.mapEditor.editSaved();
      this.reduceEditor && this.reduceEditor.editSaved();

      if (this.mapEditor.getLines() === 1){
        this.$('.beautify_map').removeClass("hide");
        $('.beautify-tooltip').tooltip();
      }
    },
    beautifyCode: function(e){
      e.preventDefault();
      var targetEditor = $(e.currentTarget).hasClass('beautify_reduce')?this.reduceEditor:this.mapEditor;
      var beautifiedCode = beautify(targetEditor.getValue());
      targetEditor.setValue(beautifiedCode);
    },
    cleanup: function () {
      this.mapEditor && this.mapEditor.remove();
      this.reduceEditor && this.reduceEditor.remove();
    }
  });

  Views.JumpToDoc = FauxtonAPI.View.extend({
    template: "addons/documents/templates/jumpdoc",

    initialize: function (options) {
      this.database = options.database;
    },

    events: {
      "submit #jump-to-doc": "jumpToDoc"
    },

    jumpToDoc: function (event) {
      event.preventDefault();
      var docId = this.$('#jump-to-doc-id').val().trim();
      FauxtonAPI.navigate('/database/' + app.utils.safeURLName(this.database.id) +'/' + app.utils.safeURLName(docId), {trigger: true});
    },

    afterRender: function () {
     this.typeAhead = new Components.DocSearchTypeahead({el: '#jump-to-doc-id', database: this.database});
     this.typeAhead.render();
    }
  });

  Views.Sidebar = FauxtonAPI.View.extend({
    template: "addons/documents/templates/sidebar",
    events: {
      "click button#delete-database": "showDeleteDatabaseModal"
    },

    initialize: function(options) {
      this.database = options.database;
      if (options.ddocInfo) {
        this.ddocID = options.ddocInfo.id;
        this.currView = options.ddocInfo.currView;
      }
    },
    showDeleteDatabaseModal: function(event){
      this.deleteDBModal.showModal();
    },

    serialize: function() {
      var docLinks = FauxtonAPI.getExtensions('docLinks'),
          newLinks = FauxtonAPI.getExtensions('sidebar:newLinks'),
          addLinks = FauxtonAPI.getExtensions('sidebar:links'),
          extensionList = FauxtonAPI.getExtensions('sidebar:list');
      return {
        changes_url: '#' + this.database.url('changes'),
        permissions_url: '#' + this.database.url('app') + '/permissions',
        db_url: '#' + this.database.url('index'),
        database: this.collection.database,
        database_url: '#' + this.database.url('app'),
        docLinks: docLinks,
        addLinks: addLinks,
        newLinks: newLinks,
        extensionList: extensionList > 0
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
      this.deleteDBModal = this.setView(
        '#delete-db-modal',
        new Views.DeleteDBModal({database: this.database})
      );

      var sidebarListViews = FauxtonAPI.getExtensions('sidebar:list');
      _.each(sidebarListViews, function (view) {
        var extension = this.insertView('#extension-navs', view);
        extension.update(this.database, this.collection, this.viewName);
        extension.render();
      }, this);

      this.collection.each(function(design) {
        if (design.has('doc')){
          var ddoc = design.id.replace(/^_design\//,"");
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
    template: "addons/documents/templates/changes",

    initialize: function () {
      this.listenTo( this.model.changes, 'sync', this.render);
      this.listenTo( this.model.changes, 'cachesync', this.render);
    },

    establish: function() {
      return [ this.model.changes.fetchOnce({prefill: true})];
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
    template: "addons/documents/templates/ddoc_info",

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
