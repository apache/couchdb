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

       // Modules
       "modules/documents/views",
       "modules/databases/base"
],

function(app, FauxtonAPI, Documents, Databases) {
  // TODO: look at using:
  // var Documents = require("modules/documents/models_collections");
  // var Databases = require("modules/databases/module");

  var DocEditorRouteObject = FauxtonAPI.RouteObject.extend({
    layout: "one_pane",
    disableLoader: true,
    initialize: function(route, masterLayout, options) {
      var databaseName = options[0], docID = options[1]||'new';

      this.database = this.database || new Databases.Model({id: databaseName});
      this.doc = new Documents.Doc({
        _id: docID
      }, {
        database: this.database
      });

      this.tabsView = this.setView("#tabs", new Documents.Views.FieldEditorTabs({
        disableLoader: true,
        selected: "code_editor",
        model: this.doc
      }));

    },

    routes: {
      "database/:database/:doc/field_editor": "field_editor",
      "database/:database/:doc/code_editor": "code_editor",
      "database/:database/:doc": "code_editor"
    },

    events: {
      "route:reRenderDoc": "reRenderDoc",
      "route:duplicateDoc": "duplicateDoc"
    },

    crumbs: function() {
      return [
        {"name": "Databases", "link": "/_all_dbs"},
        {"name": this.database.id, "link": Databases.databaseUrl(this.database)},
        {"name": this.docID, "link": "#"}
      ];
    },

    code_editor: function (database, doc) {
      this.tabsView.updateSelected('code_editor');

      this.docView = this.setView("#dashboard-content", new Documents.Views.Doc({
        model: this.doc,
        database: this.database
      }));
    },

    reRenderDoc: function () {
      this.docView.forceRender();
    },

    field_editor: function(events) {
      this.tabsView.updateSelected('field_editor');
      this.docView = this.setView("#dashboard-content", new Documents.Views.DocFieldEditor({
        model: this.doc
      }));
    },

    duplicateDoc: function (newId) {
      var doc = this.doc,
      docView = this.docView,
      database = this.database;

      doc.copy(newId).then(function () {
        doc.set({_id: newId}); 
        docView.forceRender();
        FauxtonAPI.navigate('/database/' + database.id + '/' + newId, {trigger: true});
        FauxtonAPI.addNotification({
          msg: "Document has been duplicated."
        });

      }, function (error) {
        var errorMsg = "Could not duplicate document, reason: " + error.responseText + ".";
        FauxtonAPI.addNotification({
          msg: errorMsg,
          type: "error"
        });
      });
    },

    apiUrl: function() {
      return this.doc.url();
    }
  });

  var NewDocEditorRouteObject = DocEditorRouteObject.extend({
    initialize: function (route, masterLayout, options) {
      var databaseName = options[0];

      this.database = this.database || new Databases.Model({id: databaseName});
      this.doc = new Documents.NewDoc(null,{
        database: this.database
      });

      this.tabsView = this.setView("#tabs", new Documents.Views.FieldEditorTabs({
        selected: "code_editor",
        model: this.doc
      }));

    },

    routes: {
      "database/:database/new": "code_editor"
    },

  });

  var DocumentsRouteObject = FauxtonAPI.RouteObject.extend({
    layout: "with_tabs_sidebar",
    navSelector: '.databases',
    routes: {
      "database/:database/_all_docs(:extra)": "allDocs", 
      "database/:database/_design/:ddoc/_view/:view": {
        route: "viewFn",
        roles: ['_admin']
      },
      "database/:database/new_view": "newViewEditor"
    },

    events: {
      "route:updateAllDocs": "updateAllDocsFromView",
      "route:updatePreviewDocs": "updateAllDocsFromPreview",
      "route:reloadDesignDocs": "reloadDesignDocs"
    },

    initialize: function (route, masterLayout, options) {
      var docOptions = app.getParams();
      docOptions.include_docs = true;

      this.databaseName = options[0];

      this.data = {
        database: new Databases.Model({id:this.databaseName})
      };

      this.data.designDocs = new Documents.AllDocs(null, {
        database: this.data.database,
        params: {startkey: '"_design"',
          endkey: '"_design1"',
          include_docs: true}
      });

      this.sidebar = this.setView("#sidebar-content", new Documents.Views.Sidebar({
        collection: this.data.designDocs,
        database: this.data.database
      }));

      // this.setView("#tabs", new Documents.Views.Tabs({
      //   collection: this.data.designDocs,
      //   database: this.data.database
      // }));
    },

    establish: function () {
      return this.data.designDocs.fetch();
    },

    allDocs: function(databaseName, options) {
      var docOptions = app.getParams(options);

      docOptions.include_docs = true;
      this.data.database.buildAllDocs(docOptions);

      if (docOptions.startkey && docOptions.startkey.indexOf('_design') > -1) {
        this.sidebar.setSelectedTab('design-docs');
      } else {
        this.sidebar.setSelectedTab('all-docs');
      }

      if (this.viewEditor) { this.viewEditor.remove(); }


      this.toolsView = this.setView("#dashboard-upper-menu", new Documents.Views.JumpToDoc());

      this.documentsView = this.setView("#dashboard-lower-content", new Documents.Views.AllDocsList({
        collection: this.data.database.allDocs
      }));

      this.crumbs = [
        {"name": "Databases", "link": "/_all_dbs"},
        {"name": this.data.database.id, "link": Databases.databaseUrl(this.data.database)}
      ];

      this.apiUrl = this.data.database.allDocs.url();
    },

    viewFn: function (databaseName, ddoc, view) {
      var params = app.getParams();

      view = view.replace(/\?.*$/,'');

      this.data.indexedDocs = new Documents.IndexCollection(null, {
        database: this.data.database,
        design: ddoc,
        view: view,
        params: params
      });

      var ddocInfo = {
        id: "_design/" + ddoc,
        currView: view,
        designDocs: this.data.designDocs
      };

      this.viewEditor = this.setView("#dashboard-upper-content", new Documents.Views.ViewEditor({
        model: this.data.database,
        ddocs: this.data.designDocs,
        viewName: view,
        params: params,
        newView: false,
        database: this.data.database,
        ddocInfo: ddocInfo
      }));

      if (this.toolsView) { this.toolsView.remove(); }

      this.documentsView = this.setView("#dashboard-lower-content", new Documents.Views.AllDocsList({
        database: this.data.database,
        collection: this.data.indexedDocs,
        nestedView: Documents.Views.Row,
        viewList: true,
        ddocInfo: ddocInfo
      }));

      this.sidebar.setSelectedTab(ddoc + '_' + view);

      this.crumbs = function () {
        return [
          {"name": "Databases", "link": "/_all_dbs"},
          {"name": this.data.database.id, "link": Databases.databaseUrl(this.data.database)},
          {"name": ddoc + "/" + view, "link": this.data.indexedDocs.url()}
        ];
      };

      this.apiUrl = this.data.indexedDocs.url();
    },

    newViewEditor: function () {
      var params = app.getParams();

      this.viewEditor = this.setView("#dashboard-upper-content", new Documents.Views.ViewEditor({
        ddocs: this.data.designDocs,
        params: params,
        database: this.data.database,
        newView: true
      }));

      this.sidebar.setSelectedTab('new-view');
      this.crumbs = function () {
        return [
          {"name": "Databases", "link": "/_all_dbs"},
          {"name": this.data.database.id, "link": Databases.databaseUrl(this.data.database)},
          {"name": "new"}
        ];
      };
    },

    updateAllDocsFromView: function (event) {
      var view = event.view,
      ddoc = event.ddoc;

      this.data.indexedDocs = new Documents.IndexCollection(null, {
        database: this.data.database,
        design: ddoc,
        view: view,
        params: app.getParams()
      });

      this.documentsView = this.setView("#dashboard-lower-content", new Documents.Views.AllDocsList({
        database: this.data.database,
        collection: this.data.indexedDocs,
        nestedView: Documents.Views.Row,
        viewList: true
      }));
    },

    updateAllDocsFromPreview: function (event) {
      var view = event.view,
      rows = event.rows,
      ddoc = event.ddoc;

      this.data.indexedDocs = new Documents.PouchIndexCollection(null, {
        database: this.data.database,
        design: ddoc,
        view: view,
        rows: rows
      });

      this.documentsView = this.setView("#dashboard-lower-content", new Documents.Views.AllDocsList({
        database: this.data.database,
        collection: this.data.indexedDocs,
        nestedView: Documents.Views.Row,
        viewList: true
      }));
    },

    reloadDesignDocs: function (event) {
      this.sidebar.forceRender();

      if (event && event.selectedTab) {
        this.sidebar.setSelectedTab(event.selectedTab);
      }
    }
  });

  var ChangesRouteObject = FauxtonAPI.RouteObject.extend({
    layout: "with_tabs",
    navSelector: '.databases',
    crumbs: function () {
      return [
        {"name": "Databases", "link": "/_all_dbs"},
        {"name": this.database.id, "link": Databases.databaseUrl(this.database)},
        {"name": "_changes", "link": "/_changes"}
      ];
    },

    routes: {
      "database/:database/_changes(:params)": "changes"
    },

    initialize: function (route, masterLayout, options) {
      this.databaseName = options[0];
      this.database = new Databases.Model({id: this.databaseName});

      var docOptions = app.getParams();

      this.database.buildChanges(docOptions);

      this.setView("#tabs", new Documents.Views.Tabs({
        collection: this.designDocs,
        database: this.database,
        active_id: 'changes'
      }));
    },

    changes: function (event) {
      this.setView("#dashboard-content", new Documents.Views.Changes({
        model: this.database
      }));
    },

    apiUrl: function() {
      return this.database.changes.url();
    }

  });

  Documents.RouteObjects = [DocEditorRouteObject, NewDocEditorRouteObject, DocumentsRouteObject, ChangesRouteObject];

  return Documents;
});
