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
       "addons/documents/views",
       "addons/databases/base"
],

function(app, FauxtonAPI, Documents, Databases) {

  var DocEditorRouteObject = FauxtonAPI.RouteObject.extend({
    layout: "one_pane",
    disableLoader: true,
    selectedHeader: "Databases",
    initialize: function(route, masterLayout, options) {
      var databaseName = options[0];
      this.docID = options[1]||'new';

      this.database = this.database || new Databases.Model({id: databaseName});
      this.doc = new Documents.Doc({
        _id: this.docID
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
      // We are hiding the field_editor for this first release
      // "database/:database/:doc/field_editor": "field_editor",
      "database/:database/:doc/code_editor": "code_editor",
      "database/:database/:doc": "code_editor"
    },

    events: {
      "route:reRenderDoc": "reRenderDoc",
      "route:duplicateDoc": "duplicateDoc"
    },

    crumbs: function() {
      return [
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
        FauxtonAPI.navigate('/database/' + database.safeID() + '/' + app.utils.safeURLName(newId), {trigger: true});
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
      return [this.doc.url("apiurl"), this.doc.documentation()];
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
    crumbs: function() {
      return [
        {"name": this.database.id, "link": Databases.databaseUrl(this.database)},
        {"name": "New", "link": "#"}
      ];
    },
    routes: {
      "database/:database/new": "code_editor"
    },
    selectedHeader: "Databases",

  });

  var DocumentsRouteObject = FauxtonAPI.RouteObject.extend({
    layout: "with_tabs_sidebar",
    selectedHeader: "Databases",
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
      "route:reloadDesignDocs": "reloadDesignDocs",
      "route:paginate": "paginate",
      "route:perPageChange": "perPageChange"
    },

    initialize: function (route, masterLayout, options) {
      this.databaseName = options[0];

      this.data = {
        database: new Databases.Model({id:this.databaseName})
      };

      this.data.designDocs = new Documents.AllDocs(null, {
        database: this.data.database,
        params: {
          startkey: '"_design"',
          endkey: '"_design1"',
          include_docs: true
        }
      });

      this.sidebar = this.setView("#sidebar-content", new Documents.Views.Sidebar({
        collection: this.data.designDocs,
        database: this.data.database
      }));
    },

    establish: function () {
      return this.data.designDocs.fetch();
    },

    createParams: function (options) {
      var urlParams = app.getParams(options);
      return {
        urlParams: urlParams,
        docParams: _.extend(_.clone(urlParams), {limit: this.getDocPerPageLimit(urlParams, 20)})
      };
    },

    /*
    * docParams are the options collection uses to fetch from the server 
    * urlParams are what are shown in the url and to the user
    * They are not the same when paginating
    */
    allDocs: function(databaseName, options) {
      var params = this.createParams(options),
          urlParams = params.urlParams,
          docParams = params.docParams;

      if (this.eventAllDocs) {
        this.eventAllDocs = false;
        return;
      }

      this.data.database.buildAllDocs(docParams);

      if (docParams.startkey && docParams.startkey.indexOf('_design') > -1) {
        this.sidebar.setSelectedTab('design-docs');
      } else {
        this.sidebar.setSelectedTab('all-docs');
      }

      if (this.viewEditor) { this.viewEditor.remove(); }

      this.toolsView = this.setView("#dashboard-upper-menu", new Documents.Views.JumpToDoc({
        database: this.data.database,
        collection: this.data.database.allDocs
      }));

      this.setView("#dashboard-upper-content", new Documents.Views.AllDocsLayout({
        database: this.data.database,
        collection: this.data.database.allDocs,
        params: urlParams,
        docParams: docParams
      }));

      this.documentsView = this.setView("#dashboard-lower-content", new Documents.Views.AllDocsList({
        collection: this.data.database.allDocs,
        docParams: docParams,
        params: urlParams
      }));

      this.crumbs = [
        {"name": this.data.database.id, "link": Databases.databaseUrl(this.data.database)}
      ];

      this.apiUrl = [this.data.database.allDocs.url("apiurl", urlParams), this.data.database.allDocs.documentation() ];
      //reset the pagination history - the history is used for pagination.previous
      Documents.paginate.reset();
    },

    viewFn: function (databaseName, ddoc, view) {
      var params = this.createParams(),
          urlParams = params.urlParams,
          docParams = params.docParams;
          decodeDdoc = decodeURIComponent(ddoc);

      view = view.replace(/\?.*$/,'');

      this.data.indexedDocs = new Documents.IndexCollection(null, {
        database: this.data.database,
        design: decodeDdoc,
        view: view,
        params: docParams
      });
     
      this.viewEditor = this.setView("#dashboard-upper-content", new Documents.Views.ViewEditor({
        model: this.data.database,
        ddocs: this.data.designDocs,
        viewName: view,
        params: urlParams,
        newView: false,
        database: this.data.database,
        ddocInfo: this.ddocInfo(decodeDdoc, this.data.designDocs, view)
      }));

      if (this.toolsView) { this.toolsView.remove(); }

      this.documentsView = this.createViewDocumentsView({
        designDoc: decodeDdoc,
        docParams: docParams, 
        urlParams: urlParams,
        database: this.data.database,
        indexedDocs: this.data.indexedDocs,
        designDocs: this.data.designDocs,
        view: view
      });

      this.sidebar.setSelectedTab(app.utils.removeSpecialCharacters(ddoc) + '_' + app.utils.removeSpecialCharacters(view));

      this.crumbs = function () {
        return [
          {"name": this.data.database.id, "link": Databases.databaseUrl(this.data.database)},
        ];
      };

      this.apiUrl = [this.data.indexedDocs.url("apiurl", urlParams), "docs"];
      Documents.paginate.reset();
    },

    ddocInfo: function (designDoc, designDocs, view) {
      return {
        id: "_design/" + designDoc,
        currView: view,
        designDocs: designDocs
      };
    },

    createViewDocumentsView: function (options) { 

      return this.setView("#dashboard-lower-content", new Documents.Views.AllDocsList({
        database: options.database,
        collection: options.indexedDocs,
        nestedView: Documents.Views.Row,
        viewList: true,
        ddocInfo: this.ddocInfo(options.designDoc, options.designDocs, options.view),
        docParams: options.docParams,
        params: options.urlParams
      }));
    },

    newViewEditor: function () {
      var params = app.getParams();

      this.toolsView && this.toolsView.remove();
      this.documentsView && this.documentsView.remove();

      this.viewEditor = this.setView("#dashboard-upper-content", new Documents.Views.ViewEditor({
        ddocs: this.data.designDocs,
        params: params,
        database: this.data.database,
        newView: true
      }));

      this.sidebar.setSelectedTab('new-view');
      this.crumbs = function () {
        return [
          {"name": this.data.database.id, "link": Databases.databaseUrl(this.data.database)},
        ];
      };

      Documents.paginate.reset();
    },

    updateAllDocsFromView: function (event) {
      var view = event.view,
          params = this.createParams(),
          urlParams = params.urlParams,
          docParams = params.docParams,
          ddoc = event.ddoc,
          collection;

      docParams.limit = this.getDocPerPageLimit(urlParams, this.documentsView.perPage());
      this.documentsView.forceRender();

      if (event.allDocs) {
        this.eventAllDocs = true; // this is horrible. But I cannot get the trigger not to fire the route!
        this.data.database.buildAllDocs(docParams);
        collection = this.data.database.allDocs;

      } else {
        collection = this.data.indexedDocs = new Documents.IndexCollection(null, {
          database: this.data.database,
          design: ddoc,
          view: view,
          params: docParams
        });

        if (!this.documentsView) {
          this.documentsView = this.createViewDocumentsView({
            designDoc: ddoc,
            docParams: docParams, 
            urlParams: urlParams,
            database: this.data.database,
            indexedDocs: this.indexedDocs,
            designDocs: this.data.designDocs,
            view: view
          });
        }
      }

      this.documentsView.setCollection(collection);
      this.documentsView.setParams(docParams, urlParams);

      this.apiUrl = [collection.url("apiurl", urlParams), "docs"];
      Documents.paginate.reset();
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

    perPageChange: function (perPage) {
      // We need to restore the collection parameters to the defaults (1st page)
      // and update the page size
      var params = this.documentsView.collection.restoreDefaultParameters();
      this.perPage = perPage;
      this.documentsView.updatePerPage(perPage);
      this.documentsView.forceRender();
      this.documentsView.collection.params.limit = perPage;
      this.setDocPerPageLimit(perPage);
    },

    paginate: function (options) {
      var params = {},
          urlParams = app.getParams(),
          collection = this.documentsView.collection;

      this.documentsView.forceRender();

      // this is really ugly. But we basically need to make sure that
      // all parameters are in the correct state and have been parsed before we
      // calculate how to paginate the collection
      collection.params = Documents.QueryParams.parse(collection.params);
      urlParams = Documents.QueryParams.parse(urlParams);

      if (options.direction === 'next') {
          params = Documents.paginate.next(collection.toJSON(), 
                                           collection.params,
                                           options.perPage, 
                                           !!collection.isAllDocs);
      } else {
          params = Documents.paginate.previous(collection.toJSON(), 
                                               collection.params, 
                                               options.perPage, 
                                               !!collection.isAllDocs);
      }

      // use the perPage sent from IndexPagination as it calculates how many
      // docs to fetch for next page
      params.limit = options.perPage;

      // again not pretty but need to make sure all the parameters can be correctly
      // built into a query
      params = Documents.QueryParams.stringify(params);
      collection.updateParams(params);
    },

    reloadDesignDocs: function (event) {
      this.sidebar.forceRender();

      if (event && event.selectedTab) {
        this.sidebar.setSelectedTab(event.selectedTab);
      }
    },

    setDocPerPageLimit: function (perPage) {
      window.localStorage.setItem('fauxton:perpage', perPage);
    },


    getDocPerPageLimit: function (urlParams, perPage) {
      var storedPerPage = perPage;

      if (window.localStorage) {
        storedPerPage = window.localStorage.getItem('fauxton:perpage');

        if (!storedPerPage) {
          this.setDocPerPageLimit(perPage);
          storedPerPage = perPage;
        } else {
          storedPerPage = parseInt(storedPerPage, 10);
        }
      } 

      if (!urlParams.limit || urlParams.limit > storedPerPage) {
        return storedPerPage;
      } else {
        return urlParams.limit;
      }
    }

  });

  var ChangesRouteObject = FauxtonAPI.RouteObject.extend({
    layout: "with_tabs",
    selectedHeader: "Databases",
    crumbs: function () {
      return [
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

      var docParams = app.getParams();

      this.database.buildChanges(docParams);
    },

    changes: function (event) {
      this.setView("#dashboard-content", new Documents.Views.Changes({
        model: this.database
      }));
    },

    apiUrl: function() {
      return [this.database.url("apiurl"), this.database.documentation()];
    }

  });

  Documents.RouteObjects = [DocEditorRouteObject, NewDocEditorRouteObject, DocumentsRouteObject, ChangesRouteObject];

  return Documents;
});
