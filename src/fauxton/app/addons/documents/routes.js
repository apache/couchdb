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
    },

    routes: {
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

      this.docView = this.setView("#dashboard-content", new Documents.Views.CodeEditor({
        model: this.doc,
        database: this.database
      }));
    },

    reRenderDoc: function () {
      this.docView.forceRender();
    },

    duplicateDoc: function (newId) {
      var doc = this.doc,
      docView = this.docView,
      database = this.database;
      this.docID = newId;

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

      "database/:database/_all_docs(:extra)": {
        route: "allDocs",
        roles: ["_reader","_writer","_admin"]
      },
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
        paging: {
          pageSize: 500
        },
        params: {
          startkey: '_design',
          endkey: '_design1',
          include_docs: true,
          limit: 500
        }
      });

      this.sidebar = this.setView("#sidebar-content", new Documents.Views.Sidebar({
        collection: this.data.designDocs,
        database: this.data.database
      }));
    },

    establish: function () {
      return this.data.designDocs.fetch({reset: true});
    },

    createParams: function (options) {
      var urlParams = app.getParams(options);
      var params = Documents.QueryParams.parse(urlParams);

      return {
        urlParams: urlParams,
        docParams: _.extend(params, {limit: this.getDocPerPageLimit(params, 20)})
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

      this.data.database.allDocs.paging.pageSize = this.getDocPerPageLimit(urlParams, parseInt(docParams.limit, 10));

      this.setView("#dashboard-upper-content", new Documents.Views.AllDocsLayout({
        database: this.data.database,
        collection: this.data.database.allDocs,
        params: urlParams,
        docParams: docParams
      }));

      this.documentsView = this.setView("#dashboard-lower-content", new Documents.Views.AllDocsList({
        database: this.data.database,
        collection: this.data.database.allDocs,
        docParams: docParams,
        params: urlParams,
        bulkDeleteDocsCollection: new Documents.BulkDeleteDocCollection([], {databaseId: this.data.database.get('id')})
      }));

      this.crumbs = [
        {"name": this.data.database.id, "link": Databases.databaseUrl(this.data.database)}
      ];

      this.apiUrl = [this.data.database.allDocs.urlRef("apiurl", urlParams), this.data.database.allDocs.documentation() ];
    },

    viewFn: function (databaseName, ddoc, view) {
      var params = this.createParams(),
          urlParams = params.urlParams,
          docParams = params.docParams,
          decodeDdoc = decodeURIComponent(ddoc);

      view = view.replace(/\?.*$/,'');

      this.data.indexedDocs = new Documents.IndexCollection(null, {
        database: this.data.database,
        design: decodeDdoc,
        view: view,
        params: docParams,
        paging: {
          pageSize: this.getDocPerPageLimit(urlParams, parseInt(docParams.limit, 10))
        }
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

      this.apiUrl = [this.data.indexedDocs.urlRef("apiurl", urlParams), "docs"];
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
    },

    updateAllDocsFromView: function (event) {
      var view = event.view,
          params = this.createParams(),
          urlParams = params.urlParams,
          docParams = params.docParams,
          ddoc = event.ddoc,
          pageSize,
          collection;

      var defaultPageSize = _.isUndefined(this.documentsView) ? 20 : this.documentsView.perPage();
      docParams.limit = pageSize = this.getDocPerPageLimit(urlParams, defaultPageSize);

      if (event.allDocs) {
        this.eventAllDocs = true; // this is horrible. But I cannot get the trigger not to fire the route!
        this.data.database.buildAllDocs(docParams);
        collection = this.data.database.allDocs;
        collection.paging.pageSize = pageSize;

      } else {
        collection = this.data.indexedDocs = new Documents.IndexCollection(null, {
          database: this.data.database,
          design: ddoc,
          view: view,
          params: docParams,
          paging: {
            pageSize: pageSize
          }
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
      this.documentsView.forceRender();

      this.apiUrl = [collection.urlRef("apiurl", urlParams), "docs"];
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
      this.perPage = perPage;
      this.documentsView.forceRender();
      this.documentsView.collection.pageSizeReset(perPage, {fetch: false});
      this.setDocPerPageLimit(perPage);
    },

    paginate: function (options) {
      var collection = this.documentsView.collection;

      this.documentsView.forceRender();
      collection.paging.pageSize = options.perPage;
      var promise = collection[options.direction]({fetch: false});
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
        return parseInt(storedPerPage, 10);
      } else {
        return parseInt(urlParams.limit, 10);
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
