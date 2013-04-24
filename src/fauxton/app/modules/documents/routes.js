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
       "modules/documents/resources",
       "modules/databases/base"
],

function(app, FauxtonAPI, Documents, Databases) {
  // TODO: look at using:
  // var Documents = require("modules/documents/models_collections");
  // var Databases = require("modules/databases/module");

  var DocEditorRouteObject = FauxtonAPI.RouteObject.extend({
    layout: "one_pane",

    initialize: function(options) {
      var databaseName = options[0], docID = options[1];

      this.database = this.database || new Databases.Model({id: databaseName});
      this.doc = this.doc || new Documents.Doc({
        _id: docID
      }, {
        database: this.database
      });

      this.tabsView = this.setView("#tabs", new Documents.Views.FieldEditorTabs({
        selected: "code_editor",
        model: this.doc
      }));

    },

    routes: function() {
      return _.keys(this.selectedRoutes);
    },

    selectedRoutes: {
      "database/:database/:doc/field_editor": "field_editor",
      "database/:database/:doc/code_editor": "code_editor",
      "database/:database/:doc": "code_editor"
    },

    events: {
      "route:field_editor": "field_editor",
      "route:code_editor": "code_editor"
    },

    defaultRoute: "code_editor",

    crumbs: function() {
      return [
        {"name": "Databases", "link": "/_all_dbs"},
        {"name": this.database.id, "link": Databases.databaseUrl(this.database)},
        {"name": this.docID, "link": "#"}
      ];
    },

    code_editor: function (event) {
      this.tabsView.updateSelected('code_editor');
      this.docView = this.setView("#dashboard-content", new Documents.Views.Doc({
        model: this.doc
      }));
    },

    field_editor: function(events) {
      this.tabsView.updateSelected('field_editor');
      this.docView = this.setView("#dashboard-content", new Documents.Views.DocFieldEditor({
        model: this.doc
      }));
    },

    apiUrl: function() {
      return this.doc.url();
    }
  });

  var newViewEditorCallback = function(databaseName) {
    var data = {
      database: new Databases.Model({id:databaseName})
    };
    data.designDocs = new Documents.AllDocs(null, {
      database: data.database,
      params: {startkey: '"_design"',
        endkey: '"_design1"',
        include_docs: true}
    });

    return {
      layout: "with_tabs_sidebar",

      data: data,

      crumbs: [
        {"name": "Databases", "link": "/_all_dbs"},
        {"name": data.database.id, "link": data.database.url('app')}
      ],

      views: {
        "#sidebar-content": new Documents.Views.Sidebar({
          collection: data.designDocs
        }),

        "#tabs": new Documents.Views.Tabs({
          collection: data.designDocs,
          database: data.database
        }),

        "#dashboard-content": new Documents.Views.ViewEditor({
          model: data.database,
          ddocs: data.designDocs
        })
      },

      apiUrl: data.database.url()
    };
  };

  // HACK: this kind of works
  // Basically need a way to share state between different routes, for
  // instance making a new doc won't work for switching back and forth
  // between code and field editors
  var newDocCodeEditorCallback = function(databaseName) {
    var data = {
      database: new Databases.Model({id:databaseName}),
      doc: new Documents.NewDoc(),
      selected: "code_editor"
    };
    data.doc.database = data.database;
    data.designDocs = new Documents.AllDocs(null, {
      database: data.database,
      params: {startkey: '"_design"',
        endkey: '"_design1"',
        include_docs: true}
    });

    var options = app.getParams();
    options.include_docs = true;
    data.database.buildAllDocs(options);

    return {
      layout: "one_pane",

      data: data,

      crumbs: [
        {"name": "Databases", "link": "/_all_dbs"},
        {"name": data.database.id, "link": Databases.databaseUrl(data.database)},
        {"name": "new", "link": "#"}
      ],

      views: {
        "#dashboard-content": new Documents.Views.Doc({
          model: data.doc
        }),

        "#tabs": new Documents.Views.FieldEditorTabs({
          selected: data.selected,
          model: data.doc
        })
      },

      apiUrl: data.doc.url()
    };
  };


  var DocumentsRouteObject = FauxtonAPI.RouteObject.extend({
    layout: "with_tabs_sidebar",

    events: {
      "route:all_docs": "allDocs",
      "route:all_design_docs": "allDesignDocs",
      "route:view_fn": "viewFn",
      "route:new_view": "newViewEditor"
    },

    defaultRoute: "allDocs",

    initialize: function (options) {
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
        collection: this.data.designDocs
      }));

      this.setView("#tabs", new Documents.Views.Tabs({
        collection: this.data.designDocs,
        database: this.data.database
      }));
    },

    crumbs: function () {
      return [
        {"name": "Databases", "link": "/_all_dbs"},
        {"name": this.data.database.id, "link": Databases.databaseUrl(this.data.database)}
      ];
    },

    allDocs: function(event) {
      var docOptions;

      docOptions = app.getParams(event.attr);
      docOptions.include_docs = true;

      this.data.database.buildAllDocs(docOptions);
      this.sidebar.setSelectedTab('all-docs');

      this.documentsView = this.setView("#dashboard-content", new Documents.Views.AllDocsList({
        collection: this.data.database.allDocs
      }));
    },

    allDesignDocs: function(event) {
      var docOptions = app.getParams(event.attr);
      docOptions.include_docs = true;

      this.data.database.buildAllDocs(docOptions);
      this.sidebar.setSelectedTab('design-docs');

      this.documentsView = this.setView("#dashboard-content", new Documents.Views.AllDocsList({
        collection: this.data.database.allDocs
      }));
    },

    route: function (route, routeArgs) {
      console.log('ROUTE ARG ', arguments);

      if (route === 'database/:database/_design/:ddoc/_view/:view') {

        if (!this.docCrumbs && !this.docApiUrl) {
          // Save the old crumbs and API. Easier to do it here than in each event
          this.docCrumbs = this.crumbs;
          this.docApiUrl = this.apiUrl;
        }

        this.routeArgs = {
          designDoc: routeArgs[1],
          view: routeArgs[2].replace(/\?.*$/,'')
        };
      } else {
        this.routeArgs = {};
        if (this.docCrumbs && this.docApiUrl) {
          this.crumbs = this.docCrumbs;
          this.docApiUrl = this.apiUrl;
        }
      }
    },

    viewFn: function (event) {
      var view = this.routeArgs.view,
      ddoc = this.routeArgs.designDoc,
      params = app.getParams(event.attr);

      console.log('PARAMS', params);

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

      this.setView("#dashboard-content", new Documents.Views.AllDocsList({
        collection: this.data.indexedDocs,
        nestedView: Documents.Views.Row,
        viewList: true,
        ddocInfo: ddocInfo,
        params: params
      }));

      this.crumbs = function () {
        return [
          {"name": "Databases", "link": "/_all_dbs"},
          {"name": this.data.database.id, "link": Databases.databaseUrl(this.data.database)},
          {"name": ddoc + "/" + view, "link": this.data.indexedDocs.url()}
        ];
      };

      // TODO: change to view URL
      this.apiUrl = this.data.indexedDocs.url();
    },

    newViewEditor: function (event) {
      // TODO: Get this working
      this.setView("#dashboard-content", new Documents.Views.ViewEditor({
        model: this.data.database,
        ddocs: this.data.designDocs
      }));

    },

    routes: ["database/:database/_all_docs(:extra)", "database/:database/_design/:ddoc/_view/:view", "database/:database/new_view"],

    apiUrl: function() {
      return this.data.database.allDocs.url();
    }
  });



  var ChangesRouteObject = FauxtonAPI.RouteObject.extend({
    layout: "with_tabs",

    crumbs: function () {
      return [
        {"name": "Databases", "link": "/_all_dbs"},
        {"name": this.database.id, "link": Databases.databaseUrl(this.database)},
        {"name": "_changes", "link": "/_changes"}
      ];
    },

    routes: ["database/:database/_changes(:params)"],

    events: {
      "route:_changes": "changes"
    },

    defaultRoute: "changes",

    initialize: function (options) {
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

  /* Documents.Routes = {
     "database/:database/_design%2F:doc": function(database, doc) {
     var docID = "_design/"+doc;
     return codeEditorCallback(database, docID);
     },



     "database/:database/new": newDocCodeEditorCallback,
     "database/:database/new_view": newViewEditorCallback,

  // TODO: fix optional search params
  // Can't get ":view(?*search)" to work
  // However ":view?*search" does work
  //"database/:database/_design/:ddoc/_view/:view(\?*options)": function(databaseName, ddoc, view, options) {
  "database/:database/_design/:ddoc/_view/:view": function(databaseName, ddoc, view, options) {
// hack around backbone router limitations
view = view.replace(/\?.*$/,'');
var params = app.getParams();
var data = {
database: new Databases.Model({id:databaseName})
};

data.indexedDocs = new Documents.IndexCollection(null, {
database: data.database,
design: ddoc,
view: view,
params: params
});

data.designDocs = new Documents.AllDocs(null, {
database: data.database,
params: {startkey: '"_design"',
endkey: '"_design1"',
include_docs: true}
});

var ddocInfo = {
id: "_design/" + ddoc,
currView: view,
designDocs: data.designDocs
};

return {
layout: "with_tabs_sidebar",

data: data,
  // TODO: change dashboard-content
views: {
"#dashboard-content": new Documents.Views.AllDocsList({
collection: data.indexedDocs,
nestedView: Documents.Views.Row,
viewList: true,
ddocInfo: ddocInfo,
params: params
}),

"#sidebar-content": new Documents.Views.Sidebar({
collection: data.designDocs,
ddocInfo: ddocInfo
}),

"#tabs": new Documents.Views.Tabs({
collection: data.designDocs,
database: data.database
})
},

crumbs: [
{"name": "Databases", "link": "/_all_dbs"},
{"name": data.database.id, "link": Databases.databaseUrl(data.database)},
{"name": ddoc + "/" + view, "link": data.indexedDocs.url()}
],
  // TODO: change to view URL
  apiUrl: data.indexedDocs.url()
  };
}
};*/

  Documents.RouteObjects = [DocEditorRouteObject, DocumentsRouteObject, ChangesRouteObject];

  return Documents;
});
